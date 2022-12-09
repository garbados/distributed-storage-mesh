;; run as: $ guix shell -m manifest.scm -- guile dsm.scm

(define-module (dsm)
  #:export (spawn-block-provider
            ;spawn-proxy-block-provider
            spawn-sqlite-block-provider
            spawn-memory-block-provider
            ^content-provider
            spawn-async-content-provider
            spawn-sync-content-provider)
  #:use-module ((goblins)
                #:select (spawn
                          spawn-vat
                          define-vat-run
                          $
                          <-
                          on))
  #:use-module ((goblins actor-lib cell)
                #:select (define-cell))
  #:use-module ((goblins actor-lib methods)
                #:select (methods))
  #:use-module ((goblins utils simple-sealers)
                #:select (make-sealer-triplet))
  #:use-module ((goblins contrib syrup)
                #:select (syrup-encode
                          syrup-decode))
  #:use-module ((ice-9 match)
                #:select (match))
  #:use-module ((ice-9 control)
                #:select (call-with-prompt abort-to-prompt))
  #:use-module ((srfi srfi-11)
                #:select (let-values))
  #:use-module ((gcrypt random)
                #:select (gen-random-bv
                          %gcry-very-strong-random))
  #:use-module ((eris)
                #:select (eris-encode
                          eris-decode))
  #:use-module ((sqlite3)
                #:select (sqlite-open
                          sqlite-bind
                          SQLITE_OPEN_CREATE
                          SQLITE_OPEN_READWRITE
                          sqlite-exec
                          sqlite-map
                          sqlite-prepare
                          sqlite-finalize)))

;; chunking requires a convergence secret.
;; by default we'll use a strong random.

(define (random-secret) (gen-random-bv 32 %gcry-very-strong-random))

;; (goblins actor-lib joiners) doesn't export code, for some reason
;; so we do it ourselves

(use-modules ((goblins)
              #:select (<-np
                        spawn-promise-values)))

(define (all-of* promises)
  (define-cell waiting
    promises)
  (define-cell results
    '())
  (define-cell broken?
    #f)

  (define-values (join-promise join-resolver)
    (spawn-promise-values))

  (define (results->list results-alist)
    (map
     (lambda (promise)
       (cdr (assq promise results-alist)))
     promises))

  (define (resolve-promise promise)
    (on promise
        (lambda (result)
          (unless ($ broken?)
            (let ((new-results (acons promise result ($ results)))
                  (new-waiting (delq promise ($ waiting))))
              (if (null? new-waiting)
                  (<-np join-resolver 'fulfill
                        (results->list new-results))
                  (begin ($ waiting new-waiting)
                         ($ results new-results))))))
        #:catch
        (lambda (err)
          (unless ($ broken?)
            ($ broken? #t)
            (<-np join-resolver 'break err)))))

  (map resolve-promise promises)
  join-promise)

;; return synchronously using a callback

(define (cbify vow)
  (call-with-prompt 'cbify
    (lambda () (abort-to-prompt 'cbify))
    (lambda (cb) (on vow cb))))

;; BLOCK PROVIDER

(define (spawn-block-provider save-block
                              read-block
                              delete-block
                              create-lease
                              count-leases)
  (define (make-lease ref)
    (define release (create-lease ref))
    (define-cell released?)
    (lambda ()
      (unless ($ released?)
        (release)
        ($ released? #t))
      (when (eq? 0 (count-leases ref))
        (delete-block ref))))
  (define-cell revoked? #f)
  (define (^block-provider _bcom)
    (methods
     ((save-block ref block)
      (when ($ revoked?) (error 'revoked))
      (save-block ref block)
      (make-lease ref))
     ((read-block ref)
      (when ($ revoked?) (error 'revoked))
      (read-block ref))))
  (values
   (spawn ^block-provider)
   revoked?))

;; CONTENT PROVIDERS

(define (^content-provider _bcom save-block read-block process-results)
  (methods
   ((save-content content
                  #:optional
                  (key (random-secret))
                  (block-size 'small))
    (define-cell release-cell)
    (define reducer
      (case-lambda
        (() '())
        ((results)
         (define releases (process-results results))
         ($ release-cell releases))
        ((results ref-block)
         (define ref (car ref-block))
         (define block (cdr ref-block))
         (define result (save-block ref block))
         (cons result results))))
    (define (release-content)
      (define releases ($ release-cell))
      (unless (null? releases)
        ;; run each block release
        (for-each (lambda (release) (release)) releases)
        ;; clear the releases cell so this becomes a noop
        ($ release-cell '())))
    (define readcap (eris-encode (syrup-encode content)
                                 #:block-size block-size
                                 #:block-reducer reducer
                                 #:convergence-secret key))
    (list
     readcap
     release-content))
   ((read-content readcap)
    (syrup-decode (eris-decode readcap
                               #:block-ref read-block)))))

;; for when your block provider is on another machine
(define (spawn-async-content-provider block-provider)
  (define (save-block ref block)
    (<- block-provider 'save-block ref block))
  (define (read-block ref)
    (cbify (<- block-provider 'read-block ref)))
  (define (process-results results)
    (cbify (all-of* results)))
  (spawn ^content-provider save-block read-block process-results))

;; for when your block provider is on your machine
(define (spawn-sync-content-provider block-provider)
  (define (save-block ref block)
    ($ block-provider 'save-block ref block))
  (define (read-block ref)
    ($ block-provider 'read-block ref))
  (define (process-results results)
    results)
  (spawn ^content-provider save-block read-block process-results))

;; HASHMAP (in-memory) BACKEND FOR BLOCK STORAGE

(define (init-hash-ops)
  (define blocks (make-hash-table))
  (define (save-block ref block) (hash-set! blocks ref block))
  (define (read-block ref) (hash-ref blocks ref))
  (define (delete-block ref) (hash-remove! blocks ref))

  (define leases (make-hash-table))
  (define (inc-ref ref) (hash-set! leases ref (1+ (hash-ref leases ref 0))))
  (define (dec-ref ref) (hash-set! leases ref (1- (hash-ref leases ref))))
  (define (count-leases ref) (hash-ref leases ref))
  (define (create-lease ref) (inc-ref ref) (lambda () (dec-ref ref)))

  (values save-block read-block delete-block create-lease count-leases))

(define (spawn-memory-block-provider)
  (call-with-values init-hash-ops spawn-block-provider))

;; SQLITE BACKEND FOR BLOCK STORAGE

(define (db-exec stmt)
  (let ((result (sqlite-map identity stmt)))
    (sqlite-finalize stmt)
    result))

(define (init-db db-name)
  (sqlite-open db-name (logior SQLITE_OPEN_CREATE
                               SQLITE_OPEN_READWRITE)))

(define (clear-table db table)
  (sqlite-exec db (string-append "drop table if exists " table)))

(define* (init-simple-table db table defs)
  (define rowdefs (string-join defs ", "))
  (sqlite-exec db (string-append "create table " table " ( " rowdefs  " )")))

(define lease-defs '("ref blob primary key"
                     "count integer default 0 not null"))

(define block-defs '("ref blob primary key"
                     "block blob not null"))

(define (init-sqlite-ops db-name clear?)
  (define db (init-db db-name))
  (when clear?
    (clear-table db "leases")
    (clear-table db "blocks"))
  (init-simple-table db "leases" lease-defs)
  (init-simple-table db "blocks" block-defs)
  (define (save-block ref block)
    (let* ((query "insert into blocks values (?,?);")
           (stmt (sqlite-prepare db query)))
      (sqlite-bind stmt 1 ref)
      (sqlite-bind stmt 2 block)
      (db-exec stmt)
      #t))
  (define (read-block ref)
    (let* ((query "select block from blocks where ref = ?;")
           (stmt (sqlite-prepare db query)))
      (sqlite-bind stmt 1 ref)
      (let ((result (db-exec stmt)))
        (vector-ref (car result) 0))))
  (define (delete-block ref)
    (let* ((query "delete from blocks where ref = ?;")
           (stmt (sqlite-prepare db query)))
      (sqlite-bind stmt 1 ref)
      (db-exec stmt)
      #t))
  (define (create-lease ref)
    (let* ((query "insert into leases values (?,0);")
           (stmt (sqlite-prepare db query)))
      (sqlite-bind stmt 1 ref)
      (db-exec stmt)
      #t))
  (define (count-leases ref)
    (let* ((query "select count from leases where ref = ?;")
           (stmt (sqlite-prepare db query)))
      (sqlite-bind stmt 1 ref)
      (let ((result (db-exec stmt)))
        (vector-ref (car result) 0))))
  (values save-block read-block delete-block create-lease count-leases))

(define* (spawn-sqlite-block-provider db-path #:optional clear?)
  (call-with-values (lambda () (init-sqlite-ops db-path clear?))
    spawn-block-provider))

;; PROXY BLOCK PROVIDER

(define* (spawn-proxy-block-provider add-provider
                                     remove-provider
                                     map-providers
                                     #:optional
                                     (read-n 3)
                                     (save-n 3))
  (define providers (make-hash-map))
  (map-providers (lambda (sturdyref provider)
                   (hash-set! providers sturdyref provider)))
  (methods
   ((read-block ref)
    'TODO)
   ((save-block ref block)
    'TODO)
   ((add-provider sturdyref)
    'TODO)
   ((remove-provider sturdyref)
    'TODO)
   ((map-providers fn)
    'TODO)))
