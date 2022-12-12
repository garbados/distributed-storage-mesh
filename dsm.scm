;; run as: $ guix shell -m manifest.scm -- guile dsm.scm

(define-module (dsm)
  #:export (spawn-block-provider
            ;spawn-proxy-block-provider
            spawn-sqlite-block-provider
            spawn-memory-block-provider
            ^content-provider
            spawn-async-content-provider
            spawn-sync-content-provider
            cbify)
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
                          eris-decode
                          %null-convergence-secret))
  #:use-module ((sqlite3)
                #:select (sqlite-open
                          sqlite-bind
                          SQLITE_OPEN_CREATE
                          SQLITE_OPEN_READWRITE
                          sqlite-exec
                          sqlite-map
                          sqlite-prepare
                          sqlite-finalize
                          sqlite-close))
  #:use-module ((fibers channels)
                #:select (make-channel get-message put-message)))

;; chunking requires a convergence secret.
;; by default we'll use a strong random.

(define (random-secret) (gen-random-bv 32 %gcry-very-strong-random))

;; return synchronously using a callback

(define (cbify proc)
  (define return-channel (make-channel))
  (pk 'cbify-begin return-channel)
  (define-vat-run run (spawn-vat))
  (run (pk 'cbify-inner)
       (on (proc)
           (lambda (. args)
             (pk 'cbify-args args)
             (put-message return-channel (list 'ok args))
             'done)
           #:catch
           (lambda (err)
             (put-message return-channel (list 'err err))
             'err)))
  (define result (get-message return-channel))
  (pk 'cbify-result result)
  (define status (car result))
  (define args (car (cdr result)))
  (if (eq? status 'ok)
      (apply values (car args) (cdr args))
      (error value)))

;; BLOCK PROVIDER

(define* (spawn-block-provider save-block
                               read-block
                               delete-block
                               create-lease
                               count-leases)
  (define (make-lease ref)
    (define release (create-lease ref))
    (define released? #f)
    (lambda ()
      (unless released?
        (release)
        (set! released? #t))
      (when (eq? 0 (count-leases ref))
        (delete-block ref))))
  (define (^block-provider _bcom)
    (methods
     ((save-block ref block)
      (save-block ref block)
      (make-lease ref))
     ((read-block ref)
      (read-block ref))))
  (spawn ^block-provider))

;; CONTENT PROVIDERS

(define (^content-provider _bcom save-block read-block)
  (define (my-save-content content key block-size)
    (define final-releases '())
    (define reducer
      (case-lambda
        (() '())
        ((releases)
         (set! final-releases releases))
        ((releases ref-block)
         (define ref (car ref-block))
         (define block (cdr ref-block))
         (define release (save-block ref block))
         (cons release releases))))
    (define (release-content)
      (unless (null? final-releases)
        ;; run each block release
        (for-each (lambda (release) (release)) final-releases)
        ;; clear the releases cell so this becomes a noop
        (set! final-release '())))
    (define readcap (eris-encode (syrup-encode content)
                                 #:block-size block-size
                                 #:block-reducer reducer
                                 #:convergence-secret key))
    `(,readcap ,release-content))
  (methods
   ((save-content content
                  #:optional
                  (key (random-secret))
                  (block-size 'small))
    (my-save-content content key block-size))
   ((save-content-public content
                         #:optional
                         (block-size 'small))
    (my-save-content content %null-convergence-secret block-size))
   ((read-content readcap)
    (syrup-decode (eris-decode readcap
                               #:block-ref read-block)))))

;; for when your block provider is on another machine
(define (spawn-async-content-provider block-provider)
  (define (save-block ref block)
    (pk 'async-save-result
        (cbify (lambda ()
                 (pk 'async-save ref)
                 (<- block-provider 'save-block ref block)))))
  (define (read-block ref)
    (pk 'async-read-result
        (cbify (lambda ()
                 (pk 'async-read ref)
                 (<- block-provider 'read-block ref)))))
  (spawn ^content-provider save-block read-block))

;; for when your block provider is on your machine
(define (spawn-sync-content-provider block-provider)
  (define (save-block ref block)
    ($ block-provider 'save-block ref block))
  (define (read-block ref)
    ($ block-provider 'read-block ref))
  (spawn ^content-provider save-block read-block))

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
  (define init? (file-exists? db-name))
  (define db (init-db db-name))
  (when clear?
    (clear-table db "leases")
    (clear-table db "blocks"))
  (when (or clear? init?)
    (init-simple-table db "leases" lease-defs)
    (init-simple-table db "blocks" block-defs))
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

#|(define* (spawn-proxy-block-provider vat
                                     add-provider
                                     remove-provider
                                     map-providers
                                     #:optional
                                     (read-n 3)
                                     (save-n 3))
  (define providers (make-hash-table))
  ;; internal methods for managing providers
  (define (count-providers)
    (hash-fold (lambda (_ __ i) (+ 1 i)) 0 providers))
  (define (list-providers)
    (hash-map->list (lambda (_ provider) provider) providers))
  ;; init providers using given mapper
  (map-providers (lambda (ref provider)
                   (hash-set! providers ref provider)))
  (define-cell revoked?)
  (define (^proxy-block-provider _bcom)
    (methods
     ((read-block ref)
      (when ($ revoked?) (error 'revoked))
      ;; setup vars
      (define providers-count (count-providers))
      (define n (if (> read-n providers-count)
                    read-n
                    providers-count))
      (define-cell queue (list-providers))
      ;; collate results and errors
      (define-cell errors '())
      (define-cell results '())
      ;; error handler
      (define (handle-err err)
        ($ errors (cons err ($ errors)))
        (maybe-spawn-read))
      ;; each read runs in a separate promise
      (define (spawn-read provider)
        (define result
          (cbify vat (lambda () (<- provider 'read-block ref))
                 handle-err))
        (when result
          ($ results (cons result ($ results)))))
      (define (maybe-spawn-read)
        (define provider (car ($ queue)))
        ;; fizzle if we already have a result
        (when (null? ($ results))
          ;; since we know we'll use this provider, dequeue it
          ($ queue (cdr ($ queue)))
          (spawn-read provider)))
      ;; spawn reads from the queue up to `n'
      (for-each (lambda (_) (maybe-spawn-read)) (make-list n))
      ;; finally...
      (define final-results ($ results))
      (if (null? final-results)
          (error 'failed ($ errors))
          final-results))
     ((save-block ref block)
      (when ($ revoked?) (error 'revoked))
      (define-cell errors '())
      (define-cell results '())
      (define (handle-err err)
        ($ errors (cons err ($ errors)))
        #f)
      (define (spawn-write provider)
        (define result
          (cbify (<- provider 'save-block ref block)
                 handle-err))
        (when result
          ($ results (cons result ($ results)))))
      (map spawn-write (list-providers))
      ;; collate return values
      (define releases ($ results))
      (if (< save-n (length releases))
          (error 'failed ($ errors))
          (lambda ()
            ;; combine release functions
            (for-each (lambda (release) (release)) releases)
            )))
     ((add-provider ref provider)
      (hash-set! providers ref provider)
      (add-provider ref))
     ((remove-provider ref)
      (remove-provider ref))
     ((map-providers fn)
      (map-providers fn))))
  (values
   (spawn ^proxy-block-provider)
   revoked?))|#
