(use-modules ((goblins)
              #:select (spawn
                        spawn-vat
                        define-vat-run
                        $
                        <-
                        on))
             ((goblins actor-lib cell)
              #:select (^cell))
             ((fibers)
              #:select (run-fibers spawn-fiber))
             ((fibers channels)
              #:select (make-channel get-message put-message)))

(define a-vat (spawn-vat))
(define b-vat (spawn-vat))
(define-vat-run a-run a-vat)
(define-vat-run b-run b-vat)
(define a-cell (a-run (spawn ^cell)))

;; sync works
(pk 'sync-set (a-run ($ a-cell 'town)))
;;> #<unspecified>
(pk 'sync-get (a-run ($ a-cell)))
;;> 'town

;; async does not
(pk 'async-set
    (b-run (on (<- a-cell 'world)
               (lambda (result) (pk 'async-set-result result))
               ;; only runs sometimes
               ;;> #<unspecified>
               #:catch
               (lambda (err) (pk 'err err)))))
;;> #<unspecified>
(pk 'async-get
    (b-run (on (<- a-cell)
               (lambda (result) (pk 'async-get-result result))
               ;; only runs sometimes
               ;;> 'world
               #:catch
               (lambda (err) (pk 'err err)))))
;;> #<unspecified>

;; but all our callbacks fire if we sleep
(pk 'slept)
(usleep 100)

;; so... how do i wait for `(on ...)' to return
;; if synchronous return is absolutely required?

;; this is an effort to produce an `await'-style async-to-sync
(define (cbify vat proc)
  (define return-channel (make-channel))
  (vat (lambda () (on (proc)
                      (lambda (. args)
                        (put-message return-channel (list 'ok args))
                        'done)
                      #:catch
                      (lambda (err)
                        (put-message return-channel (list 'err err))
                        'err))))
  (define result (get-message return-channel))
  (define status (car result))
  (define args (car (cdr result)))
  (if (eq? status 'ok)
      (apply values (car args) (cdr args))
      (error value)))

(pk 'cbify (cbify b-vat (lambda () (<- a-cell))))
(pk 'cbify (cbify b-vat (lambda () (<- a-cell))))

;; now let's apply it to the dsm

(use-modules ((dsm)
              #:select (spawn-memory-block-provider
                        spawn-async-content-provider)))
(define block-provider (a-run (spawn-memory-block-provider)))
(define (save-block ref block)
  (pk 'save-block ref)
  (pk 'cbify-save
      (cbify b-vat (lambda () (<- block-provider 'save-block ref block)))))
(define (read-block ref)
  (pk 'read-block ref)
  (cbify b-vat (lambda () (<- block-provider 'read-block ref))))
(define (process-results results) results)
(define content-provider (b-run (spawn-async-content-provider block-provider)))

;; this WORKS
(define readcap (car (b-run ($ content-provider 'save-content "hello world"))))
(pk 'readcap readcap)
(define content (b-run ($ content-provider 'read-content readcap)))
(pk 'content content)

;; maybe i need to change some fundamental assumptions...

;; later... zzz...
