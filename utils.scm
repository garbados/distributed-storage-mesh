(define-module (dsm utils)
  #:export (cbify await await-all await-one)
  #:use-module ((goblins)
                #:select (define-vat-run
                           spawn-vat
                           on
                           $
                           <<-))
  #:use-module ((goblins actor-lib cell)
                #:select (define-cell))
  #:use-module ((fibers channels)
                #:select (make-channel get-message put-message)))

(define (cbify proc)
  (define return-channel (make-channel))
  (define-vat-run run (spawn-vat))
  (run (on (proc)
           (lambda (. args)
             (put-message return-channel (list 'ok args))
             'done)
           #:catch
           (lambda (err)
             (put-message return-channel (list 'err err))
             'err)))
  (define result (get-message return-channel))
  (define status (car result))
  (define args (car (cdr result)))
  (if (eq? status 'ok)
      (apply values (car args) (cdr args))
      (apply error (car args) (cdr args))))

(define-syntax await
  (syntax-rules ()
    ((_ exp ...)
     (cbify (lambda () (begin exp ...))))))

;; general func for working with in-progress vows
(define (await* vows)
  (define-cell errors '())
  (define-cell results '())
  (define promises
    (map
     (lambda (vow)
       (on vow
           (lambda (. args)
             ($ results (cons args ($ results))))
           #:catch
           (lambda (err)
             ($ errors (cons err ($ errors))))
           #:promise? #t))
     vows))
  (values promises results errors))

;; return all results and failures
(define (await-all vows)
  (define-values (promises results errors) (await* vows))
  (for-each <<- promises)
  (values ($ results) ($ errors)))

;; return the first result (or all failures)
(define (await-one vows)
  (define-values (promises results errors) (await* vows))
  (for-each
   (lambda (promise)
     (unless (null? ($ result))
       (<<- promise)))
   promises)
  (define final-result ($ result))
  (if (null? final-result)
      (error 'failed ($ errors))
      (car final-result)))
