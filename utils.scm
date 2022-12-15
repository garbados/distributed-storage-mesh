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

(define* (cbify proc #:optional (vat (spawn-vat)))
  (define return-channel (make-channel))
  (define-vat-run run vat)
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

;; receives and checks for agreement between the first n results
(define (await-n vows n)
  (define-values (promises results errors) (await* vows))
  (define i 0)
  (for-each
   (lambda (promise)
     (when (< i n)
       (<<- promise)
       (set! i (1+ i))))
   promises)
  (define final-result
    (fold
     (lambda (result last-result)
       (if (eq? result last-result)
           result
           (error 'conflict)))
     ($ results)))
  (if final-result
      final-result
      (error 'failed ($ errors))))
