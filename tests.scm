;; TEST SETUP

(use-modules ((goblins)
              #:select (spawn-vat
                        define-vat-run
                        $
                        <-))
             ((dsm)
              #:select (spawn-block-provider
                        ;spawn-proxy-block-provider
                        spawn-memory-block-provider
                        spawn-sqlite-block-provider
                        spawn-async-content-provider
                        spawn-sync-content-provider
                        cbify))
             ((srfi srfi-64)
              #:select (test-group-with-cleanup
                        test-group
                        test-assert)))

(define-syntax await
  (syntax-rules ()
    [(_ vat . exprs)
     (cbify vat (lambda () exprs))]))

;; goblin vats -- event loops on a run (like a virtual machine)
(define a-vat (spawn-vat))
(define b-vat (spawn-vat))
(define-vat-run a-run a-vat)
(define-vat-run b-run b-vat)

(define (random-string)
  (define len (random 10000000))
  (define max-char 55296)
  (define (random-char) (integer->char (random max-char)))
  (define (gen-chars chars len)
    (if (eq? len 0)
        chars
        (cons (random-char) chars)))
  (pk 'content-length len)
  (list->string (gen-chars '() len)))

(define block-provider (a-run (spawn-memory-block-provider)))
(define content-provider
  (b-run (spawn-async-content-provider b-vat block-provider)))
(define (test-content vat content)
  (define readcap
    (cbify b-vat (lambda () (<- content-provider 'save-content content))))
  (define result
    (cbify b-vat (lambda () (<- content-provider 'read-content readcap))))
  (pk 'test-content content 'pass (eq? content result)))

(define (run-tests proc i n)
  (unless (eq? i n)
    (pk 'test (1+ i))
    (proc)
    (run-tests proc (1- i) n)
    ))

(run-tests (lambda () (test-content b-vat (random-string)))0 5)
