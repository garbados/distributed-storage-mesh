;; TEST SETUP

(use-modules ((goblins)
              #:select (spawn-vat
                        define-vat-run
                        $))
             ((dsm)
              #:select (spawn-memory-block-provider
                        spawn-sqlite-block-provider
                        spawn-async-content-provider
                        spawn-sync-content-provider)))

(define content "hello world")

;; goblin vats -- event loops on a run (like a virtual machine)
(define a-vat (spawn-vat))
(define-vat-run a-run a-vat)
(define b-vat (spawn-vat))
(define-vat-run b-run b-vat)

(define backends
  (list
   (list "memory" spawn-memory-block-provider)
   (list "sqlite" (lambda ()
                    (spawn-sqlite-block-provider "scratch.db" #t)))))

;; content-provider save-then-read test
(define (content-test test-name content-provider)
  (define save-result ($ content-provider 'save-content content))
  (define readcap (car save-result))
  (define result ($ content-provider 'read-content readcap))
  (pk 'test test-name 'result result 'pass (string=? content result)))

(define (sync-tests)
  (for-each
   (lambda (args)
     (define backend (car args))
     (define block-provider-spawner (car (cdr args)))
     (define block-provider (a-run (block-provider-spawner)))
     (define test-name (string-append backend "-sync"))
     (define content-provider
       (a-run (spawn-sync-content-provider block-provider)))
     (a-run (content-test test-name content-provider)))
   backends))

(define (async-tests)
  (for-each
   (lambda (args)
     (define backend (car args))
     (define block-provider-spawner (car (cdr args)))
     (define block-provider (a-run (block-provider-spawner)))
     (define test-name (string-append backend "-async"))
     (define content-provider
       (b-run (spawn-async-content-provider block-provider)))
     (b-run (content-test test-name content-provider)))
   backends))

(for-each
 (lambda (tests) (tests))
 (list sync-tests #|async-tests|#))
