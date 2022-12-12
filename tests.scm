;; TEST SETUP

(use-modules ((goblins)
              #:select (spawn
                        spawn-vat
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
              #:select (test-end
                        test-begin
                        test-assert)))

;;(define-syntax await
;;  (syntax-rules ()
;;    [(_ vat . exprs)
;;     (cbify vat (lambda () exprs))]))

;; goblin vats (like a thread)
(define a-vat (spawn-vat))
(define b-vat (spawn-vat))
;; vat runs -- macro like (vat (lambda () exprs ...))
(define-vat-run a-run a-vat)
(define-vat-run b-run b-vat)

;; randomly generate a potentially long string
(define (random-string)
  (define len (random 1000))
  (define max-char 55296)
  (define (random-char) (integer->char (random max-char)))
  (list->string
   (map (lambda (_) (random-char)) (make-list len))))

;; our handy dandy block provider. in-memory only
(define block-provider (a-run (spawn-memory-block-provider)))

(test-begin "sync content provider test")
(test-begin "sync content provider can read its own writes")
(define sync-content-provider
  (a-run (spawn-sync-content-provider block-provider)))
(for-each
 (lambda (_)
   (define content (random-string))
   (define readcap
     (car (a-run ($ sync-content-provider 'save-content content))))
   (define result
     (a-run ($ sync-content-provider 'read-content readcap)))
   (test-assert "content == result" (string=? content result)))
 (make-list 100))
(test-end)
(test-begin "sync content provider produces stable URNs")
(begin
  (define content (random-string))
  (define readcap1
    (car (a-run ($ sync-content-provider 'save-content-public content))))
  (define readcap2
    (car (a-run ($ sync-content-provider 'save-content-public content))))
  (test-assert "readcap1 == readcap2" (string=? readcap1 readcap2)))
(test-end)
(test-end)

(test-begin "isolated proof of async error")
(use-modules ((eris)
              #:select (eris-encode eris-decode %null-convergence-secret))
             ((goblins contrib syrup)
              #:select (syrup-encode syrup-decode)))
(begin
  (define blocks (make-hash-table))
  (define (save-block ref block) (hash-set! blocks ref block))
  (define (read-block ref) (hash-ref blocks ref))
  (define (^encoder _bcom)
    (define reducer
      (case-lambda
        (() '())
        ((_) 'done)
        ((_ ref-block)
         (save-block (car ref-block) (cdr ref-block)))))
    (lambda (content)
      (eris-encode (syrup-encode content)
                   #:block-size 'small
                   #:block-reducer reducer
                   #:convergence-secret %null-convergence-secret)))
  (define (^decoder _bcom)
    (lambda (readcap)
      (syrup-decode (eris-decode readcap
                                 #:block-ref read-block))))
  (define encode (a-run (spawn ^encoder)))
  (define decode (b-run (spawn ^decoder)))
  (define content "hello world")
  (define readcap (pk (a-run ($ encode content))))
  (define result (pk (b-run ($ decode readcap))))
  (test-assert "content == result" (string=? content result))
  )
(test-end "isolated proof of async error")

(test-begin "async content provider test")
(define async-content-provider
  (b-run (spawn-async-content-provider block-provider)))
(define content (random-string))
(define readcap (car (b-run ($ async-content-provider 'save-content content))))
(define result (b-run ($ async-content-provider 'read-content readcap)))
(test-assert ("content == result" (string=? content result)))
(test-end "async content provider test")
