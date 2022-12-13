;; TEST SETUP

(use-modules ((goblins)
              #:select (spawn
                        spawn-vat
                        define-vat-run
                        $
                        <-))
             ((goblins actor-lib methods)
              #:select (methods))
             ((dsm)
              #:select (^encoder
                        ^decoder
                        ^block-writer
                        ^block-reader
                        ^content-writer
                        ^content-reader
                        ^proxy-block-writer
                        ^proxy-block-reader))
             ((dsm utils)
              #:select (await))
             ((srfi srfi-64)
              #:select (test-end
                        test-begin
                        test-error
                        test-assert)))

;; test-group doesn't seem to work as advertised
(define-syntax test-group
  (syntax-rules ()
    ((_ suite-name exp ...)
     (begin
       (test-begin suite-name)
       (begin exp ...)
       (test-end suite-name)))))

;; goblin vats (like a thread)
(define a-vat (spawn-vat))
(define b-vat (spawn-vat))
;; vat runs -- macro expands to (vat (lambda () exprs ...))
(define-vat-run a-run a-vat)
(define-vat-run b-run b-vat)

;; randomly generate a potentially long string
(define (random-string)
  (define len (random 1000))
  (define max-char 55296)
  (define (random-char) (integer->char (random max-char)))
  (list->string
   (map (lambda (_) (random-char)) (make-list len))))

(test-group "logs/encoder_decoder_model_works"
  (for-each
   (lambda (_)
     (define blocks (make-hash-table))
     (define (save-block ref block) (hash-set! blocks ref block))
     (define (read-block ref) (hash-ref blocks ref))
     (define encode (a-run (spawn ^encoder save-block)))
     (define decode (b-run (spawn ^decoder read-block)))
     (define content (random-string))
     (define readcap (a-run ($ encode content)))
     (define result (a-run (await (<- decode readcap))))
     (test-assert "content == result" (string=? content result)))
   (make-list 100)))

(test-group "logs/block_content_provider_works"
  (define content (random-string))
  (define (^memory-block-provider _bcom)
    (define blocks (make-hash-table))
    (methods
     ((save-block ref block)
      (hash-set! blocks ref block))
     ((read-block ref)
      (hash-ref blocks ref))))
  (define (^content-provider _bcom save-block read-block)
    (define encode (spawn ^encoder save-block))
    (define decode (spawn ^decoder read-block))
    (methods
     ((save-content content)
      ($ encode content))
     ((read-content readcap)
      ($ decode readcap))))
  (define block-provider (a-run (spawn ^memory-block-provider)))
  (define content-provider
    (b-run
     (spawn ^content-provider
            (lambda (ref block)
              (await (<- block-provider 'save-block ref block)))
            (lambda (ref)
              (await (<- block-provider 'read-block ref))))))
  (define readcap (b-run ($ content-provider 'save-content content)))
  (test-assert "content == result" (string=? content result)))

(test-group "logs/remote-save-read-works"
  (define blocks (make-hash-table))
  (define (^save-block _bcom)
    (lambda (ref block) (hash-set! blocks ref block)))
  (define (^read-block _bcom)
    (lambda (ref) (hash-ref blocks ref)))
  (define save-block (a-run (spawn ^save-block)))
  (define read-block (a-run (spawn ^read-block)))
  (define encode (b-run (spawn ^encoder
                               (lambda (ref block)
                                 (await (<- save-block ref block))))))
  (define decode (b-run (spawn ^decoder
                               (lambda (ref)
                                 (await (<- read-block ref))))))
  (define content (random-string))
  (define readcap (b-run ($ encode content)))
  (define result (b-run ($ decode readcap)))
  (test-assert "content == result" (string=? content result)))

(test-group "logs/proxy-content-tests"
  (define (^bad-block-actor _bcom)
    (lambda (_ __) (error 'fail)))
  (define blocks (make-hash-table))
  (define (write-block ref block) (hash-set! blocks ref block))
  (define (read-block ref) (hash-ref blocks ref))
  (define block-writer1 (a-run (spawn ^block-writer write-block)))
  (define block-writer2 (a-run (spawn ^block-writer write-block)))
  (define block-writer3 (a-run (spawn ^block-writer write-block)))
  (define block-writer4 (a-run (spawn ^bad-block-actor)))
  (define block-writers (list block-writer1
                              block-writer2
                              block-writer3
                              block-writer4
                              ))
  (define block-reader1 (a-run (spawn ^block-reader read-block)))
  (define block-reader2 (a-run (spawn ^block-reader read-block)))
  (define block-reader3 (a-run (spawn ^block-reader read-block)))
  (define block-reader4 (a-run (spawn ^bad-block-actor)))
  (define block-readers (list block-reader1
                              block-reader2
                              block-reader3
                              block-reader4
                              ))
  (define proxy-read-block (b-run (spawn ^proxy-block-reader block-readers)))
  (define proxy-write-block (b-run (spawn ^proxy-block-writer block-writers)))
  (define-vat-run c-run (spawn-vat))
  (define write-content (c-run (spawn ^content-writer proxy-write-block)))
  (define read-content (c-run (spawn ^content-reader proxy-read-block)))
  (define content (random-string))
  (define readcap (c-run ($ write-content content)))
  (define result (c-run ($ read-content content)))
  (test-assert "content == result" (string=? content result)))
