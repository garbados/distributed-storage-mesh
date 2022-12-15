;; TEST SETUP

(use-modules ((goblins)
              #:select (spawn
                        spawn-vat
                        define-vat-run
                        $
                        <-))
             ((goblins actor-lib methods)
              #:select (methods))
             ((goblins actor-lib cell)
              #:select (^cell))
             ((dsm)
              #:select (^encoder
                        ^decoder
                        ^block-writer
                        ^block-reader
                        ^content-writer
                        ^content-reader
                        ^proxy-block-writer
                        ^proxy-block-reader
                        ^content-provider))
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

;; beginning from simple capabilities,
;; of the ability to write and the ability to read,
;; we can construct an advanced distributed database.
;; we can even divide these capabilities across many machines.
(test-group "logs/encoder_decoder_model_works"
  (for-each
   (lambda (_)
     ;; define how we read and write data
     (define blocks (make-hash-table))
     (define (write-block ref block) (hash-set! blocks ref block))
     (define (read-block ref) (hash-ref blocks ref))
     ;; then pass them to our encoder (writer) and decoder (reader)
     ;; we can divide this across two machines like it's nothing.
     (define encode (a-run (spawn ^encoder write-block)))
     (define decode (b-run (spawn ^decoder read-block)))
     ;; then we can encode whatever we want
     ;; and have one actor encode it
     ;; but another read it back.
     (define content (random-string))
     (define readcap (a-run ($ encode content)))
     (define result (a-run (await (<- decode readcap))))
     (test-assert "content == result" (string=? content result)))
   (make-list 100)))

;; we can think of this database as consisting primarily of two roles:
;; - the block provider: persisting blocks to disk and reading them back
;; - the content provider: encoding content into blocks and resolving readcaps
;; but this really conflates four roles, so the API is awkward and repetitive.
;; however it illustrates an important point:
;; different actors have different responsibilities
;; which make them privy to different information.
;; the content provider knows what content it is given;
;; the block provider doesn't.
(test-group "logs/block_content_provider_works"
  (define content (random-string))
  (define (^memory-block-provider _bcom)
    (define blocks (make-hash-table))
    (methods
     ((save-block ref block)
      (hash-set! blocks ref block))
     ((read-block ref)
      (hash-ref blocks ref))))
  (define (^my-content-provider _bcom save-block read-block)
    (define encode (spawn ^encoder save-block))
    (define decode (spawn ^decoder read-block))
    (methods
     ((save-content content)
      ($ encode content))
     ((read-content readcap)
      ($ decode readcap))))
  (define block-provider (a-run (spawn ^memory-block-provider)))
  (define my-content-provider
    (b-run
     (spawn ^my-content-provider
            (lambda (ref block)
              (await (<- block-provider 'save-block ref block)))
            (lambda (ref)
              (await (<- block-provider 'read-block ref))))))
  (define readcap (b-run ($ my-content-provider 'save-content content)))
  (test-assert "content == result" (string=? content result)))

;; we can even compose actors up and down the stack.
;; perhaps our read-block and save-block functions
;; are really pinging a whole different machine.
;; it's all the same to us.
(test-group "logs/remote-save-read-works"
  (define blocks (make-hash-table))
  ;; make save-block and read-block into actor-objects (still just functions)
  (define (^save-block _bcom)
    (lambda (ref block) (hash-set! blocks ref block)))
  (define (^read-block _bcom)
    (lambda (ref) (hash-ref blocks ref)))
  (define save-block (a-run (spawn ^save-block)))
  (define read-block (a-run (spawn ^read-block)))
  ;; then our encoder and decoder can ping these other machines
  (define encode
    (b-run (spawn ^encoder
                  (lambda (ref block)
                    (await (<- save-block ref block))))))
  (define decode
    (b-run (spawn ^decoder
                  (lambda (ref)
                    (await (<- read-block ref))))))
  ;; and it all works out the same
  (define content (random-string))
  (define readcap (b-run ($ encode content)))
  (define result (b-run ($ decode readcap)))
  (test-assert "content == result" (string=? content result)))

;; the model of the "proxy" allows one actor to access many
;; in order to syndicate reads and writes
;; while behaving like a single backend to users.
;; this allows parallel reads and writes,
;; routing around errors,
;; and higher availability.
(test-group "logs/proxy-content-tests"
  ;; some actors will be a problem on purpose
  (define (^bad-block-actor _bcom)
    (lambda (_ __) (error 'fail)))
  (define blocks (make-hash-table))
  (define (write-block ref block) (hash-set! blocks ref block))
  (define (read-block ref) (hash-ref blocks ref))
  (define block-writer1 (a-run (spawn ^block-writer write-block)))
  (define block-writer2 (a-run (spawn ^block-writer write-block)))
  (define block-writer3 (a-run (spawn ^block-writer write-block)))
  (define block-writer4 (a-run (spawn ^bad-block-actor)))
  (define block-writers
    (b-run (spawn ^cell (list block-writer1
                              block-writer2
                              block-writer3
                              block-writer4
                              ))))
  (define block-reader1 (a-run (spawn ^block-reader read-block)))
  (define block-reader2 (a-run (spawn ^block-reader read-block)))
  (define block-reader3 (a-run (spawn ^block-reader read-block)))
  (define block-reader4 (a-run (spawn ^bad-block-actor)))
  (define block-readers
    (b-run (spawn ^cell (list block-reader1
                              block-reader2
                              block-reader3
                              block-reader4
                              ))))
  (define proxy-read-block (b-run (spawn ^proxy-block-reader block-readers)))
  (define proxy-write-block (b-run (spawn ^proxy-block-writer block-writers)))
  (define-vat-run c-run (spawn-vat))
  (define write-content (c-run (spawn ^content-writer proxy-write-block)))
  (define read-content (c-run (spawn ^content-reader proxy-read-block)))
  (define content (random-string))
  (define readcap (c-run ($ write-content content)))
  (define result (c-run ($ read-content content)))
  (test-assert "content == result" (string=? content result)))

;; the model of the "content provider"
;; provides a generic interface for app devs
;; to save immutable, generic content.
;; it allows the responsibilities of storage and retrieval
;; to be distributed across any number of peers:
;; some storing, some proxying, some reading.
(test-group "logs/content_provider_works"
  ;; computer 1 defines a content provider
  (define content-provider (a-run (spawn ^content-provider)))
  ;; but computer 2 is running block operations
  (define blocks (make-hash-table))
  (define (write-block ref block) (hash-set! blocks ref block))
  (define (read-block ref) (hash-ref blocks ref))
  (define block-writer
    (b-run (spawn ^block-writer write-block)))
  (define block-reader
    (b-run (spawn ^block-reader read-block)))
  ;; computer 3 grants the ability to use computer 2's block ops
  (c-run (await (<- content-provider 'add-writer block-writer)))
  (c-run (await (<- content-provider 'add-reader block-reader)))
  (define content (random-string))
  ;; then computer 3 uses the provider on computer 1
  ;; to read and write blocks on computer 2
  ;; wow!
  ;; FIXME this hangs
  (define readcap
    (c-run
     (await (<- content-provider 'write-content content))))
  (define result
    (c-run
     (await (<- content-provider 'read-content readcap))))
  (test-assert "content == result" (string=? content result)))
