(define-module (dsm)
  #:export (^encoder
            ^decoder
            ^block-writer
            ^block-reader
            ^content-writer
            ^content-reader
            ^proxy-block-writer
            ^proxy-block-reader)
  #:use-module ((dsm utils)
                #:select (await await-all await-one))
  #:use-module ((goblins)
                #:select (spawn $ <-))
  #:use-module ((goblins contrib syrup)
                #:select (syrup-encode
                          syrup-decode))
  #:use-module ((eris)
                #:select (eris-encode
                          eris-decode
                          %null-convergence-secret))
  #:use-module ((eris decoder)
                #:select (eris-decoder-init
                          eris-decoder-length
                          eris-decoder-read!))
  #:use-module ((rnrs bytevectors)
                #:select (make-bytevector
                          utf8->string))
  #:use-module ((eris read-capability)
                #:select (->eris-read-capability))
  #:use-module ((gcrypt random)
                #:select (gen-random-bv
                          %gcry-very-strong-random)))

(define (random-secret)
  (gen-random-bv 32 %gcry-very-strong-random))

;; who does the work of encoding
(define (^encoder _bcom write-block)
  (define reducer
    (case-lambda
      (() '())
      ((_) 'done)
      ((_ ref-block)
       (write-block (car ref-block) (cdr ref-block)))))
  (lambda* (content #:optional
                    (key (random-secret))
                    (block-size 'small))
    ;; optionally use the "public convergence secret"
    ;; so the content can be deduped against other holders.
    ;; good for content intended for broad dissemination;
    ;; generally inadvisable.
    (if (eq? key #f) (set! key %null-convergence-secret))
    (eris-encode (syrup-encode content)
                 #:block-size block-size
                 #:block-reducer reducer
                 #:convergence-secret key)))

(define (^decoder _bcom read-block)
  ;; read-content
  (lambda (readcap)
    ;; init a decoder
    (define eris-readcap (->eris-read-capability readcap))
    (define (decode eris-readcap)
      (define decoder (eris-decoder-init
                       eris-readcap
                       #:block-ref read-block))
      ;; efficiently get the lenght of the content
      (define len (eris-decoder-length decoder))
      ;; decode the block into a buffer
      (define buffer (make-bytevector len))
      (eris-decoder-read! decoder buffer 0 len)
      ;; syrup rehydrates into the original object
      (syrup-decode buffer))
    (if (not eris-readcap)
        readcap
        (decode eris-readcap))))

;; the block writer and reader home the work of persistence
;; to whatever vat spawns them.
(define (^block-writer _bcom write-block) write-block)
(define (^block-reader _bcom read-block) read-block)

(define (^content-writer _bcom block-writer)
  ;; blocks are written on another machine
  (define (write-block ref block)
    (await (<- block-writer ref block)))
  ;; save-content
  (spawn ^encoder write-block))

(define (^content-reader _bcom block-reader)
  ;; reading blocks occurs on another machine
  (define (read-block ref)
    (await (<- block-reader ref)))
  ;; read-content
  (spawn ^decoder read-block))

(define (^proxy-block-writer _bcom block-writers)
  (lambda (ref block)
    ;; only one write must succeed, really
    (define n (1- (length block-writers)))
    (define-values (readcap errors)
      (await-all
       (map
        (lambda (writer)
          (<- writer ref block))
        block-writers)))
    (if (< n (length errors))
        ;; gather the releases into a single function
        readcap
        ;; fail if anything failed
        (error 'failed errors 'releases releases))))

(define (^proxy-block-reader _bcom block-readers)
  (lambda (ref)
    (await-one
     (map
      (lambda (reader) (<- reader ref))
      block-readers))))
