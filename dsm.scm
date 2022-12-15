(define-module (dsm)
  #:export (^encoder
            ^decoder
            ^block-writer
            ^block-reader
            ^content-writer
            ^content-reader
            ^proxy-block-writer
            ^proxy-block-reader
            ^content-provider)
  #:use-module ((dsm utils)
                #:select (await await-all await-one))
  #:use-module ((goblins)
                #:select (spawn $ <-))
  #:use-module ((goblins contrib syrup)
                #:select (syrup-encode
                          syrup-decode))
  #:use-module ((goblins actor-lib cell)
                #:select (define-cell))
  #:use-module ((goblins actor-lib methods)
                #:select (methods))
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
       (pk 'pre-write)
       (write-block (car ref-block) (cdr ref-block))
       (pk 'write))))
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
      ;; efficiently get the length of the content
      (define len (eris-decoder-length decoder))
      ;; decode the block into a buffer
      (define buffer (make-bytevector len))
      (eris-decoder-read! decoder buffer 0 len)
      ;; syrup decodes into the original object
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
  ;; write-content
  (spawn ^encoder write-block))

(define (^content-reader _bcom block-reader)
  ;; reading blocks occurs on another machine
  (define (read-block ref)
    (await (<- block-reader ref)))
  ;; read-content
  (spawn ^decoder read-block))

;; write using a cell containing a list of block-writers.
;; optional n parameter specifies number of allowed errors
(define* (^proxy-block-writer _bcom block-writers
                              #:optional (n 1))
  (lambda (ref block)
    (define-values (readcap errors)
      (await-all
       (map
        (lambda (writer)
          (<- writer ref block))
        ($ block-writers))))
    (if (> (length errors) n)
        ;; gather the releases into a single function
        readcap
        ;; fail if anything failed
        (error 'failed errors 'releases releases))))

;; read using a cell containing a list of block-readers
;; optional n parameter specifies number of required agreements
(define* (^proxy-block-reader _bcom block-readers
                              #:optional (n 1))
  (lambda (ref)
    (await-n
     (map
      (lambda (reader) (<- reader ref))
      ($ block-readers))
     n)))

(define* (^proxy-content-writer _bcom
                                #:optional
                                (init-writers '()))
  (define-cell block-writers init-writers)
  (define write-block (spawn ^proxy-block-writer block-writers))
  (define write-content (spawn ^content-writer write-block))
  (methods
   ((write-content #:rest args)
    (apply $ write-content args))
   ((add-writer writer)
    ($ block-writers (cons writer ($ block-writers))))
   ((remove-writer writer)
    ($ block-writers (delq writer ($ block-writers))))))

(define* (^proxy-content-reader _bcom
                                #:optional
                                (init-readers '()))
  (define-cell block-readers init-readers)
  (define read-block (spawn ^proxy-block-reader block-readers))
  (define read-content (spawn ^content-reader read-block))
  (methods
   ((read-content #:rest args)
    (apply $ read-content args))
   ((add-reader reader)
    ($ block-readers (cons reader ($ block-readers))))
   ((remove-reader reader)
    ($ block-readers (delq reader ($ block-readers))))))

;; all-in-one multi-backend reads and writes
(define* (^content-provider _bcom
                            #:optional
                            (init-writers '())
                            (init-readers '()))
  (define content-writer (spawn ^proxy-content-writer init-writers))
  (define content-reader (spawn ^proxy-content-reader init-readers))
  (methods
   ((read-content #:rest args)
    (apply $ content-reader 'read-content args))
   ((add-reader reader)
    ($ content-reader 'add-reader reader))
   ((remove-reader reader)
    ($ content-reader 'remove-reader reader))
   ((write-content #:rest args)
    (apply $ content-writer 'write-content args))
   ((add-writer writer)
    ($ content-writer 'add-writer writer))
   ((remove-writer writer)
    ($ content-writer 'remove-writer writer))))
