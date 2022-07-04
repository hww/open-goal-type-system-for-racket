#lang racket

(require data/gvector)


(define defaut-methods-count 16)

;; Main methods
(define new-method 0);       
(define delete-method 1);
(define print-method 2);
(define inspect-method 3);
(define length-method 4);
(define asize-of-method 4);
(define copy-method 5);
(define relocate-method 6);
(define memusage-method 7);

;; The type used in runtime, it is not a compiler
;; type
(define-struct rt-type
  (
   name            ; symbol? The type's symbol
   parent          ; rtype? The type's parent
   allocated-size  ; int? The type's size in memory
   padded-size     ; int? The type's size, when padded?
   heap-base       ; int? relative location of heap
   methods-num
   methods         ; gvector?
   )
  #:transparent
  #:mutable
  )

;; Get the method with index
(define (type-get-method this idx)
  (if (null? this)
      null
      (gvector-ref (rt-type-methods this) idx)))

;; Get the number of methods for this type
(define (type-get-method-cnt this)
  (rt-type-methods-num this))

;; The constructor
(define (new-type name parent methods-num alloc-size)
  (let ((obj (make-rt-type
              name
              parent
              0         ; allocated-size
              0         ; padded-size
              0         ; heap-base
              (make-gvector #:capacity (if (eq? 0 methods-num) defaut-methods-count methods-num))
              )))
    (unless (null? parent)
      (set-rt-type-methods! obj (vector->gvector (gvector->vector ((rt-type-methods parent))))))
    (set-type-values obj parent methods-num alloc-size)
    ))

;; Configure a type.
(define (set-type-values this parent methods-num alloc-size)
  (set-rt-type-parent! this parent)
  (set-rt-type-allocated-size! this alloc-size)
  (set-rt-type-heap-base! this 0)
  (set-rt-type-padded-size! this (and (+ #xf alloc-size) #xfffffff0))
  (set-rt-type-methods-num! this (max methods-num (rt-type-methods-num this)))
  this)

;; this = he type
;; idx = the method's index
;; options = 0 do nothing
;; options = 1 set parent method
;; options = 2 set method last argument
(define (type-set-method this idx options [m null])
  (let ((method (vector-ref (rt-type-methods this) idx)))
  (cond
    ((eq? options 0)            method)
    ((eq? options 1)            (gvector-set! (rt-type-methods this) idx m)
    ((eq? options 2)            (gvector-set! (type-get-method (rt-type-parent this) idx)))
    ))))



