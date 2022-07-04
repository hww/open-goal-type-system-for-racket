#lang racket/base
;; ----------------------------------------------------------------------------
;;
;; Valeriya P.
;; https://gist.github.com/hww
;; _______ ________ ________
;; |   |   |  |  |  |  |  |  |
;; |       |  |  |  |  |  |  |
;; |___|___|________|________|
;;
;; ----------------------------------------------------------------------------

(require racket/generic)

(provide (all-defined-out))

(define RegClass::GPR-64 0)
(define RegClass::FLOAT 1)
(define RegClass::INT-128 2)
(define RegClass::VECTOR_FLOAT 3)
(define RegClass::INVALID 4)

;; Inspect the object

(define-generics inspectable
  [inspect inspectable]
  )


;; To make a way to see a difference TODO redundant?

(define-generics comparable
  [diff comparable other]
  [compare comparable othe])

;; The virtual interface of type

(define-generics typeable
  [is-reference? typeable]
  ;; when loading data of typable type into a register,
  ;; how many bytes do we need?
  [get-load-size typeable]
  ;; do we need to sign extend when loading?
  [get-load-signed typeable]
  ;; how much space does typable use in memory?
  ;; For value types, typable is the same as load size, as
  ;; value type data is loaded directly into registers.
  [get-size-in-memory typeable]
  [get-offset typeable]
  [get-in-memory-alignment typeable]
  [get-inl-array-stride-align typeable]
  [get-inl-array-start-align typeable]
  [get-preferred-reg-class typeable])
