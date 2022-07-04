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

(require racket/contract
         racket/list
         sexp-diff)

(require "interfaces.rkt" "vmc-lib.rkt")

(provide (all-defined-out))

;; -----------------------------------------------------------------------------
;; The type tag
;;
;; Every type may have a list of tagged values
;; for eample :align 32. This structure represent
;; single tag-value pair
;; @param (tag symbol?) the tag
;; @param (value any?) the value
;; -----------------------------------------------------------------------------

(define-struct type-tag (name value)
  #:mutable
  #:transparent)

(define (type-tag-new it)
  (cond
    ((null? it) null)
    ((type-tag? it) it)
    ((pair? it)
     (unless (symbol? (car it))
       (error (format "Unexpeced type-tag key ~a" it)))
     (when (null? (cdr it))
       (error (format "Undefined type-tag value ~a" it)))
     (type-tag (car it) (cadr it)))
    (else
     (error (format "Unexpeced type-tag syntax ~a" it)))))

(define (type-tag-new* lst)
  (let :loop: ((it lst))
    (if (null? it)
        null
        (cons (type-tag-new it) (:loop: (cddr it))))))

;; -----------------------------------------------------------------------------
;; The type specification
;;
;; @param (type symbol?) name - the name
;; @param (arguments list?) args - the list of type-spec arguments
;; @param (arguments type-tag?) tags - the list of type-tag items
;; -----------------------------------------------------------------------------
;; There are two cases possible: with arguments and withot
;; The example of types
;; none
;; int
;; int32
;; pointer
;; (pointer int32)
;; (function int (pointer int32) (pointer int32) (pointer int32) none)
;; -----------------------------------------------------------------------------

(define-struct type-spec (type args tags)
  #:transparent
  #:mutable
  #:methods gen:inspectable
  [
   (define (inspect this)
     (type-spec-inspect this))
   ]
  #:methods gen:comparable
  [(define (compare this other) (equal? this other))
   (define (diff this other)    (type-spec-diff this other))])

;; Construct

(define/contract (type-spec-new (type 'none) (args null) . tags)
  (->* () (symbol? list?) #:rest list? type-spec?)
  (make-type-spec type args (type-tag-new* tags)))

;; Diff

(define/contract (type-spec-diff this other)
  (-> type-spec? any/c string?)
  (list->string (sexp-diff this other)))

;; The inspector for type spec

(define/contract (type-spec-inspect this)
   (-> type-spec? void)
  (if (and (type-spec-args-empty? this) (type-spec-tags-empty? this))
      (format "~a" (type-spec-type this))
      (apply string-append
             (append
              (list (format "(~a" (type-spec-type this)))
              (map (lambda (it) (string-append " " (type-spec-inspect it)))
                   (type-spec-args this))
              (map (lambda (it) (format " ~a ~a" (type-tag-name it) (type-tag-value it)))
                   (type-spec-tags this))
              (list ")")))))

;; alias fucntion

(define/contract (type-spec-base-type ts)
  (-> type-spec? symbol?)
  (type-spec-type ts))

;; Test Tags -------------------------------------------------------------------

(module+ test
  (require rackunit)
  (let ((ts (type-spec-new 'baz)))
    (check-equal? (type-spec-inspect ts) "baz")
    (check-equal? (type-spec-base-type ts) 'baz)
  (let ((ts (type-spec-new 'bazz '() ':x 1 ':y 2)))
    (check-equal? (type-spec-inspect ts) "(bazz :x 1 :y 2)")
    (check-equal? (type-spec-base-type ts) 'bazz)
    )))

;; -----------------------------------------------------------------------------
;;
;; -----------------------------------------------------------------------------

(define/contract (type-spec-substitute-for-method-call this method-type)
(-> type-spec? symbol? type-spec?)
  (define result (type-spec-new))
  (if (== (type-spec-type this) '-type-)
      (set-type-spec-type! result method-type)
      (set-type-spec-type! result (type-spec-type this)))

  (when (not (null? (type-spec-args this)))
             (set-type-spec-args!
              result
              (map (lambda (arg) (type-spec-substitute-for-method-call arg method-type))
                   (type-spec-args this))))
  result)

(define/contract (type-spec-is-compatible-child-method this  implementation child-type)
  (-> type-spec? type-spec? symbol? boolean?)
  ;; the base types should be same or if <this> ins the type -type-
  ;; and <implementation> type eqaul child type
  (define ok (or (== (type-spec-type implementation) (type-spec-type this))
                 (and (== (type-spec-type this) '_type_)
                      (== (type-spec-type implementation)  child-type))))
  (cond
    ((or (not ok) (!= (type-spec-args-count implementation) (type-spec-args-count this)))
     ;; if previous test failed and if args count different
     false)
    (else
     ;; all looks fine, then check each the argument
     (let :loop: ((i 0))
       (if (< i (type-spec-args-count this))
           (if (not (type-spec-is-compatible-child-method (type-spec-args-ref this i)
                                                   (type-spec-args-ref implementation i)
                                                   child-type))
               ;; the argument is not compatible
               false
               ;; try next argument
               (:loop: (1+ i)))
           ;; no mor arguments
           true)))))

;; -----------------------------------------------------------------------------
;; Tags API
;; -----------------------------------------------------------------------------

;; Find the tag with the name or return null

(define/contract (type-spec-try-get-tag this tag)
  (-> type-spec? symbol? (or/c boolean? type-tag?))
  (let :loop: ((it (type-spec-tags this)))
    (if (null? it)
        #f
        (if (equal? tag (type-tag-name (car it)))
            (car it)
            (:loop: (cdr it))))))

;; Make error if there is this tag

(define/contract (type-spec-add-new-tag this tag value)
  (-> type-spec? symbol? any/c void)
  (let ((item (type-spec-try-get-tag this tag)))
    (cond
      ((not item)
       (set-type-spec-tags! this (cons (make-type-tag tag value)
                                       (type-spec-tags this))))
      (else
       (error "Attempted to add a duplicate tag {~a} to typespec." tag)))))

;; Make an error if the tag is not found

(define/contract (type-spec-modify-tag this tag value)
  (-> type-spec? symbol? any/c void)
  (let ((item (type-spec-try-get-tag this tag)))
    (cond
      ((not item)   (error "Attempted to modify  tag {~a} to typespec." tag))
      (else         (set-type-tag-value! item value)))))

;; Find amd modify tag or add new for

(define/contract (type-spec-add-or-modify-tag this tag value)
  (-> type-spec? symbol? any/c void)
  (let ((item (type-spec-try-get-tag this tag)))
    (cond
      ((not item)   (type-spec-add-new-tag this tag value))
      (else         (set-type-tag-value! item value)))))

;; Make exception if there are no this tag

(define/contract (type-spec-safe-find-tag this tag)
  (-> type-spec? symbol? (or/c type-tag? boolean?))
  (let ((item (type-spec-try-get-tag this tag)))
    (cond
      ((not item)   (error "Attempted to modify  tag {~a} to typespec." tag))
      (else         item))))

;; Check if the tags list empty

(define/contract (type-spec-tags-empty? this)
  (-> type-spec? boolean?)
  (null? (type-spec-tags this)))

;; Return number of tags

(define/contract (type-spec-tags-couny this)
   (-> type-spec? integer?)
  (length (type-spec-tags this)))

;; Test Tags -------------------------------------------------------------------

(module+ test
  (require rackunit)
  (check-equal? (type-tag 'foo 1) (type-tag 'foo 1))
  (check-not-equal? (type-tag 'foo 1) (type-tag 'bar 1))
  (check-not-equal? (type-tag 'foo 1) (type-tag 'foo 2))
  (let ((ts (type-spec-new 'baz)))
    (type-spec-add-new-tag ts 'foo 1)
    (type-spec-add-new-tag ts 'bar 2)
    (check-equal? (type-spec-try-get-tag ts 'foo) (type-tag 'foo 1))
    (type-spec-modify-tag ts 'foo 3)
    (check-equal? (type-spec-try-get-tag ts 'foo) (type-tag 'foo 3))
    (type-spec-add-or-modify-tag ts 'baz 4)
    (check-equal? (type-spec-try-get-tag ts 'baz) (type-tag 'baz 4))
    ))

;; -----------------------------------------------------------------------------
;; The arguments api
;; -----------------------------------------------------------------------------

;; Check if the arguments list is empty

(define/contract (type-spec-args-count this)
  (-> type-spec? integer?)
  (length (type-spec-args this)))

;; Reference to the argument

(define/contract (type-spec-args-ref this i)
  (-> type-spec? integer? type-spec?)
  (list-ref (type-spec-args this) i))

;; Reference to the argument

(define/contract (type-spec-args-last this i)
  (-> type-spec? integer? type-spec?)
  (null? (type-spec-args this)
         #f
         (last (type-spec-args this))))

(define/contract (type-spec-args-first this i)
  (-> type-spec? integer? type-spec?)
  (null? (type-spec-args this)
         #f
         (first (type-spec-args this))))

;; Check if the arguments list is empty

(define/contract (type-spec-args-empty? this)
  (-> type-spec? boolean?)
  (null? (type-spec-args this)))

;; Add the argument

(define/contract (type-spec-args-add this arg)
  (-> type-spec? type-spec? void)
  (set-type-spec-args! this (append (type-spec-args this) (list arg))))

;; No arguments

(define/contract (type-spec-empty? this)
  (-> type-spec? boolean?)
  (== 0 (length (type-spec-args this))))


;; Test Args -------------------------------------------------------------------

(module+ test
  (require rackunit)
  (let ((ts (type-spec-new 'foo))
        (int (type-spec-new 'int)))
    (type-spec-args-add ts int)
    (type-spec-args-add ts int)
    (check-equal? (type-spec-args-ref ts 0) (type-spec 'int '() '()))
    (check-equal? (type-spec-args-count ts) 2)
    ))
