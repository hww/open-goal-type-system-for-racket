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

(require racket/generic
         racket/contract
         racket/format
         racket/hash
         racket/list
         racket/string
         data/gvector)

(require (for-syntax racket/base racket/string))
(require (for-syntax racket/syntax racket/base syntax/parse))

(require "vmc-lib.rkt")
(require "interfaces.rkt")

(require racket/lazy-require)
(lazy-require
 ["type-spec.rkt"
  (type-spec? type-spec)])

(provide (all-defined-out))

(define EMPTY-SYMBOL (string->symbol ""))

;; ----------------------------------------------------------------------------
;; The method info
;; ----------------------------------------------------------------------------

;; The method info structure

(define-struct method-info (id name type defined-in-type no-virtual override)
  #:mutable
  #:transparent
  #:methods gen:inspectable
  [(define (inspect this)
     (method-info-inspect this))]
  #:methods gen:comparable
  [(define (compare this other)
     (equal? this other))
   (define (diff this other)
     (method-info-diff this other))]
  )

;; Most useful constructor
(define-syntax (method-info-new stx)
  (syntax-parse stx
    ;;
    ((_ id name type defined-in-type no-virtual override)
     #'(method-info id name type defined-in-type no-virtual override))
    ;;
    ((_ id name type defined-in-type)
     #'(method-info id name type defined-in-type #f #f))
    ;;
    ;; ((_ name type)
    ;;  #'(method-info -1 name type EMPTY-SYMBOL #f #f))
    ))

;; One line inspector

(define (method-info-inspect this)
  (format "Method ~a: ~a ~a"
          (~a (method-info-id this) #:width 3)
          (~a (method-info-name this) #:width 20)
          (inspect (method-info-type this))))

;; Compare types TODO Not sure it is needed

(define (method-info-diff this other)
  (define-with-struct lhs. (method-info id name type defined-in-type no-virtual override) this)
  (define-with-struct rhs. (method-info id name type defined-in-type no-virtual override) other)
  (define result "")
  (when (!= lhs.id rhs.id)
   (string-append! result (format "id: ~a vs. ~a\n" lhs.id rhs.id)))
  (when (!= lhs.name rhs.name)
   (string-append! result (format "name:: ~a vs. ~a\n" lhs.name rhs.name)))
  (when (!= lhs.type rhs.type)
   (string-append! result (format "type: ~a vs. ~a\n" (inspect lhs.type) (inspect rhs.type))))
  (when (!= lhs.defined-in-type rhs.defined-in-type)
   (string-append! result (format "defined-in-typ: ~a vs. ~a\n" lhs.defined-in-type rhs.defined-in-type)))
  (when (!= lhs.no-virtual rhs.no-virtual)
   (string-append! result (format "no-virtual: ~a vs. ~a\n" lhs.no-virtual rhs.no-virtual)))
  (when (!= lhs.override rhs.override)
    (string-append! result (format "overrides: ~a vs. ~a\n" lhs.override rhs.override)))
  result)
   
;; Test MethodInfo  -------------------------------------------------------------------

(module+ test
  (require rackunit)
  (let ((mi1 (method-info-new 1 'name1 (type-spec 'foo1 '() '()) 'foo1 #f #t))
        (mi2 (method-info-new 2 'name2 (type-spec 'foo2 '() '()) 'foo2 #f #t)))
    (check-equal? mi1 (method-info 1 'name1 (type-spec 'foo1 '() '()) 'foo1 #f #t))

    (check-equal? (diff mi1 mi2) "id: 1 vs. 2\nname:: name1 vs. name2\ntype: foo1 vs. foo2\ndefined-in-typ: foo1 vs. foo2\n")
    ))

;; ----------------------------------------------------------------------------
;; The constants
;; ----------------------------------------------------------------------------

;; The offset if the method from begin of methods table

(define GOAL-NEW-METHOD  0)       ;; method ID of GOAL new
(define GOAL-DEL-METHOD  1)       ;; method ID of GOAL delete
(define GOAL-PRINT-METHOD  2)     ;; method ID of GOAL print
(define GOAL-INSPECT-METHOD  3)   ;; method ID of GOAL inspect
(define GOAL-LENGTH-METHOD  4)    ;; method ID of GOAL length
(define GOAL-ASIZE-METHOD  5)     ;; method ID of GOAL size
(define GOAL-COPY-METHOD  6)      ;; method ID of GOAL copy
(define GOAL-RELOC-METHOD  7)     ;; method ID of GOAL relocate
(define GOAL-MEMUSAGE-METHOD  8)  ;; method ID of GOAL mem-usage

;; ----------------------------------------------------------------------------
;; Utility struct used by the parser
;; ----------------------------------------------------------------------------

;; Flags for any definition
;; All fields are integers
(define-struct type-flags (size heap-base methods pad) #:mutable #:transparent)
(define (type-flags-new) (type-flags 0 0 0 0))
(define (type-flags-flag flags)
  (define-with-struct (type-flags size heap-base methods pad) flags)
  (+ (bitwise-and size      #xFFFF)
     (arithmetic-shift (bitwise-and heap-base #xFFFF) 16)
     (arithmetic-shift (bitwise-and methods   #xFFFF) 32)
     (arithmetic-shift (bitwise-and pad       #xFFFF) 48)))

;; ----------------------------------------------------------------------------
;; The type
;; ----------------------------------------------------------------------------

(define-struct type
  (
   methods                   ; vector<method-info>
   states                    ; hash<string,type-spec>
   new-method-info           ; MethodInfo
   new-method-info-defined   ; false
   generate-inspect          ; true
   parent                    ; string?  the parent type (is empty for none and object)
   name                      ; string?
   allow-in-runtime          ; bool? true;
   runtime-name              ; string?
   is-boxed                  ; bool? false  // does this have runtime type information?
   heap-base                 ; int? 0
   )
  #:mutable
  #:transparent
  #:methods gen:comparable
  [(define (compare this other)
     (equal? this other))
   (define (diff this other)
     (type-diff this other))]
  #:methods gen:inspectable
  [(define (inspect this)
     (format "[Type] {~a}" (type-name this)))]
  #:methods gen:typeable
  [
   (define (is-reference? this)
     (error "Abstract!"))
   ; when loading data of typable type into a register, how many bytes do we
   ; need?
   (define (get-load-size this)
     (error "Abstract!"))
   ; do we need to sign extend when loading?
   (define (get-load-signed this)
     (error "Abstract!"))
   ; how much space does typable use in memory? For value types, typable is the
   ; same as load size, as value type data is loaded directly into registers.
   (define (get-size-in-memory this)
     (error "Abstract!"))
   (define (get-offset this)
     (error "Abstract!"))
   (define (get-in-memory-alignment this)
     (error "Abstract!"))
   (define (get-inl-array-stride-align this)
     (error "Abstract!"))
   (define (get-inl-array-start-align this)
     (error "Abstract!"))
   (define (get-preferred-reg-class this)
      (error "Abstract!"))
   ]
  )

;; Default constructor

(define/contract (type-new-0)
  (-> type?)
  (type
   (make-gvector)   ; methods
   (make-hash)      ; states
   null             ; new-method
   #f               ; new-method-info-defined
   #t               ; generate inspect
   'none            ; parent
   'none            ; name
   #t               ; allow in runtime
   EMPTY-SYMBOL     ; runtime name
   #f               ; is boxed
   0                ; heap size
   ))

;; Typical constructor

(define/contract (type-new parent name is-boxed heap-base)
  (-> symbol? symbol? boolean? integer? type?)
  (let ((it (type-new-0)))
    [set-type-parent! it parent]
    [set-type-name! it name]
    [set-type-is-boxed! it is-boxed]
    [set-type-heap-base! it heap-base]
    [set-type-runtime-name! it name]
    it))

(define/contract (type-base-type this)
  (-> type? symbol?)
  (type-parent this))

;; Dsable type for runtime

(define/contract (type-disallow-in-runtime this)
  (-> type? void)
  (set-type-allow-in-runtime! this false))

;; Print information for all methods defined specifically for a type.
;; Does not print inherited methods.

(define/contract (type-methods-inspect this)
  (-> type? string?)
  (string-join
   (cons (if (type-new-method-info-defined this)
             (inspect (type-new-method-info this))
             "")
         (map (lambda (m) (inspect m))
              (gvector->list (type-methods this))))
   "\n"))


;; Does this type have a parent that makes sense to use? Object and none both
;; don't have meaningful parents.

(define/contract (type-has-parent? this)
  (-> type? boolean?)
  (and (!= (type-name this) 'object)
       (!= (type-parent this) EMPTY-SYMBOL)))

;; Returns the parent of false

(define/contract (type-get-parent this)
  (-> type? symbol?)
  (if (!= (type-name this) 'object)
      (type-parent this)
      'none))

;; Get a method that is defined specifically for this type. Returns if it was
;; found or not.

(define/contract (type-get-my-method this name)
  (-> type? symbol? (or/c #f method-info?))
  (for/gvector-find-item
      (lambda (it) (== name (method-info-name it)))
    (type-methods this)))

;; Get a method that is defined specifically in this type by id. Returns if it
;; was found or not.

(define/contract (type-get-my-method-by-id this id)
  (-> type? integer? (or/c method-info? #f))
  (assert (> id 0))  ;; 0 is new, should use explicit new method functions instead.
  (for/gvector-find-item (lambda (it) (== id (method-info-id it))) (type-methods this)))

;; Get the last method defined specifically for this type. Returns if there were
;; any methods defined specifically for this type or not.

(define/contract (type-get-my-last-method this)
  (-> type? (or/c #f method-info?))
  (if (== 0 (gvector-count (type-methods this)))
      #f
      (gvector-last (type-methods this))))

;; Get the new method defined specifically for this type. Returns if there is a
;; new method specific to this type or not.

(define/contract (type-get-my-new-method this)
  (-> type? (or/c #f method-info?))
  (if (type-new-method-info-defined this)
      (type-new-method-info this)
      #f))

;; Add a method defined specifically for this type.

(define/contract (type-add-method this info)
  (-> type? method-info? method-info?)
  (define info.id (method-info-id info))
  (for/gvector-find-item-rev
      (lambda (it)
        (when (not (method-info-override it))
          (assert (== (+ 1 (method-info-id it)) info.id))
          #t ;; stop iterating
          ))
    (type-methods this))
  (gvector-add! (type-methods this) info)
  info)

;; Add a NEW method defined specifically for this type. The name of this
;; function is confusing - this is specific to the method named NEW.

(define/contract (type-add-new-method this info)
  (-> type? method-info? method-info?)
  (assert (== (method-info-name info) 'new))
  (set-type-new-method-info-defined! this #t)
  (set-type-new-method-info! this info)
  info)

(define/contract (type-set-runtime-type this name)
  (-> type? symbol? void)
  (set-type-runtime-name! this name))


;; ----------------------------------------------------------------------------
;; Statens
;; ----------------------------------------------------------------------------

;; Add the state to the class

(define/contract (type-add-state this name type)
  (-> type? symbol? type-spec? void)
  (define state (hash-ref (type-states this) name #f))
  (when state
    (error (format "State ~a is multiply defined" name)))
  (hash-set! (type-states this) name type))

(define/contract (type-find-state this name)
  (-> type? symbol? (or/c #f type-spec?))
  (hash-ref (type-states this) name #f))

;; ----------------------------------------------------------------------------
;; The type
;; ----------------------------------------------------------------------------

(define/contract (incompatible-diff expected-type-name other)
  (-> symbol? type? string?)
  (format "diff is not implemented between ~a and ~a\n"
          expected-type-name other))


(define/contract (type-diff this other)
  (-> type? type? string?)

  (define-with-struct l. (type methods states new-method-info new-method-info-defined generate-inspect parent
                               name allow-in-runtime runtime-name  is-boxed heap-base) this)
  (define-with-struct r. (type methods states new-method-info new-method-info-defined generate-inspect parent
                               name allow-in-runtime runtime-name  is-boxed heap-base) other)
  (define result "")
  ;; Check methods count
  (when (!= l.methods r.methods)
    (let* ((l.methods.size (gvector-count l.methods))
           (r.methods.size (gvector-count r.methods))
           (min-methods-size (min l.methods.size r.methods.size)))
      (when (!= l.methods.size r.methods.size)
        (string-append! result (format "Number of additional methods ~a vs. ~a\n"
                                       l.methods.size r.methods.size)))
      ;; Check methods one to one
      (for ((i (in-range min-methods-size)))
        (let ((l (gvector-ref l.methods i))
              (r (gvector-ref r.methods i)))
          (when (!= l r)
            (string-append! result (format "Method def ~a (~a/~a):\n~a\n"
                                           i (method-info-name l) (method-info-name r)
                                           (method-info-diff l r))))))))
  ;; Check states
  (when (!= l.states r.states)
    "States are different:\n"
    (hash-map
     (lambda (name l.state)
       (let ((r.state (type-find-state other name)))
         (cond
           ((not r.state)
            (string-append! result (format "  ~a is in one, but not the other.\n" name))
            ((!= l.state r.state)
             (string-append! result (format "  ~a is defined differently: ~a vs ~a\n"
                                            name  (inspect l.state) (inspect r.state)))))))
       (type-states this))

     (hash-map
      (lambda (name l.state)
        (let ((r.state (type-find-state this name)))
          (when (not r.state)
            (string-append! result (format "  ~a is in one, but not the other.\n" name)))))
      (type-states other)))
    ;; Check other params
    (when (!= l.new-method-info r.new-method-info)
      (string-append! result (format "new-method-info: ~a vs ~a\n" l.new-method-info  r.new-method-info)))
    (when (!= l.new-method-info-defined r.new-method-info-defined)
      (string-append! result (format "new-method-info-defined: ~a vs. ~a\n" l.new-method-info-defined  r.new-method-info-defined)))
    (when (!= l.parent r.parent)
      (string-append! result (format "parent: ~a vs. ~a\n" l.parent r.parent)))
    (when (!= l.name r.name)
      (string-append! result (format "name: ~a vs. ~a\n" l.name r.name)))
    (when (!= l.allow-in-runtime r.allow-in-runtime)
      (string-append! result (format "allow-in-runtime: ~a vs. ~a\n" l.allow-in-runtime r.allow-in-runtime)))
    (when (!= l.runtime-name r.runtime-name)
      (string-append! result (format "runtime-name: ~a vs. ~a\n" l.runtime-name r.runtime-name)))
    (when (!= l.is-boxed r.is-boxed)
      (string-append! result (format "is-boxed: ~a vs. ~a\n" l.is-boxed r.is-boxed)))
    (when (!= l.heap-base r.heap-base)
      (string-append! result (format "heap-base: ~a vs. ~a\n" l.heap-base r.heap-base)))
    (when (!= l.generate-inspect r.generate-inspect)
      format("generate-inspect: ~a vs. ~a\n" l.generate-inspect r.generate-inspect)))
  result)
