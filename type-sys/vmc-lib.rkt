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
;;
(require (for-syntax racket/syntax racket/base syntax/parse))
(require racket/list racket/string racket/struct data/gvector)

(provide (all-defined-out))
;; ==============================================================================
;; Math Macros
;; ==============================================================================

(define true #t)
(define false #f)
(define (% n m) (modulo n m))
;; ----------------------------------------------------------------------------
;; Increments decrements
;; ----------------------------------------------------------------------------
;;
(define-syntax-rule (1+ v) (+ v 1))
(define-syntax-rule (2+ v) (+ v 2))
(define-syntax-rule (3+ v) (+ v 3))
(define-syntax-rule (1- v) (- v 1))
(define-syntax-rule (2- v) (- v 2))
(define-syntax-rule (3- v) (- v 3))

(define-syntax-rule (neg arg0) (- 0 arg0))
;; ----------------------------------------------------------------------------
;; In place operations
;; ----------------------------------------------------------------------------
(define-syntax-rule (neg! arg0)
  (set! arg0 (- 0 arg0)))
;;
(define-syntax-rule (+! arg0 arg1 ...)
  (set! arg0 (+ arg0 arg1 ...)))
;;
(define-syntax-rule (-! arg0 arg1 ...)
  (set! arg0 (- arg0 arg1 ...)))
;;
(define-syntax-rule (*! arg0 arg1 ...)
  (set! arg0 (* arg0 arg1 ...)))
;;
(define-syntax-rule (/! arg0 arg1 ...)
  (set! arg0 (/ arg0 arg1 ...)))
;;
(define-syntax-rule (+1! arg0)
  (set! arg0 (+ arg0 1)))
;;
(define-syntax-rule (-1! arg0)
  (set! arg0 (- arg0 1)))
;;
(define-syntax-rule (not! arg0)
  (set! arg0 (not arg0)))
;;
(define-syntax-rule (true! arg0)
  (set! arg0 #t))
;;
(define-syntax-rule (false! arg0)
  (set! arg0 #f))
;;
(module+ test
  (require rackunit)
  ;; Define the vector 2 structure
  (define x 0)
  (+! x 1)
  (check-equal? x 1)
  (-! x 2)
  (check-equal? x -1)
  (*! x 100)
  (check-equal? x -100)
  (/! x 2)
  (check-equal? x -50)
  (neg! x)
  (check-equal? x 50)
  (+1! x)
  (check-equal? x 51)
  (-1! x)
  (check-equal? x 50)

  )
;; ----------------------------------------------------------------------------
;; Minimum and maximum
;; ----------------------------------------------------------------------------
;;
(define-syntax-rule (minmax val minval maxval)
  (max (min val maxval) minval)
  )
(define-syntax-rule (minmax! val minval maxval)
  (set! val (max (min val maxval) minval))
  )
(define-syntax-rule (maxmin val minval maxval)
  (min (max val maxval) minval)
  )

;; ----------------------------------------------------------------------------
;; Aignements
;; ----------------------------------------------------------------------------

;; Align val to n-byte boundaries
(define-syntax-rule (align val n)
  (bitwise-and (- n) (+ val (- n 1))))

;; ----------------------------------------------------------------------------
;; Bit macros
;; ----------------------------------------------------------------------------

;; Align val to n-byte boundaries
(define-syntax-rule (align-n val n)
  (bitwise-and (- n) (+ val (- n 1))))
;;
(define-syntax-rule (align16 val)
  (align val 16))
;;
(define-syntax-rule (align64 val)
  (align val 64))
(require rnrs/arithmetic/bitwise-6)
;; extract bits from an integer value.
(define-syntax-rule (bit-field type val base size signed)
  (begin
    (when (and (integer? base) (integer? size))
      (when (> (+ base size) 64)
        (error "cannot extract fields across 64-bit boundaries"))
      (when (< base 0)
        (error "bitfield base cannot be negative"))
      (when (< size 0)
        (error "bitfield size cannot be negative")))
    ;; TODO does not work for negatives
    (apply (if signed arithmetic-shift bitwise-arithmetic-shift) (list (arithmetic-shift val (- 64 (+ size base))) (neg (- 64 size))))))

(module+ test
  (require rackunit)
  (check-equal? (align 3 4) 4)
  (check-equal? (align 4 4) 4)
  (check-equal? (bit-field int 768 8 2 #f) 3)
  (check-equal? (bit-field int 768 8 2 #t) 3))

;; ==============================================================================
;; Math Macros
;; ==============================================================================

(define-syntax-rule (== a b) (equal? a b))
(define-syntax-rule (!= a b) (not (equal? a b)))

;; ==============================================================================
;; Syntax convert
;; ==============================================================================

(define (number-id->number id)
  (define e (syntax-e id))
  (cond
   ((integer? e)
    e)
   (else
    (let ((x (string->number (symbol->string e))))
      (if (integer? x) x id)))))

(define (numerify v #:mode [mode 'convert])
  (define (numerify-internal x)
    (cond
      [(number? x)     x]
      [(string? x)     (string->number x)]
      [(char? x)       (string->number (string x))]
      [(symbol? x)     (string->number (symbol->string x))]
      [(syntax? x)     (syntax-e x)]
      [else            #f]))
  (define nw (numerify-internal v))
  (if nw
      (if (number? nw)
          nw
          (numerify nw))
      #f))


(define (stringify x #:mode [mode 'convert])
  (cond
    [(string? x)     x]
    [(char? x)       (string x)]
    [(symbol? x)     (symbol->string x)]
    [(number? x)     (number->string x)]
    [(identifier? x) (symbol->string (syntax-e x))]
    [else            #f]))

;; Same as stringify but makes it symbol

(define (symbolyze x)
  (cond
    [(symbol? x)     x]
    [(string? x)     (string->symbol x)]
    [(char? x)       (string->symbol (string x))]
    [(number? x)     (string->symbol (number->string x))]
    [(identifier? x) (syntax-e x)]
    [else            #f]))

(module+ test
  (require rackunit)
  ;; Define the vector 2 structure
  (check-equal? (number-id->number #'1) 1)
  (check-equal? (stringify #'1) #f)
  (check-equal? (stringify #'x) "x"))

;; ==============================================================================
;; Debugging
;; ==============================================================================

(define-syntax-rule (assert expr)
  (when (not expr)
    (error 'assert "assertion failed: ~s" (quote expr))))

;; ==============================================================================
;; Strings
;; ==============================================================================
;;
;;
;;
(define-syntax-rule (string-append! arg str ...)
  (set! arg (string-append arg str ...)))
;;
;; ---------------------------------------------------------------------------------
;; Camelizing
;; ---------------------------------------------------------------------------------
;;
;; Given a string name, perform an UpperCamelCasing of the name.
;;
(define (string->ucamel name)
  (let ((name (value->string name)))
    (apply string-append
           (map string-titlecase (regexp-split #px"[\\s]+"
                                               (regexp-replace* #px"[^\\s\\w]+" name " "))))))
;;
;; convert the name to the lowerCamelCase
;;
(define (string-lcamel name)
  (let* ((name (value->string name))
         (lst (regexp-split #px"[\\s]+"
                           (regexp-replace* #px"[^\\s\\w]+" name " "))))
    (apply string-append
           (cons (string-downcase (car lst)) (map string-titlecase (cdr lst))))))
;;
;;
(define (string->clowercase name)
  (string-join (string-split name "-") "_"))
;;
;; ---------------------------------------------------------------------------------
;; convert string or atom to string or to list
;; ---------------------------------------------------------------------------------

;; conver string or symbol to the string

(define (value->string value)
  (cond
    ((string? value)  value)
    ((number? value)  (number->string value))
    ((symbol? value)  (symbol->string value))
    (else             (error "'value->string' expected symbol or string, found" value))))

;; convert the string or symbol to the list of characters
;; (value->list "123")
;; -> (#\1 #\2 #\3)

(define (value->list value)
  (string->list (value->string value)))

;; ---------------------------------------------------------------------------------
;; String index tools
;; ---------------------------------------------------------------------------------
;;
;; return IDX or null if the idx could not be used for string
;;
;; (string-idx "ABC" 0)  -> 0
;; (string-idx "ABC" 1)  -> 1
;; (string-idx "ABC" 3)  -> ()
;; (string-idx "ABC" -1) -> 2
;; (string-idx "ABC" -2) -> 1
;; (string-idx "ABC" -4) -> ()
;;
(define (string-idx str idx)
  (let ((len (string-length str)))
    (let ((idx (if (< idx 0)
                   (+ len idx)
                   idx)))
        (if (and (< idx len) (>= idx 0))
            idx
            null))))
;;
;; reffer inside string but allow the minus in the index
;;
(define (string-ruby-ref str idx)
  (let ((idx (string-idx str idx)))
    (if (null? idx)
        null
        (string-ref str idx))))
;;
;; substing witch allow minus in the index and reversed order of indeces
;; another difference it includes <end> character!!!
;;
(define (string-ruby-substring str start [end -1])
  (let ((start (string-idx str start))
        (end (string-idx str end)))
    (if (>= end start)
        (substring str start (+ 1 end))
        (list->string (reverse (string->list (substring str end (+ 1 start))))))))
;;
;; make hex string
;;
(define (integer->hex num [width 8])
  (string-append (string-ruby-substring (string-append "00000000" (string-upcase(format "~x" num))) (- 0 width) -1)))
;;
;; make c style hex string
;;
(define (integer->chex num [width 8])
  (string-append "0x" (string-ruby-substring (string-append "00000000" (string-upcase(format "~x" num))) (- 0 width) -1)))
;;
;; ---------------------------------------------------------------------------------
;; split and join
;; ---------------------------------------------------------------------------------
;;
;; Split string to lines
;;
(define (string->lines str)
  (regexp-split  "(\n\r)|(\r\n)|[\n\r]" str))

;; ---------------------------------------------------------------------------------
;; Convert hex string to value (TODO Make it better and faster)
;; ---------------------------------------------------------------------------------

(define (char->hex-digit c)
  ;; Turn a character into a hex digit
  (cond [(char<=? #\0 c #\9)
         (- (char->integer c) (char->integer #\0))]
        [(char<=? #\A c #\F)
         (+ 10 (- (char->integer c) (char->integer #\A)))]
        [(char<=? #\a c #\f)
         (+ 10 (- (char->integer c) (char->integer #\a)))]
        [else
         (error 'char->hex-digit "~A is not a hex character" c)]))

(define (hex-string->int str)
  (let ((stream (open-input-string str))
        (val 0))
    (for ([c (in-input-port-chars stream)])
      (set! val (+ (* val 16) (char->hex-digit c))))
    val))


;; ==============================================================================
;; few helpers for vector
;; ==============================================================================

(define-syntax-rule (vector-at-0 vec) (vector*-ref vec 0))
(define-syntax-rule (vector-at-1 vec) (vector*-ref vec 1))
(define-syntax-rule (vector-at-2 vec) (vector*-ref vec 2))

(define-syntax-rule (vector-at-0! vec exp) (vector*-ref vec 0 exp))
(define-syntax-rule (vector-at-1! vec exp) (vector*-ref vec 1 exp))
(define-syntax-rule (vector-at-2! vec exp) (vector*-ref vec 2 exp))

;; generic function call lambda with the element and index
(define (for/vector-with-index func vec)
  (for ((idx (in-range (vector-length vec))))
    (func (vector-ref vec idx) idx)))

;; generic function to find the index of somthing
;; params: vector lambda-function
(define (for/vector-find-index func vec [idx 0])
  (if (> (vector-length vec) idx)
      (if (func (vector-ref vec idx))
          idx
          (for/vector-find-index func vec (+ idx 1)))
      #f))

;; generic function to find the index of somthing
;; params: vector lambda-function
(define (for/vector-find-item func vec [idx 0])
  (if (> (vector-length vec) idx)
      (if (func (vector-ref vec idx))
          (vector-ref vec idx)
          (for/vector-find-item func vec (+ idx 1)))
      #f))

;; find index of the element
(define (vector-find-value-index vec value [idx 0])
  (for/vector-find-index (lambda (e) (equal? e value)) vec))

;; find index of the element
(define (vector-ruby-index vec idx)
  (define size (vector-length vec))
  (cond
    ((>= idx 0)
     (if (< idx size) idx #f))
    (else
     (define nidx (- size idx))
     (if (and (>= idx 0)
              (< idx size))
         idx #f))))

;; ==============================================================================
;; few helpers for gvector
;; ==============================================================================

;; generic function call lambda with the element and index
(define (for/gvector-with-index func gvec)
  (for ((idx (in-range (gvector-count gvec))))
    (func (gvector-ref gvec idx) idx)))

;; generic function to find the index of somthing
;; params: vector lambda-function
(define (for/gvector-find-index func gvec [idx 0])
  (if (> (gvector-count gvec) idx)
      (if (func (gvector-ref gvec idx))
          idx
          (for/gvector-find-index func gvec (+ idx 1)))
      #f))
;;
;;
(define (for/gvector-find-item func vec [idx 0])
  (if (>= idx (gvector-count vec))
      #f
      (let ((it (gvector-ref vec idx)))
        (if (func it)
            it
            (for/gvector-find-item func vec (+ idx 1))))))

;;
(define (for/gvector-find-item-rev func vec)
  (let :loop: ((idx (- (gvector-count vec) 1)))
    (if (< idx 0)
            #f
            (let ((it (gvector-ref vec idx)))
              (if (func it)
                  it
                  (:loop: func vec (- idx 1)))))))

;; find index of the element
(define (gvector-find-value-index gvec value [idx 0])
  (for/gvector-find-index (lambda (e) (equal? e value)) gvec))

;; Last element of the gvector
(define (gvector-first gv)
  (let ((cnt (gvector-count gv)))
    (if (== cnt 0)
        #f
        (gvector-ref gv (- cnt 1)))))

;; Last element of the gvector
(define (gvector-last gv)
  (let ((cnt (gvector-count gv)))
    (if (== cnt 0)
        #f
        (gvector-ref gv (- cnt 1)))))

(module+ test
  (require rackunit)
  (define gv (make-gvector))
  (gvector-add! gv 1)
  (gvector-add! gv 2)
  (gvector-add! gv 3)
  (let ((it (for/gvector-find-item (lambda (i) (eq? i 2)) gv)))
    (check-equal? it 2))
  )

;; ==============================================================================
;; Conditional
;; ==============================================================================


(define-syntax-rule (move-if-not-zero result value check original)
  (if (!= check 0)
       (set! result value)
       (set! result original)))

;; "dest = src1 < src2 ? 1 : 0 -- Compare as Signed Integers
(define-syntax-rule (set-on-less-than dest src1 src2)
  (if (< src1 src2)
      (set! dest 1)
      (set! dest 0)))

(define INT8_MAX   128)
(define INT8_MIN  -127)
(define INT16_MAX  32767)
(define INT16_MIN  -32768)
(define INT32_MAX  2147483647)
(define INT32_MIN -2147483648)

(define UINT8_MAX  255)
(define UINT16_MAX 65535)
(define UINT32_MAX 4294967295)

;; helper
(define-syntax-rule (integer-fits? in size is-signed)
  (cond
    ((== 1 size)
     (if is-signed
         (and (>= in INT8_MIN) (<= in INT8_MAX))
         (and (>= in 0) (<= in UINT8_MAX))))
    ((== 2 size)
     (if is-signed
         (and (>= in INT16_MIN) (<= in INT16_MAX))
         (and (>= in 0) (<= in UINT16_MAX))))
    ((== 4 size)
     (if is-signed
         (and (>= in INT32_MIN) (<= in INT32_MAX))
         (and (>= in 0) (<= in UINT32_MAX))))
    ((== 8 size)
     true)
    (else
     (assert false))))

;; ==============================================================================
;; Copy structures
;; ==============================================================================

;; Usage (type-copy child-struct (paretn-struct args ...) child-args)

(define (type-copy new-type base . args)
  (apply new-type (append (struct->list base) args)))


;; ==============================================================================
;; Iterators
;; ==============================================================================

(define (for-each-in-list lst func)
  (for ((it (in-list lst))) (func it)))

;; ==============================================================================
;; File helper
;; ==============================================================================

(define (for-file-parse filename)
  ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ;; Read all file helper
  (define read-all
    (case-lambda
      [() (read-all (current-input-port))]
      [(iport)
       (port-count-lines! iport)
       (let loop ()
         (let ([next (read-syntax filename iport)])
           (if (eof-object? next)
               null
               (cons next (loop)))))]))
  ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  (with-input-from-file filename
    read-all))

;; ==============================================================================
;; Struct helpers
;; ==============================================================================
;; ----------------------------------------------------------------------------
;;
;; DEFINE WITH STRUCTURE FIELDS
;;
;; The main purpose - expand struct fields into local environment
;; definition. It allow to expand several structures with
;; prefix.
;;
;; (define-struct v3 (x y z))
;; (define va (v3 1 2 3))
;; (define vb (v3 4 5 6))
;;
;; (define/contract (v3-compare-xy this other)
;;   (define-with-struct lhs. (v3 x y) this)
;;   (define-with-struct rhs. (v3 x y) other)
;;   (and (equal? lhs.x rhs.x)
;;        (equal? lhs.y rhs.y)))
;;
;; ----------------------------------------------------------------------------

(define-syntax (define-with-struct stx)
  (syntax-parse stx
    [(_ (type:id arg0:id ...) obj)
     (define get-args (for/list ([arg (syntax->list #'(arg0 ...))]) (list arg (format-id stx "~a-~a" #'type arg))))
     (with-syntax ((((n get-arg) ...) get-args))
       (syntax/loc stx
         (begin (define n (get-arg obj)) ...)))]
    [(_ prefix:id (type:id arg0:id ...) obj)
     (define get-args (for/list ([arg (syntax->list #'(arg0 ...))]) (list (format-id stx "~a~a" #'prefix arg) (format-id stx "~a-~a" #'type arg))))
     (with-syntax ((((n get-arg) ...) get-args))
       (syntax/loc stx
         (begin (define n (get-arg obj)) ...)))]))

(module+ test
  (require rackunit)
  ;; Define the vector 2 structure
  (define-struct v2 (x y) #:transparent #:mutable)
  ;; Define the quaternion structure
  (define-struct (quat v2) (z w) #:transparent #:mutable)
  ;; Define quaternion value
  (let ((q (quat 1 2 3 4)))
    ;; Define fields of quaternion
    (define-with-struct (quat z w) q)
    (check-equal? z 3)
    (check-equal? w 4)
    ;; Define fields of quaternion with prefix
    (define-with-struct a. (quat z w) q)
    (check-equal? a.z 3)
    (check-equal? a.w 4)
    (define (foo)
      (define-with-struct f. (v2 x y) (v2 1 2)) (+ f.x f.y))
    (check-equal? (foo) 3)))


;; ----------------------------------------------------------------------------
;;
;; DEFINE WITH STRUCTURE FIELDS GETTERS AND SETTER
;;
;; The main purpose - expand struct fields into getters and setters
;;
;; (define-struct v3 (x y z))
;; (define v (v3 1 2 3))
;;
;; (define-with-struct-methods (v3 x y))
;; (set-x! v 10)
;; (x v) ;; -> 10
;;
;; (define-with-struct m- (v3 x y))
;; (set-m-x! v 10)
;; (m-x v) ;; -> 10
;;
;; ----------------------------------------------------------------------------

(define-syntax (define-with-struct-methods stx)
  (syntax-parse stx
    [(_ (type:id arg0:id ...) )
     (define get-args (for/list ([arg (syntax->list #'(arg0 ...))])
                        (list arg
                              (format-id stx "set-~a!" arg)
                              (format-id stx "~a-~a" #'type arg)
                              (format-id stx "set-~a-~a!" #'type arg))))
     (with-syntax ((((getter setter get-arg set-arg) ...) get-args))
       (syntax/loc stx
         (begin
           (begin (define (getter obj) (get-arg obj))
                  (define (setter obj val) (set-arg obj val))) ...)))]
    [(_ prefix:id (type:id arg0:id ...))
     (define get-args (for/list ([arg (syntax->list #'(arg0 ...))])
                        (list
                         (format-id stx "~a~a" #'prefix arg)
                         (format-id stx "set-~a~a!" #'prefix arg)
                         (format-id stx "~a-~a" #'type arg)
                         (format-id stx "set-~a-~a!" #'type arg))))
     (with-syntax ((((getter setter get-arg set-arg) ...) get-args))
       (syntax/loc stx
         (begin
           (begin
             (define (getter obj) (get-arg obj))
             (define (setter obj val) (set-arg obj val))) ...)))]))

(module+ test
  (require rackunit)
  (let ((q (quat 1 2 3 4)))
    ;; Define fields of quaternion
    (define-with-struct-methods (quat z w) )
    (check-equal? (z q) 3)
    (check-equal? (w q) 4)
    (set-z! q 2)
    (check-equal? (z q) 2)
    ;; Define fields of quaternion with prefix
    (define-with-struct-methods a- (quat z w))
    (check-equal? (a-z q) 2)
    (check-equal? (a-w q) 4)
    (set-a-w! q 5)
    (check-equal? (a-w q) 5)))
