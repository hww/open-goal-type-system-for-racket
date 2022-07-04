#lang racket/base

(require racket/contract racket/pretty)
(require "interfaces.rkt" "type-system.rkt" "type-spec.rkt" "type.rkt" "basic-types.rkt" "../vmc-lib.rkt")

(provide add-builtin-types)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Builtin Type Structures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Add a simple structure type - don't use this outside of add_builtin_types as
;; it forces you to do things in the wrong order.

(define/contract (add-builtin-structure  this parent type-name (boxed #f))
(->* (type-system? symbol? symbol?) (boolean?) struct-type?)
  (add-type this type-name (struct-type-new parent type-name boxed false false 0))
  (get-type-of-type this struct-type? type-name))


;; Add a simple basic type - don't use this outside of add-builtin-types as it
;; forces you to do things in the wrong order.

(define/contract (add-builtin-basic this parent type-name)
  (-> type-system? symbol? symbol? basic-type?)
  (add-type this type-name (basic-type-new parent type-name false 0))
  (get-type-of-type this basic-type? type-name))

;; Add a simple value type - don't use this outside of add-builtin-types as it
;; forces you to do things in the wrong order.

(define/contract (add-builtin-value-type this parent type-name size (boxed #f) (sign-extend #f) (reg-kind RegClass::GPR-64))
  (->* (type-system? symbol? symbol? integer?) (boolean? boolean? integer?) void)
  (add-type this type-name (value-type-new parent type-name boxed size sign-extend reg-kind))
  (get-type-of-type this value-type? type-name))

;; Helper for inheritance of structure types when setting up builtin types.

(define/contract (builtin-structure-inherit this st)
  (-> type-system? struct-type? void)
  (struct-type-inherit st (get-type-of-type this struct-type? (type-get-parent st))))

;; Add types which are built-in to GOAL.

(define (add-builtin-types this)
  (-> type-system? void?)

  (define obj-type (types-find this 'object))
  ;; some of the basic types have confusing circular dependencies so this is
  ;; done manually. there are no inlined things so its ok to do some things out
  ;; of order because the actual size doesn't really matter.


  (define structure-type   (add-builtin-structure this 'object 'structure))
  (define basic-type       (add-builtin-basic this 'structure 'basic))
  (define symbol-type      (add-builtin-basic this 'basic 'symbol))
  (define type-type        (add-builtin-basic this 'basic 'type))
  (define string-type      (add-builtin-basic this 'basic 'string))

  (basic-type-set-final string-type)  ;; no virtual calls used on string.
  (define function-type    (add-builtin-basic this 'basic 'function))
  (define vu-function-type (add-builtin-structure this 'structure 'vu-function))
  (define link-block-type  (add-builtin-basic this 'basic 'link-block))
  (define kheap-type       (add-builtin-structure this 'structure 'kheap))
  (define array-type       (add-builtin-basic this 'basic 'array))
  (define pair-type        (add-builtin-structure this 'object 'pair true))
  (define connectable-type (add-builtin-structure this 'structure 'connectable))
  (define file-stream-type (add-builtin-basic this 'basic 'file-stream))
  (add-builtin-value-type this 'object 'pointer 4)

  (define inline-array-type (add-builtin-value-type this 'object 'inline-array 4))
  (type-set-runtime-type  inline-array-type 'pointer)

  (add-builtin-value-type this 'object 'number 8)  ;; sign extend?
  (add-builtin-value-type this 'number 'float 4 false false RegClass::FLOAT)
  (add-builtin-value-type this 'number 'integer 8 false false)   ;; sign extend?
  (add-builtin-value-type this 'integer 'binteger 8 true false)  ;; sign extend?
  (add-builtin-value-type this 'integer 'sinteger 8 false true)
  (add-builtin-value-type this 'sinteger 'int8 1 false true)
  (add-builtin-value-type this 'sinteger 'int16 2 false true)
  (add-builtin-value-type this 'sinteger 'int32 4 false true)
  (add-builtin-value-type this 'sinteger 'int64 8 false true)
  (add-builtin-value-type this 'sinteger 'int128 16 false true RegClass::INT-128)
  (add-builtin-value-type this 'integer 'uinteger 8)
  (add-builtin-value-type this 'uinteger 'uint8 1)
  (add-builtin-value-type this 'uinteger 'uint16 2)
  (add-builtin-value-type this 'uinteger 'uint32 4)
  (add-builtin-value-type this 'uinteger 'uint64 8)
  (add-builtin-value-type this 'uinteger 'uint128 16 false false RegClass::INT-128)

  ;; (add special units types.
  (define meters (add-builtin-value-type this 'float 'meters 4 false false RegClass::FLOAT))
  (type-set-runtime-type meters 'float)
  (define degrees (add-builtin-value-type this 'float 'degrees 4 false false RegClass::FLOAT))
  (type-set-runtime-type degrees 'float)
  (define seconds (add-builtin-value-type this 'int64 'seconds 8 false true))
  (type-set-runtime-type seconds'int64)

  (define int-type (add-builtin-value-type this 'integer 'int 8 false true))
  (type-disallow-in-runtime int-type)
  (define uint-type (add-builtin-value-type this 'uinteger 'uint 8 false false))
  (type-disallow-in-runtime uint-type)

  ;; Methods and Fields
  (forward-declare-type-as this 'memory-usage-block 'basic)

  ;; OBJECT
  (declare-method this obj-type 'new false (make-function-typespec this '(symbol type int) '_type_) false)
  (declare-method this obj-type 'delete false (make-function-typespec this '(_type_) 'none) false)
  (declare-method this obj-type 'print false (make-function-typespec this '(_type_) '_type_) false)
  (declare-method this obj-type 'inspect false (make-function-typespec this '(_type_) '_type_) false)
  (declare-method this obj-type 'length false (make-function-typespec this '(_type_) 'int) false)  ;; todo - this integer type?
  (declare-method this obj-type 'asize-of false (make-function-typespec this '(_type_) 'int) false)
  (declare-method this obj-type 'copy false (make-function-typespec this '(_type_ symbol) '_type_) false)
  (declare-method this obj-type 'relocate false (make-function-typespec this '(_type_ int) '_type_) false)
  (declare-method this obj-type 'mem-usage false (make-function-typespec this '(_type_ memory-usage-block int) '_type_) false)

  ;; STRUCTURE
  ;; structure new doesn't support dynamic sizing which is kinda weird - it grabs the size from
  ;; the type.  Dynamic structures use new-dynamic-structure which is used exactly once ever.
  (declare-method this structure-type 'new false (make-function-typespec this '(symbol type) '_type_) false)
  ;; structure-type is a field-less StructureType so we have to do this to match the runtime.
  ;;  structure_type_>override-size-in-memory(4)

  ;; BASIC
  ;; we intentionally don't inherit from structure because structure's size is weird.
   (add-field-to-type this basic-type 'type (make-typespec this 'type))
  ;; the default new basic doesn't support dynamic sizing. anything dynamic will override this
  ;; and then call (method object new) to do the dynamically-sized allocation.
  (declare-method this basic-type 'new false (make-function-typespec this '(symbol type) '_type_) false)

  ;; SYMBOL
  (builtin-structure-inherit this symbol-type)
  (add-field-to-type this symbol-type 'value (make-typespec this 'object))
  ;; a new method which returns type none means new is illegal.
  (declare-method this symbol-type 'new false (make-function-typespec this '() 'none) false)

  ;; TYPE
  (builtin-structure-inherit this type-type)
  (declare-method this type-type 'new false (make-function-typespec this '(symbol type int) '_type_) false)
  (add-field-to-type this type-type 'symbol (make-typespec this 'symbol))
  (add-field-to-type this type-type 'parent (make-typespec this 'type))
  (add-field-to-type this type-type 'size (make-typespec this 'uint16))  ;; actually u16
  (add-field-to-type this type-type 'psize (make-typespec this 'uint16))  ;; todo u16 or s16. what really is this?
  (add-field-to-type this type-type 'heap-base (make-typespec this 'uint16))         ;; todo
  (add-field-to-type this type-type 'allocated-length (make-typespec this 'uint16))  ;; todo
  (add-field-to-type this type-type 'method-table (make-typespec this 'function) false true)

  ;; STRING

  (builtin-structure-inherit this string-type)
  (add-field-to-type this string-type 'allocated-length (make-typespec this 'int32))   ;; todo integer type
  (add-field-to-type this string-type 'data (make-typespec this 'uint8) false true)  ;; todo integer type
  ;; string is never deftype'd for the decompiler so we need to manually give the constructor
  ;; type here.
  (declare-method this string-type 'new false (make-function-typespec this '(symbol type int string) '_type_) false)

  ;; FUNCTION
  (builtin-structure-inherit this function-type)
  ;; ???

  ;; VU FUNCTION
  ;; don't inherit
  (add-field-to-type this vu-function-type 'length (make-typespec this 'int32))   ;; todo integer type
  (add-field-to-type this vu-function-type 'origin (make-typespec this 'int32))   ;; todo sign extend?
  (add-field-to-type this vu-function-type 'qlength (make-typespec this 'int32))  ;; todo integer type
  (add-field-to-type this vu-function-type 'data (make-typespec this 'uint8) false true)

  ;; link block
  (builtin-structure-inherit this link-block-type)
  (add-field-to-type this link-block-type 'allocated-length (make-typespec this 'int32))  ;; todo integer type
  (add-field-to-type this link-block-type 'version (make-typespec this 'int32))  ;; todo integer type
  ;; there's probably some dynamically sized stuff after this...

  ;; kheap
  (add-field-to-type this kheap-type 'base (make-typespec this 'pointer))
  (add-field-to-type this kheap-type 'top (make-typespec this 'pointer))
  (add-field-to-type this kheap-type 'current (make-typespec this 'pointer))
  (add-field-to-type this kheap-type 'top-base (make-typespec this 'pointer))

  ;; todo
  (builtin-structure-inherit this array-type)
  (declare-method this array-type 'new false (make-function-typespec this '(symbol type type int) '_type_) false)
  ;; array has: number number type
  (add-field-to-type this array-type 'length (make-typespec this 'int32))
  (add-field-to-type this array-type 'allocated-length (make-typespec this 'int32))
  (add-field-to-type this array-type 'content-type (make-typespec this 'type))
  (add-field-to-type this array-type 'data (make-typespec this 'uint8) false true)

  ;; pair
  (struct-type-override-offset pair-type 2)
  (declare-method this pair-type 'new false (make-function-typespec this '(symbol type object object) '_type_) false)
  (add-field-to-type this pair-type 'car (make-typespec this 'object))
  (add-field-to-type this pair-type 'cdr (make-typespec this 'object))

  ;; this type is very strange as the compiler knows about it in gkernel-h yet it is
  ;; defined inside of connect.
  (add-field-to-type this connectable-type 'next0 (make-typespec this 'connectable))
  (add-field-to-type this connectable-type 'prev0 (make-typespec this 'connectable))
  (add-field-to-type this connectable-type 'next1 (make-typespec this 'connectable))
  (add-field-to-type this connectable-type 'prev1 (make-typespec this 'connectable))

  ;; todo
  (builtin-structure-inherit this file-stream-type)
  (add-field-to-type this file-stream-type 'flags (make-typespec this 'uint32))
  (add-field-to-type this file-stream-type 'mode (make-typespec this 'symbol))
  (add-field-to-type this file-stream-type 'name (make-typespec this 'string))
  (add-field-to-type this file-stream-type 'file (make-typespec this 'uint32))
  (declare-method this file-stream-type 'new false
                  (make-function-typespec this '(symbol type string symbol) '_type_) false)
  "Builtings Initialized"
  )

;; Type spec text--------------------------------------------------------

(module+ test
  (require rackunit)
  (let ((this (type-system-new)))
    (add-builtin-types this)
    (display (inspect-all-type-information this))))

#|

Here is result of open goal project

_varargs_
[ValueType] meters
 parent: float
 boxed: false
 size: 4
 sext: false
 register: float

_type_
[ValueType] uint8
 parent: uinteger
 boxed: false
 size: 1
 sext: false
 register: gpr64

[StructureType] vu-function
 parent: structure
 boxed: false
 dynamic: true
 size: 12
 pack: false
 misalign: false
 heap-base: 0
 stack-singleton: false
 fields:
   Field: (length int32 :offset 0) inline: false, dynamic: false, array: false, array size   0, align  4, skip false
   Field: (origin int32 :offset 4) inline: false, dynamic: false, array: false, array size   0, align  4, skip false
   Field: (qlength int32 :offset 8) inline: false, dynamic: false, array: false, array size   0, align  4, skip false
   Field: (data uint8 :offset 12) inline: false, dynamic: true , array: true , array size   0, align  1, skip false
 methods:

none
[ValueType] uint128
 parent: uinteger
 boxed: false
 size: 16
 sext: false
 register: int128

[ValueType] object
 parent: object
 boxed: false
 size: 4
 sext: false
 register: gpr64
  Method   0: new                  (function symbol type int _type_)
  Method   1: delete               (function _type_ none)
  Method   2: print                (function _type_ _type_)
  Method   3: inspect              (function _type_ _type_)
  Method   4: length               (function _type_ int)
  Method   5: asize-of             (function _type_ int)
  Method   6: copy                 (function _type_ symbol _type_)
  Method   7: relocate             (function _type_ int _type_)
  Method   8: mem-usage            (function _type_ memory-usage-block int _type_)

[StructureType] structure
 parent: object
 boxed: false
 dynamic: false
 size: 0
 pack: false
 misalign: false
 heap-base: 0
 stack-singleton: false
 fields:
 methods:
  Method   0: new                  (function symbol type _type_)

[BasicType] type
 parent: basic
 dynamic: true
 size: 20
 heap-base: 0
 fields:
   Field: (type type :offset 0) inline: false, dynamic: false, array: false, array size   0, align  4, skip false
   Field: (symbol symbol :offset 4) inline: false, dynamic: false, array: false, array size   0, align  4, skip false
   Field: (parent type :offset 8) inline: false, dynamic: false, array: false, array size   0, align  4, skip false
   Field: (size uint16 :offset 12) inline: false, dynamic: false, array: false, array size   0, align  2, skip false
   Field: (psize uint16 :offset 14) inline: false, dynamic: false, array: false, array size   0, align  2, skip false
   Field: (heap-base uint16 :offset 16) inline: false, dynamic: false, array: false, array size   0, align  2, skip false
   Field: (allocated-length uint16 :offset 18) inline: false, dynamic: false, array: false, array size   0, align  2, skip false
   Field: (method-table function :offset 20) inline: false, dynamic: true , array: true , array size   0, align  4, skip false
 methods:
  Method   0: new                  (function symbol type int _type_)

[BasicType] basic
 parent: structure
 dynamic: false
 size: 4
 heap-base: 0
 fields:
   Field: (type type :offset 0) inline: false, dynamic: false, array: false, array size   0, align  4, skip false
 methods:
  Method   0: new                  (function symbol type _type_)

[BasicType] symbol
 parent: basic
 dynamic: false
 size: 8
 heap-base: 0
 fields:
   Field: (type type :offset 0) inline: false, dynamic: false, array: false, array size   0, align  4, skip false
   Field: (value object :offset 4) inline: false, dynamic: false, array: false, array size   0, align  4, skip false
 methods:
  Method   0: new                  (function none)

[BasicType] string
 parent: basic
 dynamic: true
 size: 8
 heap-base: 0
 fields:
   Field: (type type :offset 0) inline: false, dynamic: false, array: false, array size   0, align  4, skip false
   Field: (allocated-length int32 :offset 4) inline: false, dynamic: false, array: false, array size   0, align  4, skip false
   Field: (data uint8 :offset 8) inline: false, dynamic: true , array: true , array size   0, align  1, skip false
 methods:
  Method   0: new                  (function symbol type int string _type_)

[StructureType] connectable
 parent: structure
 boxed: false
 dynamic: false
 size: 16
 pack: false
 misalign: false
 heap-base: 0
 stack-singleton: false
 fields:
   Field: (next0 connectable :offset 0) inline: false, dynamic: false, array: false, array size   0, align  4, skip false
   Field: (prev0 connectable :offset 4) inline: false, dynamic: false, array: false, array size   0, align  4, skip false
   Field: (next1 connectable :offset 8) inline: false, dynamic: false, array: false, array size   0, align  4, skip false
   Field: (prev1 connectable :offset 12) inline: false, dynamic: false, array: false, array size   0, align  4, skip false
 methods:

[BasicType] function
 parent: basic
 dynamic: false
 size: 4
 heap-base: 0
 fields:
   Field: (type type :offset 0) inline: false, dynamic: false, array: false, array size   0, align  4, skip false
 methods:

[ValueType] binteger
 parent: integer
 boxed: true
 size: 8
 sext: false
 register: gpr64

[ValueType] integer
 parent: number
 boxed: false
 size: 8
 sext: false
 register: gpr64

[BasicType] link-block
 parent: basic
 dynamic: false
 size: 12
 heap-base: 0
 fields:
   Field: (type type :offset 0) inline: false, dynamic: false, array: false, array size   0, align  4, skip false
   Field: (allocated-length int32 :offset 4) inline: false, dynamic: false, array: false, array size   0, align  4, skip false
   Field: (version int32 :offset 8) inline: false, dynamic: false, array: false, array size   0, align  4, skip false
 methods:

[StructureType] kheap
 parent: structure
 boxed: false
 dynamic: false
 size: 16
 pack: false
 misalign: false
 heap-base: 0
 stack-singleton: false
 fields:
   Field: (base pointer :offset 0) inline: false, dynamic: false, array: false, array size   0, align  4, skip false
   Field: (top pointer :offset 4) inline: false, dynamic: false, array: false, array size   0, align  4, skip false
   Field: (current pointer :offset 8) inline: false, dynamic: false, array: false, array size   0, align  4, skip false
   Field: (top-base pointer :offset 12) inline: false, dynamic: false, array: false, array size   0, align  4, skip false
 methods:

[BasicType] array
 parent: basic
 dynamic: true
 size: 16
 heap-base: 0
 fields:
   Field: (type type :offset 0) inline: false, dynamic: false, array: false, array size   0, align  4, skip false
   Field: (length int32 :offset 4) inline: false, dynamic: false, array: false, array size   0, align  4, skip false
   Field: (allocated-length int32 :offset 8) inline: false, dynamic: false, array: false, array size   0, align  4, skip false
   Field: (content-type type :offset 12) inline: false, dynamic: false, array: false, array size   0, align  4, skip false
   Field: (data uint8 :offset 16) inline: false, dynamic: true , array: true , array size   0, align  1, skip false
 methods:
  Method   0: new                  (function symbol type type int _type_)

[ValueType] int16
 parent: sinteger
 boxed: false
 size: 2
 sext: true
 register: gpr64

[StructureType] pair
 parent: object
 boxed: true
 dynamic: false
 size: 8
 pack: false
 misalign: false
 heap-base: 0
 stack-singleton: false
 fields:
   Field: (car object :offset 0) inline: false, dynamic: false, array: false, array size   0, align  4, skip false
   Field: (cdr object :offset 4) inline: false, dynamic: false, array: false, array size   0, align  4, skip false
 methods:
  Method   0: new                  (function symbol type object object _type_)

[BasicType] file-stream
 parent: basic
 dynamic: false
 size: 20
 heap-base: 0
 fields:
   Field: (type type :offset 0) inline: false, dynamic: false, array: false, array size   0, align  4, skip false
   Field: (flags uint32 :offset 4) inline: false, dynamic: false, array: false, array size   0, align  4, skip false
   Field: (mode symbol :offset 8) inline: false, dynamic: false, array: false, array size   0, align  4, skip false
   Field: (name string :offset 12) inline: false, dynamic: false, array: false, array size   0, align  4, skip false
   Field: (file uint32 :offset 16) inline: false, dynamic: false, array: false, array size   0, align  4, skip false
 methods:
  Method   0: new                  (function symbol type string symbol _type_)

[ValueType] seconds
 parent: int64
 boxed: false
 size: 8
 sext: true
 register: gpr64

[ValueType] pointer
 parent: object
 boxed: false
 size: 4
 sext: false
 register: gpr64

[ValueType] inline-array
 parent: object
 boxed: false
 size: 4
 sext: false
 register: gpr64

[ValueType] number
 parent: object
 boxed: false
 size: 8
 sext: false
 register: gpr64

[ValueType] float
 parent: number
 boxed: false
 size: 4
 sext: false
 register: float

[ValueType] sinteger
 parent: integer
 boxed: false
 size: 8
 sext: true
 register: gpr64

[ValueType] uint16
 parent: uinteger
 boxed: false
 size: 2
 sext: false
 register: gpr64

[ValueType] int8
 parent: sinteger
 boxed: false
 size: 1
 sext: true
 register: gpr64

[ValueType] int32
 parent: sinteger
 boxed: false
 size: 4
 sext: true
 register: gpr64

[ValueType] int64
 parent: sinteger
 boxed: false
 size: 8
 sext: true
 register: gpr64

[ValueType] int128
 parent: sinteger
 boxed: false
 size: 16
 sext: true
 register: int128

[ValueType] uinteger
 parent: integer
 boxed: false
 size: 8
 sext: false
 register: gpr64

[ValueType] uint32
 parent: uinteger
 boxed: false
 size: 4
 sext: false
 register: gpr64

[ValueType] uint64
 parent: uinteger
 boxed: false
 size: 8
 sext: false
 register: gpr64

[ValueType] degrees
 parent: float
 boxed: false
 size: 4
 sext: false
 register: float

[ValueType] int
 parent: integer
 boxed: false
 size: 8
 sext: true
 register: gpr64

[ValueType] uint
 parent: uinteger
 boxed: false
 size: 8
 sext: false
 register: gpr64

|#
