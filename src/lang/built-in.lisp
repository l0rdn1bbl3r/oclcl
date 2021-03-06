#|
  This file is a part of oclcl project.
  Copyright (c) 2012 Masayuki Takagi (kamonama@gmail.com)
                2015 gos-k (mag4.elan@gmail.com)
|#

(in-package :cl-user)
(defpackage oclcl.lang.built-in
  (:use :cl
   :oclcl.lang.type
        :oclcl.lang.data)
  (:export
   ;; address-of operator
   :pointer

   ;; shift operators
   :<<
   :>>
   ;; Built-in work-item functions
   :get-work-dim
   :get-global-size
   :get-global-id
   :get-local-size
   :get-local-id
   :get-num-groups
   :get-group-id
   :get-global-offset

   ;; Built-in type casting
   :to-uchar
   :to-char
   :to-uint
   :to-int
   :to-ulong
   :to-long
   :to-float
   :to-double
   :to-bool

   ;; Reinterpreting Types
   :as-uchar2
   :as-uchar4
   
   ;; Built-in math functions
   :acos
   :acosh
   :acospi
   :asin
   :asinh
   :asinpi
   :atan
   :atan2
   :cbrt
   :ceil
   :copysign
   :cos
   :cosh
   :cospi
   :erfc
   :erf
   :exp
   :exp2
   :exp10
   :expm1
   :fabs
   :fdim
   :floor
   :fma
   :fmax
   :fmin
   :fmod
   :fract
   :frexp
   :hypot
   :ilogb
   :ldexp
   :lgamma
   :lgamma_r
   :log
   :log2
   :log10
   :log1p
   :logb
   :mad
   :maxmag
   :minmag
   :modf
   :nan
   :nextafter
   :pow
   :pown
   :powr
   :remainder
   :remquo
   :rint
   :rootn
   :round
   :rsqrt
   :sin
   :sincos
   :sinh
   :sinpi
   :sqrt
   :tan
   :tanh
   :tanpi
   :tgamma
   :trunc

   :half-cos
   :half-divide
   :half-exp
   :half-exp2
   :half-exp10
   :half-log
   :half-log2
   :half-log10
   :half-powr
   :half-recip
   :half-rsqrt
   :half-sin
   :half-sqrt
   :half-tan

   :native-cos
   :native-divide
   :native-exp
   :native-exp2
   :native-exp10
   :native-log
   :native-log2
   :native-log10
   :native-powr
   :native-recip
   :native-rsqrt
   :native-sin
   :native-sqrt
   :native-tan

   :abs
   :abs-diff
   :add-sat
   :hadd
   :rhadd
   :clamp
   :clz
   :mad-hi
   :mad-sat
   :max
   :min
   :mul-hi
   :rotate
   :sub-sat
   :upsample
   :popcount

   :clamp
   :degrees
   :max
   :min
   :mix
   :radians
   :step
   :smoothstep
   :sign

   :cross
   :dot
   :distance
   :length
   :normalize
   :fast-distance
   :fast-length
   :fast-normalize
           
   :barrier

   :mem-fence
   :read-mem-fence
   :write-mem-fence

   :atomic-add
   :atomic-sub
   :atomic-xchg
   :atomic-inc
   :atomic-dec
   :atomic-cmpxchg
   :atomic-min
   :atomic-max
   :atomic-and
   :atomic-or
   :atomic-xor

   :vec-step
   :shuffle

   :printf

   ;; Interfaces
   :built-in-function-return-type
   :built-in-function-infix-p
   :built-in-function-c-name
   :vector-literal-p

   ;; Vector type accessors for 8 and 16 elts long vector types
   :s0
   :s1
   :s2
   :s4
   :s5
   :s6
   :s7
   :s8
   :s9
   :s10
   :s11
   :s12
   :s13
   :s14
   :s15))
(in-package :oclcl.lang.built-in)

(defparameter +scalar-signed-integer-types+ '(char short int long))
(defparameter +scalar-unsigned-integer-types+ '(uchar ushort uint ulong))
(defparameter +scalar-integer-types+ (append +scalar-signed-integer-types+
                                             +scalar-unsigned-integer-types+))
(defparameter +scalar-float-types+ '(float double))
(defparameter +scalar-number-types+ (append +scalar-integer-types+
                                            +scalar-float-types+))

(defparameter +integer-result-types+ '(char char short short int int long long))
(defparameter +float-result-types+ '(int long))
(defparameter +result-gentypes+ (append +integer-result-types+
                                        +float-result-types+))

(defparameter +vector-char-types+ '(char2 char3 char4 char8 char16))
(defparameter +vector-short-types+ '(short2 short3 short4 short8 short16))
(defparameter +vector-int-types+ '(int2 int3 int4 int8 int16))
(defparameter +vector-long-types+ '(long2 long3 long4 long8 long16))
(defparameter +vector-signed-integer-types+ (append +vector-char-types+
                                                    +vector-short-types+
                                                    +vector-int-types+
                                                    +vector-long-types+))

(defparameter +vector-uchar-types+ '(uchar2 uchar3 uchar4 uchar8 uchar16))
(defparameter +vector-ushort-types+ '(ushort2 ushort3 ushort4 ushort8 ushort16))
(defparameter +vector-uint-types+ '(uint2 uint3 uint4 uint8 uint16))
(defparameter +vector-ulong-types+ '(ulong2 ulong3 ulong4 ulong8 ulong16))
(defparameter +vector-unsigned-integer-types+ (append +vector-uchar-types+
                                                      +vector-ushort-types+
                                                      +vector-uint-types+
                                                      +vector-ulong-types+))

(defparameter +vector-integer-types+ (append +vector-signed-integer-types+
                                             +vector-unsigned-integer-types+))
(defparameter +vector-single-float-types+ '(float2 float3 float4 float8 float16))
(defparameter +vector-double-float-types+ '(double2 double3 double4 double8 double16))
(defparameter +vector-float-types+ (append +vector-single-float-types+
                                           +vector-double-float-types+))
(defparameter +vector-number-types+ (append +vector-integer-types+
                                            +vector-float-types+))

(defparameter +all-char-types+ (append (list 'char)
                                       +vector-char-types+))
(defparameter +all-short-types+ (append (list 'short)
                                        +vector-short-types+))
(defparameter +all-int-types+ (append (list 'int)
                                      +vector-int-types+))
(defparameter +all-long-types+ (append (list 'long)
                                       +vector-long-types+))
(defparameter +all-uchar-types+ (append (list 'uchar)
                                        +vector-uchar-types+))
(defparameter +all-ushort-types+ (append (list 'ushort)
                                         +vector-ushort-types+))
(defparameter +all-uint-types+ (append (list 'uint)
                                       +vector-uint-types+))
(defparameter +all-ulong-types+ (append (list 'ulong)
                                        +vector-ulong-types+))

(defparameter +all-signed-types+ (append +all-char-types+
                                         +all-short-types+
                                         +all-int-types+
                                         +all-long-types+))
(defparameter +all-unsigned-types+ (append +all-uchar-types+
                                           +all-ushort-types+
                                           +all-uint-types+
                                           +all-ulong-types+))

(defparameter +all-single-float-types+ (append (list 'float)
                                               +vector-single-float-types+))
(defparameter +all-double-float-types+ (append (list 'double)
                                               +vector-double-float-types+))

(defparameter +number-types+ (append +scalar-number-types+
                                     +vector-number-types+))

(defun generate-vector-type-symbols (scalar-type)
  (loop for n in '("2" "3" "4" "8" "16")
        collecting (intern (concatenate 'string (symbol-name scalar-type) n))))

(defun same-type-function (function size type infix)
  (loop for type-symbol in (cons type (generate-vector-type-symbols type))
        collecting (list (make-list size :initial-element type-symbol) type-symbol infix function)))

(defun types-function (function size types)
  (loop for type in types
        appending (same-type-function function size type nil)))

(defun float-types-unary-function (function)
  (types-function function 1 +scalar-float-types+))

(defun float-types-binary-function (function)
  (types-function function 2 +scalar-float-types+))

(defun float-types-ternary-function (function)
  (types-function function 3 +scalar-float-types+))

(defun integer-types-unary-function (function)
  (types-function function 1 +scalar-integer-types+))

(defun integer-types-binary-function (function)
  (types-function function 2 +scalar-integer-types+))

(defun integer-types-ternary-function (function)
  (types-function function 3 +scalar-integer-types+))

(defun signed-integer-types-unary-function (function)
  (types-function function 1 +scalar-signed-integer-types+))

(defun same-types-binary-operator (operator types)
  (loop for type in types
        appending (same-type-function operator 2 type t)))

(defun scalar-vector-binary-operator (operator scalar-type)
  (loop for vector-type in (generate-vector-type-symbols scalar-type)
        collecting (list (list scalar-type vector-type) vector-type t operator)))

(defun vector-scalar-binary-operator (operator scalar-type)
  (loop for vector-type in (generate-vector-type-symbols scalar-type)
        collecting (list (list vector-type scalar-type) vector-type t operator)))

(defun integer-vector-scalar-binary-operator (operator)
  (loop for type in +scalar-integer-types+
        collecting (vector-scalar-binary-operator operator type)))

(defun arithmetic-binary-operator (operator types)
  (loop for type in types
        appending (same-type-function operator 2 type t)
        appending (scalar-vector-binary-operator operator type)
        appending (vector-scalar-binary-operator operator type)))

(defun vector-relational-operator (operator argument-type result-type)
  (loop for n in '("2" "3" "4" "8" "16")
        for argument-vector-type = (intern (concatenate 'string (symbol-name argument-type) n))
        for result-vector-type = (intern (concatenate 'string (symbol-name result-type) n))
        collecting (list (list argument-type argument-vector-type) result-vector-type t operator)
        collecting (list (list argument-vector-type argument-type) result-vector-type t operator)
        collecting (list (list argument-vector-type argument-vector-type) result-vector-type t operator)))

(defun relational-operator (operator)
  (loop for argument-type in +scalar-number-types+
        for result-type in +result-gentypes+
        ;; spec is 'int but ...
        collecting (list (list argument-type argument-type) 'bool t operator)
        appending (vector-relational-operator operator argument-type result-type)))

(defun allow-any-types (_)
  (declare (ignore _))
  t)

;;;
;;; Built-in functions
;;;

(defparameter +built-in-functions+
  `(;; arithmetic operators
    + ,(arithmetic-binary-operator "+" +scalar-number-types+)
    - ,(append (signed-integer-types-unary-function "-")
               (float-types-unary-function "-")
               (arithmetic-binary-operator "-" +scalar-number-types+))
    * ,(append '(((float4 float) float4 t "*")
                 ((float float4) float4 t "*"))
               (arithmetic-binary-operator "*" +scalar-number-types+))
    / ,(arithmetic-binary-operator "/" +scalar-number-types+)
    mod ,(arithmetic-binary-operator "%" +scalar-integer-types+)

    ;; relational operators
    = ,(relational-operator "==")
    /= ,(relational-operator "!=")
    < ,(relational-operator "<")
    > ,(relational-operator ">")
    <= ,(relational-operator "<=")
    >= ,(relational-operator ">=")

    ;; logical operators
    not  (((bool) bool nil "!"))
    ;; bitwise logical operators
    logior ,(mapcar (lambda (type) `((,type ,type) ,type t "|")) +scalar-integer-types+)
    logand ,(mapcar (lambda (type) `((,type ,type) ,type t "&")) +scalar-integer-types+)
    
    ;; address-of operator
    pointer (((int)   int*   nil "&")
             ((float) float* nil "&")
             ((double) double* nil "&"))
    ;; shift operator
    << (((int uint) int t "<<"))
    >> (((int uint) int t ">>"))
    ;; built-in vector constructor
    ;; now taken care of by compile-expression to allow all the overloaded constructors
    ;; 
    ;; uint4 (((uint uint uint uint) uint4 nil "(uint4)")
    ;;        ((int int int int) uint4 nil "(uint4)"))
    ;; float3 (((float float float) float3 nil "(float3)"))
    ;; float4 (((float float float float) float4 nil "(float4)"))
    ;; double2 (((double double) double2 nil "(double2)"))
    ;; double3 (((double double double) double3 nil "(double3)"))
    ;; double4 (((double double double double) double4 nil "(double4)"))
    ;;
    ;; type casting intrinsics
    double-to-int-rn (((double) int nil "__double2int_rn"))
    to-uchar ((,#'allow-any-types uchar nil "(uchar)"))
    to-char ((,#'allow-any-types char nil "(char)"))
    to-uint ((,#'allow-any-types uint nil "(uint)"))
    to-int ((,#'allow-any-types int nil "(int)"))
    to-ulong ((,#'allow-any-types ulong nil "(ulong)"))
    to-long ((,#'allow-any-types long nil "(long)"))
    to-float ((,#'allow-any-types float nil "(float)"))
    to-double ((,#'allow-any-types double nil "(double)"))
    to-bool ((,#'allow-any-types bool nil "(bool)"))

    ;; Reinterpreting types
    as-uchar2 (((short) uchar2 nil "as_uchar2"))
    as-uchar4 (((int) uchar4 nil "as_uchar4")
               ((uint) uchar4 nil "as_uchar4")
               ((float) uchar4 nil "as_uchar4"))

    ;; OpenCL v1.2 dr19: 6.12.1 Work-Item Functions
    get-work-dim ((() uint nil "get_work_dim"))
    get-global-size (((int) size-t nil "get_global_size")
                     ((uint) size-t nil "get_global_size"))
    get-global-id (((int) size-t nil "get_global_id")
                   ((uint) size-t nil "get_global_id"))
    get-local-size (((int) size-t nil "get_local_size")
                    ((uint) size-t nil "get_local_size"))
    get-local-id (((int) size-t nil "get_local_id")
                  ((uint) size-t nil "get_local_id"))
    get-num-groups (((int) size-t nil "get_num_groups")
                    ((uint) size-t nil "get_num_groups"))
    get-group-id (((int) size-t nil "get_group_id")
                  ((uint) size-t nil "get_group_id"))
    get-global-offset (((int) size-t nil "get_global_offset")
                       ((uint) size-t nil "get_global_offset"))

    ;; OpenCL v.1.2 dr19: 6.12.2 Math Functions
    acos ,(float-types-unary-function "acos")
    acosh ,(float-types-unary-function "acosh")
    acospi ,(float-types-unary-function "acospi")
    asin ,(float-types-unary-function "asin")
    asinh ,(float-types-unary-function "asinh")
    asinpi ,(float-types-unary-function "asinpi")
    atan ,(float-types-unary-function "atan")
    atan2 ,(float-types-binary-function "atan2")
    cbrt ,(float-types-unary-function "cbrt")
    ceil ,(float-types-unary-function "ceil")
    copysign ,(float-types-binary-function "copysign")
    cos ,(float-types-unary-function "cos")
    cosh ,(float-types-unary-function "cosh")
    cospi ,(float-types-unary-function "cospi")
    erfc ,(float-types-unary-function "erfc")
    erf ,(float-types-unary-function "erf")
    exp ,(float-types-unary-function "exp")
    exp2 ,(float-types-unary-function "exp2")
    exp10 ,(float-types-unary-function "exp10")
    expm1 ,(float-types-unary-function "expm1")
    fabs ,(float-types-unary-function "fabs")
    fdim ,(float-types-binary-function "fdim")
    floor ,(float-types-unary-function "floor")
    fma ,(float-types-ternary-function "fma")
    fmax ,(append (float-types-binary-function "fmax")
                  (vector-scalar-binary-operator "fmax" 'float)
                  (vector-scalar-binary-operator "fmax" 'double))
    fmin ,(append (float-types-binary-function "fmin")
                  (vector-scalar-binary-operator "fmin" 'float)
                  (vector-scalar-binary-operator "fmin" 'double))
    fmod ,(float-types-binary-function "fmod")
    ;;fract
    ;;frexp
    hypot ,(float-types-binary-function "hypot")
    ilogb ,(mapcar #'(lambda (arg-type ret-type)
                       `((,arg-type) ,ret-type nil "ilogb"))
                   (append +all-single-float-types+
                           +all-double-float-types+)
                   (append +all-int-types+
                           +all-int-types+))
    ;;ldexp
    lgamma ,(float-types-unary-function "lgamma")
    ;;lgamma_r
    log ,(float-types-unary-function "log")
    log2 ,(float-types-unary-function "log2")
    log10 ,(float-types-unary-function "log10")
    log1p ,(float-types-unary-function "log1p")
    logb ,(float-types-unary-function "logb")
    mad ,(float-types-ternary-function "mad")
    maxmag ,(float-types-unary-function "maxmag")
    minmag (,@(float-types-unary-function "minmag")
	    ((double2 double2) double2 nil "minmag"))
    ;;modf
    nan ,(mapcar #'(lambda (arg-type ret-type)
                     `((,arg-type) ,ret-type nil "nan"))
                 (append +all-uint-types+
                         +all-ulong-types+)
                 (append +all-single-float-types+
                         +all-double-float-types+))
    nextafter ,(float-types-binary-function "nextafter")
    pow ,(float-types-binary-function "pow")
    pown ,(mapcar #'(lambda (lhs rhs)
                      `((,lhs ,rhs) ,lhs nil "pown"))
                  (append +all-single-float-types+
                          +all-double-float-types+)
                  (append +all-int-types+
                          +all-int-types+))
    powr ,(float-types-binary-function "powr")
    remainder ,(float-types-binary-function "remainder")
    ;;remquo
    rint ,(float-types-unary-function "rint")
    ;;rootn
    rootn ,(mapcar #'(lambda (lhs rhs)
                       `((,lhs ,rhs) ,lhs nil "rootn"))
                   (append +all-single-float-types+
                           +all-double-float-types+)
                   (append +all-int-types+
                           +all-int-types+))
    round ,(float-types-unary-function "round")
    rsqrt ,(float-types-unary-function "rsqrt")
    sin ,(float-types-unary-function "sin")
    ;;sincos
    sinh ,(float-types-unary-function "sinh")
    sinpi ,(float-types-unary-function "sinpi")
    sqrt ,(float-types-unary-function "sqrt")
    tan ,(float-types-unary-function "tan")
    tanh ,(float-types-unary-function "tanh")
    tanpi ,(float-types-unary-function "tanpi")
    tgamma ,(float-types-unary-function "tgamma")
    trunc ,(float-types-unary-function "trunc")

    half-cos ,(same-type-function "half_cos" 1 'float nil)
    half-divide ,(same-type-function "half_divide" 2 'float nil)
    half-exp ,(same-type-function "half_exp" 1 'float nil)
    half-exp2 ,(same-type-function "half_exp2" 1 'float nil)
    half-exp10 ,(same-type-function "half_exp10" 1 'float nil)
    half-log ,(same-type-function "half_log" 1 'float nil)
    half-log2 ,(same-type-function "half_log2" 1 'float nil)
    half-log10 ,(same-type-function "half_log10" 1 'float nil)

    half-powr ,(same-type-function "half_powr" 1 'float nil)
    half-recip ,(same-type-function "half_recip" 1 'float nil)
    half-rsqrt ,(same-type-function "half_rsqrt" 1 'float nil)
    half-sin ,(same-type-function "half_sin" 1 'float nil)
    half-sqrt ,(same-type-function "half_sqrt" 1 'float nil)
    half-tan ,(same-type-function "half_tan" 1 'float nil)

    native-cos ,(same-type-function "native_cos" 1 'float nil)
    native-divide ,(same-type-function "native_divide" 2 'float nil)
    native-exp ,(same-type-function "native_exp" 1 'float nil)
    native-exp2 ,(same-type-function "native_exp2" 1 'float nil)
    native-exp10 ,(same-type-function "native_exp10" 1 'float nil)
    native-log ,(same-type-function "native_log" 1 'float nil)
    native-log2 ,(same-type-function "native_log2" 1 'float nil)
    native-log10 ,(same-type-function "native_log10" 1 'float nil)
    native-powr ,(same-type-function "native_powr" 1 'float nil)
    native-recip ,(same-type-function "native_recip" 1 'float nil)
    native-rsqrt ,(same-type-function "native_rsqrt" 1 'float nil)
    native-sin ,(same-type-function "native_sin" 1 'float nil)
    native-sqrt ,(same-type-function "native_sqrt" 1 'float nil)
    native-tan ,(same-type-function "native_tan" 1 'float nil)

    ;; OpenCL v.1.2 dr19: 6.12.3 Integer Functions
    abs ,(mapcar #'(lambda (arg-type ret-type)
                     `((,arg-type) ,ret-type nil "abs"))
                 (append +all-signed-types+
                         +all-unsigned-types+)
                 (append +all-unsigned-types+
                         +all-unsigned-types+))
    abs-diff ,(mapcar #'(lambda (arg-type ret-type)
                     `((,arg-type ,arg-type) ,ret-type nil "abs_diff"))
                 (append +all-signed-types+
                         +all-unsigned-types+)
                 (append +all-unsigned-types+
                         +all-unsigned-types+))
    add-sat ,(integer-types-binary-function "add_sat")
    hadd ,(integer-types-binary-function "hadd")
    rhadd ,(integer-types-binary-function "rhadd")
    ;; for clamp see also OpenCL v.1.2 dr19: 6.12.4 Common Functions
    clamp ,(append 
            ;; same type args integer/float clamp (TYPES-FUNCTION creates vector types)
            (types-function "clamp" 3 +scalar-number-types+)
            ;; Integer clamp w/ scaler clamping args
            (alexandria:mappend #'(lambda (sgentype)
                                    (mapcar
                                     (lambda (gentype)
                                       `((,gentype ,sgentype ,sgentype) ,gentype nil "clamp"))
                                     (generate-vector-type-symbols sgentype)))
                                (append +scalar-signed-integer-types+ +scalar-unsigned-integer-types+))
            ;; single float clamp w/ scalar clamping args
            (mapcar #'(lambda (gentype)
                        `((,gentype float float) ,gentype nil "clamp"))
                    +all-single-float-types+)
            ;; double float clamp w/ scalar clamping args
            (mapcar #'(lambda (gentype)
                        `((,gentype double double) ,gentype nil "clamp"))
                    +all-double-float-types+))
    clz ,(integer-types-unary-function "clz")
    mad-hi ,(integer-types-ternary-function "mad_hi")
    mad-sat ,(integer-types-ternary-function "mad_sat")
    max ,(append (integer-types-binary-function "max")
                 (integer-vector-scalar-binary-operator "max"))
    min ,(append (integer-types-binary-function "min")
                 (integer-vector-scalar-binary-operator "min"))
    mul-hi ,(integer-types-binary-function "mul_hi")
    rotate ,(integer-types-binary-function "rotate")
    sub-sat ,(integer-types-binary-function "sub_sat")
    upsample ,(mapcar #'(lambda (lhs rhs ret)
                          `((,lhs ,rhs) ,ret nil "upsample"))
                      (append (list 'char 'uchar)
                              +vector-char-types+
                              +vector-uchar-types+
                              (list 'short 'ushort)
                              +vector-short-types+
                              +vector-ushort-types+
                              (list 'int 'uint)
                              +vector-int-types+
                              +vector-uint-types+)
                      (append (list 'uchar 'uchar)
                              +vector-uchar-types+
                              +vector-uchar-types+
                              (list 'ushort 'ushort)
                              +vector-ushort-types+
                              +vector-ushort-types+
                              (list 'uint 'uint)
                              +vector-uint-types+
                              +vector-uint-types+)
                      (append (list 'short 'ushort)
                              +vector-short-types+
                              +vector-ushort-types+
                              (list 'int 'uint)
                              +vector-int-types+
                              +vector-uint-types+
                              (list 'long 'ulong)
                              +vector-long-types+
                              +vector-ulong-types+))
    popcount ,(integer-types-unary-function "popcount")

    mad24 ,(append (same-type-function "mad24" 3 'int nil)
                   (same-type-function "mad24" 3 'uint nil))
    mul24 ,(append (same-type-function "mul24" 2 'int nil)
                   (same-type-function "mul24" 2 'uint nil))

    ;; OpenCL v.1.2 dr19: 6.12.4 Common Functions
    ;; clamp impl Integer function
    degrees ,(float-types-unary-function "degrees")
    ;;max
    ;;min
    ;;mix
    radians ,(float-types-unary-function "radians")
    ;;step
    ;;smoothstep
    sign ,(float-types-unary-function "sign")

    ;; OpenCL v.1.2 dr19: 6.12.5 Geometric Functions
    cross ,(loop for (argments . return) in '(((float3 float3) . float3)
                                              ((float4 float4) . float4)
                                              ((double3 double3) . double3)
                                              ((double4 double4) . double4))
                 collecting `(,argments ,return nil "cross"))
    dot ,(loop for (argments . return) in '(((float2 float2) . float)
                                            ((float3 float3) . float)
                                            ((float4 float4) . float)
                                            ((double2 double2) . double)
                                            ((double3 double3) . double)
                                            ((double4 double4) . double))
               collecting `(,argments ,return nil "dot"))
    distance ,(loop for (arguments . return) in '(((float float) . float)
                                                  ((float2 float2) . float)
                                                  ((float3 float3) . float)
                                                  ((float4 float4) . float)
                                                  ((double double) . double)
                                                  ((double2 double2) . double)
                                                  ((double3 double3) . double)
                                                  ((double4 double4) . double))
                    collecting `(,arguments ,return nil "distance"))
    length ,(loop for (arguments . return) in '(((float) . float)
                                                ((float2) . float)
                                                ((float3) . float)
                                                ((float4) . float)
                                                ((double) . double)
                                                ((double2) . double)
                                                ((double3) . double)
                                                ((double4) . double))
                  collecting `(,arguments ,return nil "length"))
    normalize ,(loop for (arguments . return) in '(((float) . float)
                                                   ((float2) . float2)
                                                   ((float3) . float3)
                                                   ((float4) . float4)
                                                   ((double) . double)
                                                   ((double2) . double2)
                                                   ((double3) . double3)
                                                   ((double4) . double4))
                     collecting `(,arguments ,return nil "normalize"))

    fast-distance ,(loop for (arguments . return) in '(((float float) . float)
                                                       ((float2 float2) . float)
                                                       ((float3 float3) . float)
                                                       ((float4 float4) . float))
                         collecting `(,arguments ,return nil "fast_distance"))
    fast-length ,(loop for (arguments . return) in '(((float) . float)
                                                     ((float2) . float)
                                                     ((float3) . float)
                                                     ((float4) . float))
                       collecting `(,arguments ,return nil "fast_length"))
    fast-normalize ,(loop for (arguments . return) in '(((float) . float)
                                                        ((float2) . float)
                                                        ((float3) . float)
                                                        ((float4) . float))
                          collecting `(,arguments ,return nil "fast_normalize"))

    ;; TODO
    ;; OpenCL v.1.2 dr19: 6.12.6 Relational Functions

    ;; TODO
    ;; OpenCL v.1.2 dr19: 6.12.7 Vector Data Load and Store Functions

    ;; OpenCL v.1.2 dr19: 6.12.8 Synchronization Functions
    barrier (((cl-mem-fence-flags) void nil "barrier"))

    ;; OpenCL v.1.2 dr19: 6.12.9 Explicit Memory Fence Functions
    mem-fence (((cl-mem-fence-flags) void nil "mem_fence"))
    read-mem-fence (((cl-mem-fence-flags) void nil "read_mem_fence"))
    write-mem-fence (((cl-mem-fence-flags) void nil "write_mem_fence"))

    ;; TODO
    ;; OpenCL v.1.2 dr19: 6.12.10 Async Copies from Global to Local Memory, Local to Global Memory, and Prefetch

    ;; OpenCL v.1.2 dr19: 6.12.11 Atomic Functions
    atomic-add (((int* int) int nil "atomic_add")
                ((uint* uint) uint nil "atomic_add"))
    atomic-sub (((int* int) int nil "atomic_sub")
                ((uint* uint) uint nil "atomic_sub"))
    atomic-xchg (((int* int) int nil "atomic_xchg")
                 ((uint* uint) uint nil "atomic_xchg")
                 ((float* float) float nil "atomic_xchg"))
    atomic-inc (((int*) int nil "atomic_inc")
                ((uint*) uint nil "atomic_inc"))
    atomic-dec (((int*) int nil "atomic_dec")
                ((uint*) uint nil "atomic_dec"))
    atomic-cmpxchg (((int* int int) int nil "atomic_cmpxchg")
                    ((uint* uint uint) uint nil "atomic_cmpxchg"))
    atomic-min (((int*) int nil "atomic_min")
                ((uint*) uint nil "atomic_min"))
    atomic-max (((int*) int nil "atomic_max")
                ((uint*) uint nil "atomic_max"))
    atomic-and (((int* int) int nil "atomic_and")
                ((uint* int) uint nil "atomic_and"))
    atomic-or (((int*) int nil "atomic_or")
               ((uint*) uint nil "atomic_or"))
    atomic-xor (((int*) int nil "atomic_xor")
                ((uint*) uint nil "atomic_xor"))

    ;; OpenCL v.1.2 dr19: 6.12.12 Miscellaneous Vector Functions
    vec-step ,(loop for argument in +number-types+
                    collecting `((argument) int nil "vec_step"))
    shuffle ,(loop for (x . mask) in (append (mapcar #'cons +vector-integer-types+ +vector-unsigned-integer-types+)
                                             (mapcar #'cons +vector-unsigned-integer-types+ +vector-unsigned-integer-types+)
                                             (mapcar #'cons +vector-single-float-types+ +vector-uint-types+)
                                             (mapcar #'cons +vector-double-float-types+ +vector-ulong-types+))
                   collecting `((,x ,mask) x nil "shuffle"))
    shuffle2 ,(loop for (x . mask) in (append (mapcar #'cons +vector-integer-types+ +vector-unsigned-integer-types+)
                                              (mapcar #'cons +vector-unsigned-integer-types+ +vector-unsigned-integer-types+)
                                              (mapcar #'cons +vector-single-float-types+ +vector-uint-types+)
                                              (mapcar #'cons +vector-double-float-types+ +vector-ulong-types+))
                    collecting `((,x ,x ,mask) x nil "shuffle2"))

    ;; OpenCL v.1.2 dr19: 6.12.13 printf
    printf ((,#'allow-any-types int nil "printf"))))

(defun inferred-function-candidates (name)
  (or (getf +built-in-functions+ name)
      (error "The function ~S is undefined." name)))

(defun inferred-function (name argument-types)
  (let ((candidates (inferred-function-candidates name)))
    (or (assoc argument-types candidates :test #'(lambda (args params)
                                                   (typecase params
                                                     (function (funcall params args))
                                                     (list (equal args params))
                                                     (t nil))))
        (error "The function ~S with ~S is type mismatch." name argument-types))))

(defun built-in-function-return-type (name argument-types)
  (let ((return-type (cadr (inferred-function name argument-types))))
    (typecase return-type
      (function (funcall return-type argument-types))
      (t return-type))))

(defun built-in-function-infix-p (name argument-types)
  (caddr (inferred-function name argument-types)))

(defun built-in-function-c-name (name argument-types)
  (cadddr (inferred-function name argument-types)))

;;;
;;; Vector Literal
;;;

(defun vector-literal-p (form)
  (cl-pattern:match form
    ((vec-type . _) (find vec-type +vector-number-types+))
    (_ nil)))
