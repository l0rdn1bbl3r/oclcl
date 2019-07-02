#|
  This file is a part of oclcl project.
  Copyright (c) 2012 Masayuki Takagi (kamonama@gmail.com)
                2015 gos-k (mag4.elan@gmail.com)
|#

(in-package :cl-user)
(defpackage oclcl.lang.data
  (:use :cl)
  (:export ;; Symbol
   :oclcl-symbol
   :oclcl-symbol-p
   ;; Bool
   :oclcl-bool-p
   ;; Int
   :oclcl-int-p
   ;; Float
   :oclcl-float-p
   ;; Double
   :oclcl-double-p
   ;; String
   :oclcl-string-p
   ;; Float2
   :float2
   :make-float2
   :float2-x
   :float2-y
   :float2-z
   :float2-p
   :float2-=
   ;; Float3
   :float3
   :make-float3
   :float3-x
   :float3-y
   :float3-z
   :float3-p
   :float3-=
   ;; Float4
   :float4
   :make-float4
   :float4-x
   :float4-y
   :float4-z
   :float4-w
   :float4-p
   :float4-=
   ;; Double2
   :double2
   :make-double2
   :double2-x
   :double2-y
   :double2-z
   :double2-p
   :double2-=           
   ;; Double3
   :double3
   :make-double3
   :double3-x
   :double3-y
   :double3-z
   :double3-p
   :double3-=
   ;; Double4
   :double4
   :make-double4
   :double4-x
   :double4-y
   :double4-z
   :double4-w
   :double4-p
   :double4-=
   ;; Long2
   :long2
   :make-long2
   :long2-x
   :long2-y
   :long2-z
   :long2-w
   :long2-p
   :long2-=
   ;; Long3
   :long3
   :make-long3
   :long3-x
   :long3-y
   :long3-z
   :long3-w
   :long3-p
   :long4-=
   ;; Long4
   :long4
   :make-long4
   :long4-x
   :long4-y
   :long4-z
   :long4-w
   :long4-p
   :long4-=
   ;; Ulong2
   :ulong2
   :make-ulong2
   :ulong2-x
   :ulong2-y
   :ulong2-z
   :ulong2-w
   :ulong2-p
   :ulong2-=
   ;; Ulong3
   :ulong3
   :make-ulong3
   :ulong3-x
   :ulong3-y
   :ulong3-z
   :ulong3-w
   :ulong3-p
   :ulong4-=
   ;; Ulong4
   :ulong4
   :make-ulong4
   :ulong4-x
   :ulong4-y
   :ulong4-z
   :ulong4-w
   :ulong4-p
   :ulong4-=
   ;; Int2
   :int2
   :make-int2
   :int2-x
   :int2-y
   :int2-z
   :int2-w
   :int2-p
   :int2-=
   ;; Int3
   :int3
   :make-int3
   :int3-x
   :int3-y
   :int3-z
   :int3-w
   :int3-p
   :int4-=
   ;; Int4
   :int4
   :make-int4
   :int4-x
   :int4-y
   :int4-z
   :int4-w
   :int4-p
   :int4-=
   ;; Uint2
   :uint2
   :make-uint2
   :uint2-x
   :uint2-y
   :uint2-z
   :uint2-w
   :uint2-p
   :uint2-=
   ;; Uint3
   :uint3
   :make-uint3
   :uint3-x
   :uint3-y
   :uint3-z
   :uint3-w
   :uint3-p
   :uint4-=
   ;; Uint4
   :uint4
   :make-uint4
   :uint4-x
   :uint4-y
   :uint4-z
   :uint4-w
   :uint4-p
   :uint4-=
   ))
(in-package :oclcl.lang.data)


;;;
;;; Symbol
;;;

(deftype oclcl-symbol ()
  `(satisfies oclcl-symbol-p))

(defun oclcl-symbol-p (object)
  (symbolp object))


;;;
;;; Bool
;;;

(defun oclcl-bool-p (object)
  (typep object 'boolean))


;;;
;;; Int
;;;

(defun oclcl-int-p (object)
  (integerp object))


;;;
;;; Float
;;;

(defun oclcl-float-p (object)
  (typep object 'single-float))


;;;
;;; Double
;;;

(defun oclcl-double-p (object)
  (typep object 'double-float))

;;; String
;;;

(defun oclcl-string-p (object)
  (stringp object))

;;;
;;; Float3
;;;

(defstruct (float3 (:constructor make-float3 (x y z)))
  (x 0.0 :type single-float)
  (y 0.0 :type single-float)
  (z 0.0 :type single-float))

(defun float3-= (a b)
  (and (= (float3-x a) (float3-x b))
       (= (float3-y a) (float3-y b))
       (= (float3-z a) (float3-z b))))

(cffi:defcstruct (float3 :class float3-c)
  (x :float)
  (y :float)
  (z :float))

(defmethod cffi:translate-into-foreign-memory ((value float3)
                                               (type float3-c)
                                               ptr)
  (cffi:with-foreign-slots ((x y z) ptr (:struct float3))
    (setf x (float3-x value)
          y (float3-y value)
          z (float3-z value))))

(defmethod cffi:translate-from-foreign (value (type float3-c))
  (cffi:with-foreign-slots ((x y z) value (:struct float3))
    (make-float3 x y z)))

;;;
;;; Float4
;;;

(defstruct (float4 (:constructor make-float4 (x y z w)))
  (x 0.0 :type single-float)
  (y 0.0 :type single-float)
  (z 0.0 :type single-float)
  (w 0.0 :type single-float))

(defun float4-= (a b)
  (and (= (float4-x a) (float4-x b))
       (= (float4-y a) (float4-y b))
       (= (float4-z a) (float4-z b))
       (= (float4-w a) (float4-w b))))

(cffi:defcstruct (float4 :class float4-c)
  (x :float)
  (y :float)
  (z :float)
  (w :float))

(defmethod cffi:translate-into-foreign-memory ((value float4)
                                               (type float4-c)
                                               ptr)
  (cffi:with-foreign-slots ((x y z w) ptr (:struct float4))
    (setf x (float4-x value)
          y (float4-y value)
          z (float4-z value)
          w (float4-w value))))

(defmethod cffi:translate-from-foreign (value (type float4-c))
  (cffi:with-foreign-slots ((x y z w) value (:struct float4))
    (make-float4 x y z w)))


;;;
;;; Double3
;;;

(defstruct (double3 (:constructor make-double3 (x y z)))
  (x 0.0d0 :type double-float)
  (y 0.0d0 :type double-float)
  (z 0.0d0 :type double-float))

(defun double3-= (a b)
  (and (= (double3-x a) (double3-x b))
       (= (double3-y a) (double3-y b))
       (= (double3-z a) (double3-z b))))

(cffi:defcstruct (double3 :class double3-c)
  (x :double)
  (y :double)
  (z :double))

(defmethod cffi:translate-into-foreign-memory ((value double3)
                                               (type double3-c)
                                               ptr)
  (cffi:with-foreign-slots ((x y z) ptr (:struct double3))
    (setf x (double3-x value)
          y (double3-y value)
          z (double3-z value))))

(defmethod cffi:translate-from-foreign (value (type double3-c))
  (cffi:with-foreign-slots ((x y z) value (:struct double3))
    (make-double3 x y z)))


;;;
;;; Double4
;;;

(defstruct (double4 (:constructor make-double4 (x y z w)))
  (x 0.0d0 :type double-float)
  (y 0.0d0 :type double-float)
  (z 0.0d0 :type double-float)
  (w 0.0d0 :type double-float))

(defun double4-= (a b)
  (and (= (double4-x a) (double4-x b))
       (= (double4-y a) (double4-y b))
       (= (double4-z a) (double4-z b))
       (= (double4-w a) (double4-w b))))

(cffi:defcstruct (double4 :class double4-c)
  (x :double)
  (y :double)
  (z :double)
  (w :double))

(defmethod cffi:translate-into-foreign-memory ((value double4)
                                               (type double4-c)
                                               ptr)
  (cffi:with-foreign-slots ((x y z w) ptr (:struct double4))
    (setf x (double4-x value)
          y (double4-y value)
          z (double4-z value)
          w (double4-w value))))

(defmethod cffi:translate-from-foreign (value (type double3-c))
  (cffi:with-foreign-slots ((x y z w) value (:struct double4))
    (make-double4 x y z w)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-lisp-and-foreign-vector-structs (name type cffi-type sizes)
  `(progn
     ,@(alexandria:mappend
	(lambda (n)
	  (let* ((slots (subseq '(x y z w) 0 n))
		 (name-n (alexandria:symbolicate name (format nil "~d" n)))
		 (c-name-n (alexandria:symbolicate name-n '-c))
		 (constructor (alexandria:symbolicate 'make- name-n))
                 (slot-vars-a (subseq '(ax ay az aw) 0 n))
		 (slot-vars-b (subseq '(bx by bz bw) 0 n))
		 (slot-bindings-a (mapcar (lambda (var slot) `(,var ,slot)) slot-vars-a slots))
		 (slot-bindings-b (mapcar (lambda (var slot) `(,var ,slot)) slot-vars-b slots)))
            `((defstruct (,name-n (:constructor ,constructor ,(subseq slots 0 n)))
		,@(mapcar (lambda (slot) `(,slot nil :type ,type)) slots))
	      (defun ,(alexandria:symbolicate name-n '-=) (a b)
		(with-slots ,slot-bindings-a a
		  (with-slots ,slot-bindings-b b
		    (and ,@(mapcar (lambda (a b) `(= ,a ,b)) slot-vars-a slot-vars-b)))))
	      (cffi:defcstruct (,name-n :class ,c-name-n)
		,@(mapcar (lambda (slot) `(,slot ,cffi-type)) slots))
	      (defmethod cffi:translate-into-foreign-memory ((value ,name-n) (type ,c-name-n) ptr)
		(cffi:with-foreign-slots (,slots ptr (:struct ,name-n))
		  (with-slots ,slot-bindings-a value
		    (setf ,@(alexandria:mappend (lambda (a b) `(,a ,b)) slots slot-vars-a)))))
	      (defmethod cffi:expand-into-foreign-memory ((value ,name-n) (type ,c-name-n) ptr)
		`(cffi:with-foreign-slots (,',slots ,ptr (:struct ,',name-n))
		   (with-slots ,',slot-bindings-a ,value
		     (setf ,@',(alexandria:mappend (lambda (a b) `(,a ,b)) slots slot-vars-a)))))
	      (defmethod cffi:translate-from-foreign (ptr (type ,c-name-n))
		(cffi:with-foreign-slots (,slots ptr (:struct ,name-n))
		  (,constructor ,@slots)))
	      (defmethod cffi:expand-from-foreign (ptr (type ,c-name-n))
		`(cffi:with-foreign-slots (,',slots ,ptr (:struct ,',name-n))
		   (,',constructor ,@',slots))))))
	sizes)))

;; define missing structs
(define-lisp-and-foreign-vector-structs long (signed-byte 64) :int64 (2 3 4))
(define-lisp-and-foreign-vector-structs ulong (unsigned-byte 64) :uint64 (2 3 4))
(define-lisp-and-foreign-vector-structs int (signed-byte 32) :int32 (2 3 4))
(define-lisp-and-foreign-vector-structs uint (unsigned-byte 32) :uint32 (2 3 4))
(define-lisp-and-foreign-vector-structs double double-float :double (2))
(define-lisp-and-foreign-vector-structs float single-float :float (2))
