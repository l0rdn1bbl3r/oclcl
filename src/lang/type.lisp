#|
  This file is a part of oclcl project.
  Copyright (c) 2012 Masayuki Takagi (kamonama@gmail.com)
                2015 gos-k (mag4.elan@gmail.com)
|#

(in-package :cl-user)
(defpackage oclcl.lang.type
  (:use :cl
        :oclcl.lang.data)
  (:export ;; opencl types
           :bool
           :bool*
           :char :char2 :char3 :char4 :char8 :char16
           :char* :char2* :char3* :char4* :char8* :char16*
           :uchar :uchar2 :uchar3 :uchar4 :uchar8 :uchar16
           :uchar* :uchar2* :uchar3* :uchar4* :uchar8* :uchar16*
           :short :short2 :short3 :short4 :short8 :short16
           :short* :short2* :short3* :short4* :short8* :short16*
           :ushort :ushort2 :ushort3 :ushort4 :ushort8 :ushort16
           :ushort* :ushort2* :ushort3* :ushort4* :ushort8* :ushort16*
           :int :int2 :int3 :int4 :int8 :int16
           :int* :int2* :int3* :int4* :int8* :int16*
           :uint :uint2 :uint3 :uint4 :uint8 :uint16
           :uint* :uint2* :uint3* :uint4* :uint8* :uint16*
           :long :long2 :long3 :long4 :long8 :long16
           :long* :long2* :long3* :long4* :long8* :long16*
           :ulong :ulong2 :ulong3 :ulong4 :ulong8 :ulong16
           :ulong* :ulong2* :ulong3* :ulong4* :ulong8* :ulong16*
           ;:float :float2 :float3 :float4 :float8 :float16
           :float :float2 :float8 :float16
           :float* :float2* :float3* :float4* :float8* :float16*
           ;:double :double2 :double3 :double4 :double8 :double16
           :double :double2 :double8 :double16
           :double* :double2* :double3* :double4* :double8* :double16*
           :size-t
           :void
           :cl-mem-fence-flags
           ;; Type
           :oclcl-type
           :oclcl-type-p
           :cffi-type
           :cffi-type-size
           :opencl-type
           ;; Scalar type
           :scalar-type-p
           ;; Structure type
           :structure-type-p
           ;; Structure accessor
           :structure-accessor-p
           :structure-from-accessor
           :structure-accessor-opencl-accessor
           :structure-accessor-return-type
           ;; Array type
           :array-type-p
           :array-type-base
           :array-type-dimension
           :array-type
           ;; User defined structs
           :ocl-user-struct-p
           :ocl-user-struct-accessor-p
           :symbol-user-struct
           :symbol-user-struct-slot
           :unbound-user-struct
           :unbound-user-struct-slot
           ;; ocl-struct
           :ocl-struct
           ;; readers
           :struct-ocl-name
           :struct-c-name
           :struct-slots
           :struct-accessors
           ;; ocl-struct-slot
           :ocl-struct-slot
           ;; readers
           :struct-slot-ocl-name
           :slot-accessor
           :struct-slot-c-name
           :struct-slot-type
           )
  (:import-from :alexandria
                :format-symbol))
(in-package :oclcl.lang.type)


;;;
;;; Type
;;;

(deftype oclcl-type ()
  `(satisfies oclcl-type-p))

(defun oclcl-type-p (object)
  (or (scalar-type-p object)
      (structure-type-p object)
      (array-type-p object)))

(defun cffi-type (type)
  (cond
    ((scalar-type-p type) (scalar-cffi-type type))
    ((structure-type-p type) (structure-cffi-type type))
    ((array-type-p type) (array-cffi-type type))
    (t (error "The value ~S is an invalid type." type))))

(defun cffi-type-size (type)
  (cond
    ((scalar-type-p type) (scalar-cffi-type-size type))
    ((structure-type-p type) (structure-cffi-type-size type))
    ((array-type-p type) (array-cffi-type-size type))
    (t (error "The value ~S is an invalid type." type))))

(defun opencl-type (type)
  (cond
    ((scalar-type-p type) (scalar-opencl-type type))
    ((structure-type-p type) (structure-opencl-type type))
    ((array-type-p type) (array-opencl-type type))
    (t (error "The value ~S is an invalid type." type))))


;;;
;;; Scalar type
;;;

(defparameter +scalar-types+
  '((void :void "void")
    (bool (:boolean :int8) "bool")
    (char :char "char")
    (uchar :uchar "uchar")
    (short :short "short")
    (ushort :ushort "ushort")
    (int :int "int")
    (uint :uint "uint")
    (long :long "long")
    (ulong :ulong "ulong")
    (float :float "float")
    (double :double "double")
    (size-t :size-t "size_t")
    (cl-mem-fence-flags :int "cl_mem_fence_flags")))

(defun scalar-type-p (object)
  (and (assoc object +scalar-types+)
       t))

(defun scalar-cffi-type (type)
  (unless (scalar-type-p type)
    (error "The value ~S is an invalid type." type))
  (cadr (assoc type +scalar-types+)))

(defun scalar-cffi-type-size (type)
  (cffi:foreign-type-size (scalar-cffi-type type)))

(defun scalar-opencl-type (type)
  (unless (scalar-type-p type)
    (error "The value ~S is an invalid type." type))
  (caddr (assoc type +scalar-types+)))


;;;
;;; Structure type
;;;

(defparameter +structure-table+
  '((float2 "float2" ((float2-x "x" float)
                      (float2-y "y" float)))
    (float3 "float3" ((float3-x "x" float)
                      (float3-y "y" float)
                      (float3-z "z" float)))
    (float4 "float4" ((float4-x "x" float)
                      (float4-y "y" float)
                      (float4-z "z" float)
                      (float4-w "w" float)))
    ;; numeric accessors are defined as VREF in the compiler see COMPILE-EXPRESSION
    (float8 "float8")
    (float16 "float16")
    (double2 "double2" ((double2-x "x" double)
                        (double2-y "y" double)))
    (double3 "double3" ((double3-x "x" double)
                        (double3-y "y" double)
                        (double3-z "z" double)))
    (double4 "double4" ((double4-x "x" double)
                        (double4-y "y" double)
                        (double4-z "z" double)
                        (double4-w "w" double)))
    (double8 "double8")
    (double16 "double16")
    (int2 "int2" ((int2-x "x" int)
                  (int2-y "y" int)))
    (int3 "int3" ((int3-x "x" int)
                  (int3-y "y" int)
                  (int3-z "z" int)))
    (int4 "int4" ((int4-x "x" int)
                  (int4-y "y" int)
                  (int4-z "z" int)
                  (int4-w "w" int)))
    (int8 "int8")
    (int16 "int16")
    (uint2 "uint2" ((uint2-x "x" uint)
                    (uint2-y "y" uint)))
    (uint3 "uint3" ((uint3-x "x" uint)
                    (uint3-y "y" uint)
                    (uint3-z "z" uint)))
    (uint4 "uint4" ((uint4-x "x" uint)
                    (uint4-y "y" uint)
                    (uint4-z "z" uint)
                    (uint4-w "w" uint)))
    (uint8 "uint8")
    (uint16 "uint16")
    (long2 "long2" ((long2-x "x" long)
                    (long2-y "y" long)))
    (long3 "long3" ((long3-x "x" long)
                    (long3-y "y" long)
                    (long3-z "z" long)))
    (long4 "long4" ((long4-x "x" long)
                    (long4-y "y" long)
                    (long4-z "z" long)
                    (long4-w "w" long)))
    (long8 "long8")
    (long16 "long16")
    (ulong2 "ulong2" ((ulong2-x "x" ulong)
                      (ulong2-y "y" ulong)))
    (ulong3 "ulong3" ((ulong3-x "x" ulong)
                      (ulong3-y "y" ulong)
                      (ulong3-z "z" ulong)))
    (ulong4 "ulong4" ((ulong4-x "x" ulong)
                      (ulong4-y "y" ulong)
                      (ulong4-z "z" ulong)
                      (ulong4-w "w" ulong)))
    (ulong8 "ulong8")
    (ulong16 "ulong16")))

(defparameter +structure-types+
  (mapcar #'car +structure-table+))

(defun structure-type-p (object)
  (and (or (member object +structure-types+)
           (ocl-user-struct-p object))
       t))

(defun structure-cffi-type (type)
  (unless (structure-type-p type)
    (error "The value ~S is an invalid type." type))
  `(:struct ,type))

(defun structure-cffi-type-size (type)
  (cffi:foreign-type-size (structure-cffi-type type)))

(defun structure-opencl-type (type)
  (unless (structure-type-p type)
    (error "The value ~S is an invalid type." type))
  (if (ocl-user-struct-p type)
      (struct-c-name (symbol-user-struct type))
      (cadr (assoc type +structure-table+))))

(defun structure-accessors (type)
  (unless (structure-type-p type)
    (error "The value ~S is an invalid type." type))
  (if (ocl-user-struct-p type)
      (struct-slots (symbol-user-struct type))
      (caddr (assoc type +structure-table+))))

;;;
;;; User defined structs
;;;

(defclass ocl-struct ()
  ((ocl-name :initarg :ocl-name :reader struct-ocl-name)
   (slots :initarg :slots :reader struct-slots)
   (accessors :initarg :accessors :reader struct-accessors)))

(defun struct-c-name (ocl-struct)
  (oclcl.lang.util:c-identifier (struct-ocl-name ocl-struct)))

(defclass ocl-struct-slot ()
  ((ocl-name :initarg :ocl-name :reader struct-slot-ocl-name)
   (accessor :initarg :accessor :reader slot-accessor)
   (type :initarg :type :reader struct-slot-type)
   (struct :initarg :struct :reader struct-slot-struct)))

(defun struct-slot-c-name (ocl-struct-slot)
  (oclcl.lang.util:c-identifier (struct-slot-ocl-name ocl-struct-slot)))

(lispn:define-namespace user-struct ocl-struct nil
  "A namespace for user defined structs.")

(lispn:define-namespace user-struct-slot ocl-struct-slot nil
  "A namespace for slots of user defined structs.")

(defun ocl-user-struct-p (x)
  (and (handler-case (symbol-user-struct x)
         (unbound-user-struct () nil))
       t))

(defun ocl-user-struct-accessor-p (x)
  (and (handler-case (symbol-user-struct-slot x)
         (unbound-user-struct-slot () nil))
       t))

;;;
;;; Structure type - accessor
;;;

(defparameter +accessor->structure+
  (loop for structure in +structure-types+
     append (loop for (accessor nil nil) in (structure-accessors structure)
               collect (list accessor structure))))

(defun %structure-from-accessor (accessor)
  (cadr (assoc accessor +accessor->structure+)))

(defun structure-accessor-p (accessor)
  (and (or (%structure-from-accessor accessor)
           (ocl-user-struct-accessor-p accessor))
       t))

(defun structure-from-accessor (accessor)
  (typecase accessor
    (symbol (if (ocl-user-struct-accessor-p accessor)
                (struct-slot-struct (symbol-user-struct-slot accessor))
                (%structure-from-accessor accessor)))
    (t (error "The value ~S is not a structure accessor." accessor))))

(defun structure-accessor-opencl-accessor (accessor)
  (if (ocl-user-struct-accessor-p accessor)
      (struct-slot-c-name (symbol-user-struct-slot accessor))
      (let ((structure (structure-from-accessor accessor)))
        (second (assoc accessor (structure-accessors structure))))))

(defun structure-accessor-return-type (accessor)
  (if (ocl-user-struct-accessor-p accessor)
      (struct-slot-type (symbol-user-struct-slot accessor))
      (let ((structure (structure-from-accessor accessor)))
        (third (assoc accessor (structure-accessors structure))))))


;;;
;;; Array type
;;;

(defparameter +array-type-regex+
  "^([^\\*]+)(\\*+)$")

(defun array-type-p (object)
  (when (symbolp object)
    (let ((package (symbol-package object))
          (object-string (princ-to-string object)))
      (cl-ppcre:register-groups-bind (base-string nil)
          (+array-type-regex+ object-string)
        (let ((base (intern (string base-string) package)))
          (oclcl-type-p base))))))

(defun array-type-base (type)
  (unless (array-type-p type)
    (error "The value ~S is an invalid type." type))
  (let ((type-string (princ-to-string type)))
    (cl-ppcre:register-groups-bind (base-string nil)
        (+array-type-regex+ type-string)
      (intern (string base-string) (symbol-package type)))))

(defun array-type-stars (type)
  (unless (array-type-p type)
    (error "The value ~S is an invalid type." type))
  (let ((type-string (princ-to-string type)))
    (cl-ppcre:register-groups-bind (_ stars-string)
        (+array-type-regex+ type-string)
      (declare (ignore _))
      (intern (string stars-string) 'oclcl.lang.type)))) 

(defun array-type-dimension (type)
  (length (princ-to-string (array-type-stars type))))

(defun array-cffi-type (type)
  (unless (array-type-p type)
    (error "The value ~S is an invalid type." type))
  'cu-device-ptr)

(defun array-cffi-type-size (type)
  (cffi:foreign-type-size (array-cffi-type type)))

(defun array-opencl-type (type)
  (let ((base (array-type-base type))
        (stars (array-type-stars type)))
    (format nil "__global ~A~A" (opencl-type base) stars)))

(defun array-type (type dimension)
  (unless (and (oclcl-type-p type)
               (not (array-type-p type)))
    (error "The value ~S is an invalid type." type))
  (let ((stars (loop repeat dimension collect #\*)))
    (handler-case (format-symbol (symbol-package type) "~A~{~A~}" type stars)
      (error () (format-symbol 'oclcl.lang.type "~A~{~A~}" type stars)))))
