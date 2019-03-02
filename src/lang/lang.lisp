#|
  This file is a part of oclcl project.
  Copyright (c) 2012 Masayuki Takagi (kamonama@gmail.com)
                2015 gos-k (mag4.elan@gmail.com)
|#

(in-package :cl-user)
(defpackage :oclcl.lang
  (:use :cl
        :cl-reexport)
  (:documentation "
Exports the symbols for writing OpenCL programs.
APIs for manipulating those programs (e.g. compiling, lookup...) are not exported from this package."))
(in-package :oclcl.lang)



;; reexport symbols of data structures oclcl provides
(reexport-from :oclcl.lang.data
               :include '(;; Float2
			  :float2
                          :make-float2
                          :float2-x
                          :float2-y
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
                          ;; Int2
			  :int2
                          :make-int2
                          :int2-x
                          :int2-y
                          :int2-p
                          :int2-=
                          ;; Int3
                          :int3
                          :make-int3
                          :int3-x
                          :int3-y
                          :int3-z
                          :int3-p
                          :int3-=
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
                          :uint2-p
                          :uint2-=
                          ;; Uint3
                          :uint3
                          :make-uint3
                          :uint3-x
                          :uint3-y
                          :uint3-z
                          :uint3-p
                          :uint3-=
                          ;; Uint4
                          :uint4
                          :make-uint4
                          :uint4-x
                          :uint4-y
                          :uint4-z
                          :uint4-w
                          :uint4-p
                          :uint4-=
                          ;; Long2
			  :long2
                          :make-long2
                          :long2-x
                          :long2-y
                          :long2-p
                          :long2-=
                          ;; Long3
                          :long3
                          :make-long3
                          :long3-x
                          :long3-y
                          :long3-z
                          :long3-p
                          :long3-=
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
                          :ulong2-p
                          :ulong2-=
                          ;; Ulong3
                          :ulong3
                          :make-ulong3
                          :ulong3-x
                          :ulong3-y
                          :ulong3-z
                          :ulong3-p
                          :ulong3-=
                          ;; Ulong4
                          :ulong4
                          :make-ulong4
                          :ulong4-x
                          :ulong4-y
                          :ulong4-z
                          :ulong4-w
                          :ulong4-p
                          :ulong4-=))

;; reexport symbols of oclcl types
;; OpenCL v1.2 dr19: 6.1 Supported Data Type
(reexport-from :oclcl.lang.type
               :include '(:bool
                          :bool*
                          :char :char2 :char3 :char4 :char8 :char16
                          :char* :char2* :char3* :char4* :char8* :char16*
                          :uchar :uchar2 :uchar3 :uchar4 :uchar8 :uchar16
                          :uchar* :uchar2* :uchar3* :uchar4* :uchar8* :uchar16*
                          :short :short2 :short3 :short4 :short8 :short16
                          :short* :short2* :short3* :short4* :short8* :short16*
                          :ushort :ushort2 :ushort3 :ushort4 :ushort8 :ushort16
                          :ushort* :ushort2* :ushort3* :ushort4* :ushort8* :ushort16*
			  ;; :int :int2 :int3 :int4 :int8 :int16
			  :int :int8 :int16
                          :int* :int2* :int3* :int4* :int8* :int16*
                          ;; :uint :uint2 :uint3 :uint4 :uint8 :uint16
			  :uint :uint8 :uint16
                          :uint* :uint2* :uint3* :uint4* :uint8* :uint16*
                          ;; :long :long2 :long3 :long4 :long8 :long16
			  :long :long8 :long16
                          :long* :long2* :long3* :long4* :long8* :long16*
                          ;; :ulong :ulong2 :ulong3 :ulong4 :ulong8 :ulong16
			  :ulong :ulong8 :ulong16
                          :ulong* :ulong2* :ulong3* :ulong4* :ulong8* :ulong16*
                          ;:float :float2 :float3 :float4 :float8 :float16
                          :float :float8 :float16
                          :float* :float2* :float3* :float4* :float8* :float16*
                          ;:double :double2 :double3 :double4 :double8 :double16
                          :double :double8 :double16
                          :double* :double2* :double3* :double4* :double8* :double16*
                          :half
                          :size-t
                          :ptrdiff-t
                          :intptr-t
                          :uintptr-t
                          :void
                          :cl-mem-fence-flags))

;; reexport symbols of oclcl syntax except the ones exported
;; from COMMON-LISP package
(reexport-from :oclcl.lang.syntax
               :include '(:clk-local-mem-fence
                          :clk-global-mem-fence
                          :with-local-memory
                          :set
                          :vref))

;; reexport symbols of oclcl built-in functions except the ones
;; exported from COMMON-LISP package
(reexport-from :oclcl.lang.built-in)


