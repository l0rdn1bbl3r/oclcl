#|
  This file is a part of oclcl project.
  Copyright (c) 2012 Masayuki Takagi (kamonama@gmail.com)
                2015 gos-k (mag4.elan@gmail.com)
|#

(in-package :cl-user)
(defpackage oclcl-test.lang.type
  (:use :cl :prove
        :oclcl.lang.data
        :oclcl.lang.type))
(in-package :oclcl-test.lang.type)

;; add a dummy user-struct
(setf (symbol-user-struct 'foo)
      (make-instance 'ocl-struct :ocl-name 'foo :slots '(bar baz) :accessors '(foo-bar foo-baz))
      (symbol-user-struct-slot 'foo-bar)
      (make-instance 'ocl-struct-slot :struct 'foo :type 'int :accessor 'foo-bar :ocl-name 'bar)
      (symbol-user-struct-slot 'foo-baz)
      (make-instance 'ocl-struct-slot :struct 'foo :type 'float :accessor 'foo-baz :ocl-name
                     'baz))

(cffi:defcstruct foo
  (bar :int)
  (baz :float))

(plan nil)


;;;
;;; test OCLCL-TYPE-P function
;;;

(subtest "OCLCL-TYPE-P"
  (dolist (type '(char uchar short ushort int uint long ulong))
    (is (oclcl-type-p type) t (format nil "integer type : ~a" type)))
  (dolist (type '(float double))
    (is (oclcl-type-p type) t (format nil "float type : ~a" type)))
  (is (oclcl-type-p 'int) t "basic case 1")
  (is (oclcl-type-p 'float3) t "basic case 2")
  (is (oclcl-type-p 'float3*) t "basic case 3")
  (is (oclcl-type-p '*float*) nil "basic case 4")
  (is (oclcl-type-p 'foo) t "user struct: foo"))


;;;
;;; test CFFI-TYPE function
;;;

(subtest "CFFI-TYPE"
  (is (cffi-type 'int) :int "basic case 1")
  (is (cffi-type 'float3) '(:struct float3) "basic case 2")
  (is (cffi-type 'foo) '(:struct foo) "user struct: foo"))


;;;
;;; test CFFI-TYPE-SIZE function
;;;

(subtest "CFFI-TYPE-SIZE"
  (is (cffi-type-size 'int) 4 "basic case 1")
  (is (cffi-type-size 'float3) 12 "basic case 2")
  (is (cffi-type-size 'foo)
      (+ (cffi-type-size 'int) (cffi-type-size 'float))
      "user struct: foo"))


;;;
;;; test OPENCL-TYPE function
;;;

(subtest "OPENCL-TYPE"
  (is (opencl-type 'int) "int" "basic case 1")
  (is (opencl-type 'float3) "float3" "basic case 2")
  (is (opencl-type 'float3*) "__global float3*" "basic case 3")
  (is (opencl-type 'foo) "foo" "user struct: foo"))


;;;
;;; test STRUCTURE-ACCESSOR-P function
;;;

(subtest "STRUCTURE-ACCESSOR-P"
  (is (structure-accessor-p 'float3-x) t "basic case 1")
  (is (structure-accessor-p 'float4-w) t "basic case 2")
  (is (structure-accessor-p 'float3-w) nil "basic case 3")
  (is (structure-accessor-p 'foo-bar) t "user struct: foo case 1")
  (is (structure-accessor-p 'foo-baz) t "user struct: foo case 2")
  (is (structure-accessor-p 'foo-boo) nil "user struct: foo case 3"))


;;;
;;; test STRUCTURE-ACCESSOR-OPENCL-ACCESSOR function
;;;

(subtest "STRUCTURE-ACCESSOR-OPENCL-ACCESSOR"
  (is (structure-accessor-opencl-accessor 'float3-x) "x" "basic case 1")
  (is (structure-accessor-opencl-accessor 'float4-w) "w" "basic case 2")
  (is-error (structure-accessor-opencl-accessor 'float3-w) simple-error
            "ACCESSOR which is not an invalid accessor.")
  (is (structure-accessor-opencl-accessor 'foo-bar) "bar" "user struct: foo case 1")
  (is (structure-accessor-opencl-accessor 'foo-baz) "baz" "user struct: foo case 2")
  (is-error (structure-accessor-opencl-accessor 'foo-boo) simple-error
            "ACCESSOR which is not an invalid accessor."))



;;;
;;; test STRUCTURE-ACCESSOR-RETURN-TYPE function
;;;

(subtest "STRUCTURE-ACCESSOR-RETURN-TYPE"
  (is (structure-accessor-return-type 'float3-x) 'float "basic case 1")
  (is (structure-accessor-return-type 'double4-w) 'double "basic case 2")
  (is-error (structure-accessor-return-type 'float3-w) simple-error
            "ACCESSOR which is not an invalid accessor.")
  (is (structure-accessor-return-type 'foo-bar) 'int "user struct: foo case 1")
  (is (structure-accessor-return-type 'foo-baz) 'float "user struct: foo case 2")
  (is-error (structure-accessor-return-type 'foo-boo) simple-error
            "ACCESSOR which is not an invalid accessor."))


;;;
;;; test ARRAY-TYPE-BASE function
;;;

(subtest "ARRAY-TYPE-BASE"
  (is (array-type-base 'int*) 'int
      "basic case 1")
  (is (array-type-base 'int**) 'int
      "basic case 2"))


;;;
;;; test ARRAY-TYPE-DIMENSION function
;;;

(subtest "ARRAY-TYPE-DIMENSION"
  (is (array-type-dimension 'int*) 1
      "basic case 1")
  (is (array-type-dimension 'int**) 2
      "basic case 2"))


(finalize)
