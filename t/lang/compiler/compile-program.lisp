#|
  This file is a part of oclcl project.
  Copyright (c) 2012 Masayuki Takagi (kamonama@gmail.com)
                2015 gos-k (mag4.elan@gmail.com)
|#

(in-package :cl-user)
(defpackage oclcl-test.lang.compiler.compile-program
  (:use :cl :prove
        :oclcl.lang.type
        :oclcl.lang.program
        :oclcl.lang.compiler.compile-program
        :oclcl.lang.user-type))
(in-package :oclcl-test.lang.compiler.compile-program)

(plan nil)


;;;
;;; test COMPILE-PROGRAM funcition
;;;

(subtest "COMPILE-PROGRAM"
  (let ((program (make-program)))
    (setf (symbol-user-struct 'foo)
          (make-instance 'ocl-struct :ocl-name 'foo :slots '(bar baz) :accessors '(foo-bar foo-baz))
          (symbol-user-struct-slot 'foo-bar)
          (make-instance 'ocl-struct-slot :struct 'foo :type 'int :accessor 'foo-bar :ocl-name 'bar)
          (symbol-user-struct-slot 'foo-baz)
          (make-instance 'ocl-struct-slot :struct 'foo :type 'float :accessor 'foo-baz :ocl-name
                         'baz))
    (program-add-user-struct nil 'foo)
    (program-define-memory program 'a :constant 1)
    (program-define-memory program 'b :global 1.0f0)
    (program-define-function program 'foo 'void '((x int*))
                             '((set (aref x 0) (bar 1))
                               (return)))
    (program-define-function program 'bar 'int '((x int)) '((return x)))
    (program-define-function program 'baz 'void '() '((return)))
    (program-define-function program 'boo 'float '((x foo)) '((return (foo-baz x))))
    (compile-program program)
    (is (compile-program program)
        "

/**
 *  Struct definitions
 */

typedef struct foo { int bar; float baz; } foo;


/**
 *  Memory objects
 */

__constant int oclcl_test_lang_compiler_compile_program_a = 1;
__global float oclcl_test_lang_compiler_compile_program_b = 1.0f;


/**
 *  Kernel function prototypes
 */

float oclcl_test_lang_compiler_compile_program_boo(foo x);
__kernel void oclcl_test_lang_compiler_compile_program_baz();
int oclcl_test_lang_compiler_compile_program_bar(int x);
__kernel void oclcl_test_lang_compiler_compile_program_foo(__global int* x);


/**
 *  Kernel function definitions
 */

float oclcl_test_lang_compiler_compile_program_boo(foo x)
{
  return x.baz;
}

__kernel void oclcl_test_lang_compiler_compile_program_baz()
{
  return;
}

int oclcl_test_lang_compiler_compile_program_bar(int x)
{
  return x;
}

__kernel void oclcl_test_lang_compiler_compile_program_foo(__global int* x)
{
  x[0] = oclcl_test_lang_compiler_compile_program_bar(1);
  return;
}
"
        "basic case 1")))


(finalize)
