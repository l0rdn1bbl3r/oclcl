#|
  This file is a part of oclcl project.
  Copyright (c) 2012 Masayuki Takagi (kamonama@gmail.com)
                2015 gos-k (mag4.elan@gmail.com)
|#

(in-package :cl-user)
(defpackage oclcl.lang.compiler.compile-expression
  (:use :cl
        :oclcl.lang.util
        :oclcl.lang.type
        :oclcl.lang.syntax
        :oclcl.lang.environment
        :oclcl.lang.built-in
        :oclcl.lang.compiler.compile-data
        :oclcl.lang.compiler.type-of-expression)
  (:export :compile-expression))
(in-package :oclcl.lang.compiler.compile-expression)


;;;
;;; Compile expression
;;;

(defun compile-expression (form var-env func-env)
  (cond
    ((%macro-p form func-env) (compile-macro form var-env func-env))
    ((%symbol-macro-p form var-env)
     (compile-symbol-macro form var-env func-env))
    ((literal-p form) (compile-literal form))
    ((opencl-literal-p form) (compile-opencl-literal form))
    ((reference-p form) (compile-reference form var-env func-env))
    ((inline-if-p form) (compile-inline-if form var-env func-env))
    ((arithmetic-p form) (compile-arithmetic form var-env func-env))
    ((vector-literal-p form) (compile-vector-literal form var-env func-env))
    ((%user-struct-p form) (compile-user-struct form var-env func-env))
    ((function-p form) (compile-function form var-env func-env))
    (t (error "The value ~S is an invalid expression." form))))


;;;
;;; Macro
;;;

(defun %macro-p (form func-env)
  (oclcl.lang.compiler.type-of-expression::%macro-p form func-env))

(defun compile-macro (form var-env func-env)
  (let ((operator (macro-operator form))
        (operands (macro-operands form)))
    (let ((expander (function-environment-macro-expander func-env operator)))
      (let ((form1 (funcall expander operands)))
        (compile-expression form1 var-env func-env)))))


;;;
;;; Symbol macro
;;;

(defun %symbol-macro-p (form var-env)
  (oclcl.lang.compiler.type-of-expression::%symbol-macro-p form var-env))

(defun compile-symbol-macro (form var-env func-env)
  (let ((form1 (variable-environment-symbol-macro-expansion var-env form)))
    (compile-expression form1 var-env func-env)))


;;;
;;; Literal
;;;

(defun compile-literal (form)
  (cond
    ((bool-literal-p form) (compile-bool-literal form))
    ((int-literal-p form) (compile-int-literal form))
    ((float-literal-p form) (compile-float-literal form))
    ((double-literal-p form) (compile-double-literal form))
    ((string-literal-p form) (compile-string-literal form))
    (t (error "The value ~S is an invalid expression." form))))

(defun compile-bool-literal (form)
  (compile-bool form))

(defun compile-int-literal (form)
  (compile-int form))

(defun compile-float-literal (form)
  (compile-float form))

(defun compile-double-literal (form)
  (compile-double form))

(defun compile-string-literal (form)
  (compile-string form))

;;; OpenCL literal
;;;;

(defun compile-opencl-literal (form)
  (c-macro-name form))

;;;
;;; Reference
;;;

(defun compile-reference (form var-env func-env)
  (cond
    ((variable-reference-p form)
     (compile-variable-reference form var-env))
    ((structure-reference-p form)
     (compile-structure-reference form var-env func-env))
    ((vector-numeric-reference-p form)
     (compile-vector-numeric-reference form var-env func-env))
    ((array-reference-p form)
     (compile-array-reference form var-env func-env))
    (t (error "The value ~S is an invalid form." form))))


;;;
;;; Reference - Variable
;;;

(defun compile-variable-reference (form var-env)
  (cond
    ((variable-environment-variable-exists-p var-env form)
     (compile-symbol form))
    ((variable-environment-memory-exists-p var-env form)
     (variable-environment-memory-c-name var-env form))
    (t
     (error "The variable ~S not found." form))))

;;;
;;; Reference - Structure
;;;

(defun compile-structure-reference (form var-env func-env)
  (let ((accessor (structure-reference-accessor form))
        (expr (structure-reference-expr form)))
    ;; check if the expression part of structure reference has the
    ;; same type as accessor's structure
    (let ((structure (structure-from-accessor accessor))
          (expr-type (type-of-expression expr var-env func-env)))
      (unless (eq structure expr-type)
        (error "The structure reference ~S is invalid." form)))
    (let ((accessor1 (structure-accessor-opencl-accessor accessor))
          (expr1 (compile-expression expr var-env func-env)))
      (format nil "~A.~A" expr1 accessor1))))


;;;
;;; Reference - Array
;;;

(defun compile-array-indices (indices var-env func-env)
  (mapcar #'(lambda (index)
              (compile-expression index var-env func-env))
          indices))

(defun compile-array-reference (form var-env func-env)
  (let ((expr (array-reference-expr form))
        (indices (array-reference-indices form)))
    ;; check if the expression part of array reference has the same
    ;; dimension as the array reference
    (let ((expr-type (type-of-expression expr var-env func-env)))
      (unless (= (array-type-dimension expr-type) (length indices))
        (error "The dimension of array reference ~S is invalid." form)))
    (let ((expr1 (compile-expression expr var-env func-env))
          (indices1 (compile-array-indices indices var-env func-env)))
      (format nil "~A~{[~A]~}" expr1 indices1))))

;;;
;;; Reference - Numeric Vector Types Reference
;;;

(defun compile-vector-numeric-reference (form var-env func-env)
  (let ((expr (vector-numeric-reference-expr form))
        (idx (vector-numeric-reference-index form)))
    (let ((expr-type (type-of-expression expr var-env func-env)))
      (unless (and (< idx (vector-type-length expr-type))
                   (<= 0 idx))
        (error "The vector type reference ~S is invalid." form)))
    (let ((expr1 (compile-expression expr var-env func-env)))
      (format nil "~A.s~A" expr1 (if (<= idx 9)
                                     idx
                                     (svref #(a b c d e f) (- idx 10)))))))


;;;
;;; Inline-if
;;;

(defun compile-inline-if (form var-env func-env)
  (let ((test-expr (inline-if-test-expression form))
        (then-expr (inline-if-then-expression form))
        (else-expr (inline-if-else-expression form)))
    ;; check if the test part of inline-if expression has bool type
    (let ((test-type (type-of-expression test-expr var-env func-env)))
      (unless (eq test-type 'bool)
        (error "The type of expression ~S is invalid." form)))
    ;; check if the then part of inline-of expression has the same
    ;; type as the else part of it
    (let ((then-type (type-of-expression then-expr var-env func-env))
          (else-type (type-of-expression else-expr var-env func-env)))
      (unless (eq then-type else-type)
        (error "The type of expression ~S is invalid." form)))
    (let ((test-expr1 (compile-expression test-expr var-env func-env))
          (then-expr1 (compile-expression then-expr var-env func-env))
          (else-expr1 (compile-expression else-expr var-env func-env)))
      (format nil "(~A ? ~A : ~A)" test-expr1 then-expr1 else-expr1))))


;;;
;;; Arithmetic operations
;;;

(defun compile-arithmetic (form var-env func-env)
  (let ((operator (arithmetic-operator form))
        (operands (arithmetic-operands form)))
    (if (<= (length operands) 2)
        (compile-function form var-env func-env)
        (let ((operand-first (car operands))
              (operand-second (cadr operands))
              (operand-tail (cddr operands)))
          (let ((form1 `(,operator (,operator ,operand-first ,operand-second)
                                   ,@operand-tail)))
            (compile-expression form1 var-env func-env))))))


;;;
;;; Function application
;;;

(defun type-of-operands (operands var-env func-env)
  (oclcl.lang.compiler.type-of-expression::type-of-operands operands var-env
                                                              func-env))

(defun compile-operands (operands var-env func-env)
  (mapcar #'(lambda (operand)
              (compile-expression operand var-env func-env))
          operands))

(defun compile-function (form var-env func-env)
  (let ((operator (function-operator form)))
    (if (function-environment-function-exists-p func-env operator)
        (compile-user-defined-function form var-env func-env)
        (compile-built-in-function form var-env func-env))))

(defun compile-user-defined-function (form var-env func-env)
  (let ((operator (function-operator form))
        (operands (function-operands form)))
    ;; check if the operands have the same types as the operator expect
    (let ((expected (function-environment-function-argument-types
                       func-env operator))
          (actual (type-of-operands operands var-env func-env)))
      (unless (equal expected actual)
        (error "The function application ~S is invalid." form)))
    (let ((operator1 (function-environment-function-c-name func-env
                                                           operator))
          (operands1 (compile-operands operands var-env func-env)))
      (if operands1
          (format nil "~A(~{~A~^, ~})" operator1 operands1)
          (format nil "~A()" operator1)))))

(defun compile-built-in-function (form var-env func-env)
  (let ((operator (function-operator form))
        (operands (function-operands form)))
    (let ((operand-types (type-of-operands operands var-env func-env)))
      (if (built-in-function-infix-p operator operand-types)
          (compile-built-in-infix-function operator operands operand-types
                                           var-env func-env)
          (compile-built-in-prefix-function operator operands operand-types
                                            var-env func-env)))))

(defun compile-built-in-infix-function (operator operands operand-types
                                        var-env func-env)
    (let ((op (built-in-function-c-name operator operand-types))
          (lhe (compile-expression (car operands) var-env func-env))
          (rhe (compile-expression (cadr operands) var-env func-env)))
      (format nil "(~A ~A ~A)" lhe op rhe)))

(defun compile-built-in-prefix-function (operator operands operand-types
                                         var-env func-env)
    (let ((operator1 (built-in-function-c-name operator operand-types))
          (operands1 (compile-operands operands var-env func-env)))
      (if operands1
          (format nil "~A(~{~A~^, ~})" operator1 operands1)
          (format nil "~A()" operator1))))

;;;
;;; Vector Literal
;;;

(defun vector-type-length (vec-type) 
  (let ((str (string vec-type)))
    (values (parse-integer (remove-if #'alpha-char-p str))
            (intern (remove-if-not #'alpha-char-p str)))))

(defun compile-vector-literal (form var-env func-env)
  (flet ((%error (t1 t2 &optional base-type)
           (error "The types ~A and ~A (debug: base-type ~a) of the vector and its contents don't
  match." t1 t2 base-type)))
    (destructuring-bind (vec-type &rest contents) form
      (multiple-value-bind (n base-type) (vector-type-length vec-type)
        (case (length contents)
          (1 (let ((content-type (type-of-expression (car contents) var-env func-env)))
               (if (eq base-type content-type)
                   (format nil "~((~A)(~A)~)" vec-type (compile-expression (car contents)
                                                                           var-env func-env))
                   (%error vec-type content-type))))
          (t (let ((sum (reduce #'+ (mapcar (lambda (expr)
                                              (let ((type (type-of-expression expr var-env func-env)))
                                                (cond ((string= (symbol-name type)
                                                                (symbol-name base-type))
                                                       1)
                                                      ((scalar-type-p type)
                                                       (%error vec-type type base-type))
                                                      (t
                                                       (multiple-value-bind (m content-type)
                                                           (vector-type-length type)
                                                         (if (string= (symbol-name base-type)
                                                                      (symbol-name content-type))
                                                             m
                                                             (%error vec-type content-type)))))))
                                            contents))))
               (if (= n sum)
                   (format nil "~((~A)(~{~A~^, ~})~)" vec-type
                           (mapcar (lambda (expr)
                                     (compile-expression expr var-env func-env))
                                   contents))
                   (error "The lengths ~A and ~A of the vector and its contents don't match."
                          n sum)))))))))

;;;
;;; User defined structs
;;;

(defun %user-struct-p (form)
  (handler-case (symbol-user-struct (car form))
    (unbound-user-struct () nil)))

(defun compile-user-struct (form var-env func-env)
  (destructuring-bind (struct-type &rest init-args) form
    (let* ((struct (symbol-user-struct struct-type))
           (accs (struct-accessors struct)))
      (unless (= (length accs) (length init-args))
        (error "The lengths of the struct slots ~a and the init-args ~a don't match." accs init-args))
      (mapc (lambda (acc arg)
              (let* ((slot (symbol-user-struct-slot acc))
                     (name (struct-slot-ocl-name slot))
                     (slot-type (struct-slot-type slot))
                     (arg-type (type-of-expression arg var-env func-env)))
                (unless (eq slot-type arg-type)
                  (error "The types ~a of slot ~a and ~a of init-arg ~a don't match."
                         slot-type name arg-type arg))))
            accs
            init-args)
      (format nil "~((~a){~{~A~^, ~}}~)"
              struct-type
              (mapcar (lambda (expr)
                        (compile-expression expr var-env func-env))
                      init-args)))))
