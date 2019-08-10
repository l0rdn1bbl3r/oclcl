(defpackage oclcl.lang.user-type
  (:use :cl :oclcl.lang.type :oclcl.lang.program :alexandria)
  (:export :define-multi-citizen-struct :program-user-structs :program-add-user-struct))
(in-package :oclcl.lang.user-type)

(defparameter *program-user-struct-table* (make-hash-table))

(defun program-user-structs (program-name)
  "Return the names (as symbols) of the user defined structs that are defined in PROGRAM."
  (gethash program-name *program-user-struct-table*))

(defun program-add-user-struct (program-name user-struct-name)
  "Add USER-STRUCT-NAME to the list of user defined structs in the programm with name
PROGRAM-NAME (a symbol) so its definition will be added when the program is compiled."
  (check-type program-name symbol)
  (pushnew user-struct-name (gethash program-name *program-user-struct-table*)))

(defmacro define-multi-citizen-struct (name slot-descriptions &key lisp-struct-p
                                                                lisp-name
                                                                with-foreign-slots-macro-p
                                                                foreign-slot-accessors-p
                                                                foreign-prefix)
  "Define the foreign struct NAME both on the host and the device and make sure it can be used in
OCLCL Code. Optionally define a lisp struct of the same or another name and/or a wrapper to access
the foreign slot-values from lisp in a lispy way without creating a lisp struct and copy the from
foreign memory.

SLOT-DESCRIPTIONS must be a list of lists. Each of the form

SLOT-DESCRIPTION::= (slot-name :cffi-type cffi-type :oclcl-type oclcl-type) |
                    (slot-name :cffi-type cffi-type :oclcl-type oclcl-type :lisp-type lisp-type)

If LISP-STRUCT-P is non-nil a lisp struct with the given slots and a boa constructor MAKE-NAME will
be defined. This struct will have the name LISP-NAME - if supplied - or NAME otherwise.

If WITH-FOREIGN-SLOTS-MACRO-P is non-nil a macro WITH-NAME-SLOTS is defined that works like
WITH-SLOTS and takes a foreign pointer as second argument. 

If FOREIGN-SLOT-ACCESSORS-P is non-nil a macro NAME-SLOTNAME optionaly prefixed by FOREIGN-PREFIX
will be defined for each foreign slot. It takes a foreign pointer and expands to an setf-able "
  (when (and foreign-slot-accessors-p lisp-struct-p (not lisp-name) (not foreign-prefix))
    (error "The names for the accessors to the lisp struct and the foreign struct can't have the
same names. Supply LISP-NAME and/or FOREIGN-PREFIX."))
  (let* ((cffi-class-name (symbolicate name '-c))
         (slot-names (mapcar #'car slot-descriptions))
         (accesor-names (mapcar (lambda (slot-name) (symbolicate name '- slot-name))
                                slot-names))
         (ptr-name (symbolicate 'foreign-ptr-to- name))
         (macro-name (symbolicate 'with- name '-slots)))
    `(progn
       (setf (symbol-user-struct ',name) (make-instance 'ocl-struct :ocl-name ',name
                                                                    :slots ',slot-names
                                                                    :accessors ',accesor-names))
       (program-add-user-struct (program-name *program*) ',name)
       ,@(mapcar (lambda (acc slot)
                   (let* ((slot-name (car slot))
                          (type (getf (cdr slot) :oclcl-type)))
                     `(setf (symbol-user-struct-slot ',acc)
                            (make-instance 'ocl-struct-slot :struct ',name
                                                            :type ',type
                                                            :accessor ',acc
                                                            :ocl-name ',slot-name))))
                 accesor-names
                 slot-descriptions)
       (cffi:defcstruct (,name :class ,cffi-class-name)
         ,@(mapcar (lambda (slot) `(,(car slot) ,(getf (cdr slot) :cffi-type)))
                   slot-descriptions))
       ,@(when lisp-struct-p
           (let* ((lisp-class-name (or name lisp-name))
                  (slot-vars (make-gensym-list (length slot-descriptions) "slot"))
                  (slot-bindings (mapcar (lambda (var slot) `(,var ,slot)) slot-vars slot-names))
                  (slot-settings (mappend (lambda (a b) `(,a ,b)) slot-names slot-vars))
                  (constructor-name (symbolicate 'make- (or lisp-name name))))
             `((defstruct (,(or lisp-name name) (:constructor ,constructor-name ,slot-names))
                 ,@(mapcar (lambda (slot) `(,(car slot) nil :type ,(getf (cdr slot) :lisp-type)))
                    slot-descriptions))
               (defmethod cffi:translate-into-foreign-memory ((value ,lisp-class-name) (type ,cffi-class-name) ptr)
                 (cffi:with-foreign-slots (,slot-names ptr (:struct ,name))
                   (with-slots ,slot-bindings value
                     (setf ,@slot-settings))))
               (defmethod cffi:expand-into-foreign-memory ((value ,lisp-class-name) (type ,cffi-class-name) ptr)
                 `(cffi:with-foreign-slots (,',slot-names ,ptr (:struct ,',name))
                    (with-slots ,',slot-bindings ,value
                      (setf ,@',slot-settings))))
               (defmethod cffi:translate-from-foreign (ptr (type ,cffi-class-name))
                 (cffi:with-foreign-slots (,slot-names ptr (:struct ,name))
                   (,constructor-name ,@slot-names)))
               (defmethod cffi:expand-from-foreign (ptr (type ,cffi-class-name))
                 `(cffi:with-foreign-slots (,',slot-names ,ptr (:struct ,',name))
                    (,',constructor-name ,@',slot-names))))))
       ,@(when with-foreign-slots-macro-p
           `((defmacro ,macro-name (slots ,ptr-name &body body) 
               (let ((unknown-slot (set-difference slots ',slot-names :key (lambda (x)
                                                                             (if (listp x)
                                                                                 (cadr x)
                                                                                 x)))))
                 (when unknown-slot
                   (error "Struct ~a has no slot named ~a. Slots must be a subset of ~a."
                          ',name unknown-slot ',slot-names)))
               (flet ((ensure-binding (slot)
                        (etypecase slot
                          (list
                           (destructuring-bind (var? slot-name) slot
                             (etypecase var?
                               (list
                                (unless (eql :pointer (car var?))
                                  (error "Malformed slot binding ~a" var?))
                                slot)
                               (keyword
                                (unless (eql :pointer var?)
                                  (error "Malformed slot binding ~a" var?))
                                (list slot (cadr slot)))
                               (symbol slot))))
                          (symbol (list slot slot)))))
                 (once-only (,ptr-name)
                   (let ((bindings (mapcar (lambda (binding)
                                             (destructuring-bind (var slot-name)
                                                 (ensure-binding binding)
                                               (if (listp var)
                                                   `(,(cadr var) (cffi:foreign-slot-pointer
                                                                  ,,ptr-name
                                                                  '(:struct ,',name)
                                                                  ',slot-name))
                                                   `(,var (cffi:foreign-slot-value
                                                           ,,ptr-name
                                                           '(:struct ,',name)
                                                           ',slot-name)))))
                                           slots)))
                     `(symbol-macrolet ,bindings
                        ,@body)))))))
       ,@(when foreign-slot-accessors-p
           (mappend (lambda (slot)
                      (let ((acc-name (symbolicate (or foreign-prefix "") name '- slot)))
                        `((defun ,acc-name (,ptr-name)
                            (cffi:foreign-slot-value ,ptr-name '(:struct ,name) ',slot))
                          (defun (setf ,acc-name) (new-value ,ptr-name)
                            (setf (cffi:foreign-slot-value ,ptr-name '(:struct ,name) ',slot)
                                  new-value)))))
                    slot-names))
       (values))))
