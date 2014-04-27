(defpackage :typed
  (:shadow :defun :labels)
  (:export :defun :labels)
  (:use :cl))
(in-package :typed)

(cl:defun decompose-lambda-list (lambda-list)
  (loop
     for decl in lambda-list
     if (listp decl)
       collect (first decl) into names and
       collect (second decl) into types
     else
       collect decl into names and
       collect 'T into types
     finally (return (values names types))))

(cl:defun declare-types (names types)
  `(declare
    ,@(mapcar (lambda (name type) `(type ,type ,name))
              names types)))

(defmacro truly-the (type form)
  #+SBCL `(sb-ext:truly-the ,type ,form)
  #-SBCL form)

(defmacro defun (name lambda-list return-type &body body)
  (multiple-value-bind (params types)
      (decompose-lambda-list lambda-list)
    `(progn
       (declaim (type (function ,types ,return-type) ,name))
       (cl:defun ,name ,params
         ,(declare-types params types)
         ,(if return-type
              `(truly-the ,return-type (progn ,@body))
              `(progn ,@body (values)))))))

(cl:defun decompose-label-decl (decl)
  (destructuring-bind (name lambda-list return-type &body body)
      decl
    (declare (ignorable return-type))
    (multiple-value-bind (params types)
        (decompose-lambda-list lambda-list)
      `(,name ,params
         ,(declare-types params types)
         ,(if return-type
              `(truly-the ,return-type (progn ,@body))
              `(progn ,@body (values)))))))

(defmacro labels (decls &body body)
  `(cl:labels ,(mapcar #'decompose-label-decl decls)
     ,@body))
