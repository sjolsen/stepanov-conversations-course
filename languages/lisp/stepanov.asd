(defpackage :stepanov
  (:use :cl :asdf)
  (:export :quicksort :test))
(in-package :stepanov)

(defsystem stepanov
  :name "stepanov"
  :author "Stuart Olsen"
  :description "Common Lisp port of quicksort tuned for SBCL"
  :serial t
  :components ((:file "typed")
               (:file "fast")
               (:file "quicksort")
               (:file "sort64")))
