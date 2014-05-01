(in-package :stepanov)

(defmacro optimize-aggressively ()
  `(declaim (optimize (speed 3)
                      (space 0)
                      (safety 0)
                      (debug 0)
                      (compilation-speed 0))))

(deftype vfixnum () '(simple-array fixnum))

;;; Definition lifted from my SJ-LISP package
(defmacro swapf (place1 place2 &key second-value &environment env)
  "Swap the values of the two places, returning the first place's new value"
  (multiple-value-bind (vars1 vals1 store-vars1 writer-form1 reader-form1)
      (get-setf-expansion place1 env)
    (multiple-value-bind (vars2 vals2 store-vars2 writer-form2 reader-form2)
        (get-setf-expansion place2 env)
      (let ((tmp-values (loop
                           repeat (length store-vars1)
                           collecting (gensym))))
        `(let (,@(mapcar #'list vars1 vals1)
               ,@(mapcar #'list vars2 vals2))
           (multiple-value-bind ,tmp-values ,reader-form1
             (multiple-value-bind ,store-vars1 ,reader-form2
               (multiple-value-bind ,store-vars2 (values ,@tmp-values)
                 ,writer-form1
                 ,writer-form2
                 ,(if second-value
                      `(values ,@store-vars2)
                      `(values ,@store-vars1))))))))))
