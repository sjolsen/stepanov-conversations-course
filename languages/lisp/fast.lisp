(in-package :stepanov)

(defmacro optimize-aggressively ()
  `(declaim (optimize (speed 3)
                      (space 0)
                      (safety 0)
                      (debug 0)
                      (compilation-speed 0))))

(deftype int63 () 'fixnum)
(deftype vint63 () '(simple-array int63))

(defmacro swapf (a b)
  `(rotatef ,a ,b))
