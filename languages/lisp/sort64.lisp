(in-package :stepanov)
(optimize-aggressively)

(typed:defun nano-time () fixnum
  (* (expt 10 9)
     (/ (the (unsigned-byte 32) (get-internal-real-time))
        internal-time-units-per-second)))

(typed:defun time-sort ((data vfixnum) (buffer vfixnum) (size fixnum)) fixnum
  (typed:labels ((copy-data ((first fixnum)) nil
                   (loop
                      for i from 0 below size
                      for j from first
                      do (setf (aref buffer i) (aref data j)))))
    (let ((start-time (nano-time)))
      (loop
         for first from 0 below (length data) by size
         do (copy-data first)
         do (quicksort buffer 0 size))
      (- (nano-time) start-time))))

(defun test (&optional (min-size 8) (max-size #.(* 16 1024 1024)))
  (declare (type fixnum min-size max-size))
  (let ((data (make-array (list max-size) :element-type 'fixnum))
        (buffer (make-array (list max-size) :element-type 'fixnum)))
    ;; iota
    (loop
       for i from 0 below max-size
       do (setf (aref data i) i))
    ;; shuffle
    (loop
       for i from (1- max-size) downto 1
       for j = (random i)
       do (swapf (aref data i) (aref data j)))
    ;; test
    (format t "           ~12@A ~6@A ~6@A~%" "size" "time" "log2")
    (loop
       for lg from (log min-size 2)
       for size of-type fixnum = min-size then (* size 2) while (<= size max-size)
       for time = (time-sort data buffer size)
       for linear-time = (floor time max-size)
       for log-time = (/ linear-time lg)
       do (format t "    [Lisp] ~12D ~6D ~6,1F~%" size linear-time log-time))))
