(in-package :stepanov)
(optimize-aggressively)

(declaim (inline unguarded-linear-insert
                 unguarded-insertion-sort
                 linear-insert
                 insertion-sort
                 median-of-3))

(typed:defun unguarded-linear-insert ((data vfixnum) (last fixnum) (value fixnum)) nil
  (loop
     for to downfrom last
     for from downfrom (1- last)
     for element = (aref data from)
     while (< value element)
     do (setf (aref data to) element)
     finally (setf (aref data to) value)))

(typed:defun unguarded-insertion-sort ((data vfixnum) (first fixnum) (last fixnum)) nil
  (loop
     for i from first below last
     do (unguarded-linear-insert data i (aref data i))))

(typed:defun linear-insert ((data vfixnum) (first fixnum) (last fixnum) (value fixnum)) nil
  (if (< value (aref data first))
      (loop
         for to from last above first
         for from = (1- to)
         do (setf (aref data to) (aref data from))
         finally (setf (aref data first) value))
      (unguarded-linear-insert data last value)))

(typed:defun insertion-sort ((data vfixnum) (first fixnum) (last fixnum)) nil
  (unless (= first last)
    (loop
       for i from (1+ first) below last
       do (linear-insert data first i (aref data i)))))

(typed:defun median-of-3 ((a fixnum) (b fixnum) (c fixnum)) fixnum
  (if (< a b)
      (cond ((< b c) b)
            ((< a c) c)
            (t a))
      (cond ((< a c) a)
            ((< b c) c)
            (t b))))

(typed:defun unguarded-partition ((data vfixnum) (first fixnum) (last fixnum) (pivot fixnum)) fixnum
  (typed:labels ((next-first ((first fixnum)) fixnum
                   (loop
                      for next from first
                      while (< (aref data next) pivot)
                      finally (return next)))
                 (next-last ((last fixnum)) fixnum
                   (loop
                      for next downfrom last
                      while (< pivot (aref data next))
                      finally (return next))))
    (loop
       for a = (next-first first) then (next-first (1+ a))
       for b = (next-last (1- last)) then (next-last (1- b))
       while (< a b)
       do (swapf (aref data a) (aref data b))
       finally (return-from unguarded-partition a))))

(typed:defun quicksort-loop ((data vfixnum) (first fixnum) (last fixnum) (threshold fixnum)) nil
  (loop
     for len of-type fixnum = (- last first)
     while (> len threshold)
     for middle = (floor (+ first last) 2)
     for pivot = (median-of-3 (aref data first)
                              (aref data middle)
                              (aref data (1- last)))
     for cut = (unguarded-partition data first last pivot)
     if (> cut middle)
       do (quicksort-loop data cut last threshold) and
       do (setf last cut)
     else
       do (quicksort-loop data first cut threshold) and
       do (setf first cut)))

(typed:defun quicksort ((data vfixnum) (first fixnum) (last fixnum)) nil
  (let ((len (- last first))
        (threshold 16))
    (if (<= len threshold)
        (insertion-sort data first last)
        (let ((middle (+ first threshold)))
          (quicksort-loop data first last threshold)
          (insertion-sort data first middle)
          (unguarded-insertion-sort data middle last)))))
