(in-package :stepanov)
(optimize-aggressively)

(declaim (inline unguarded-linear-insert
                 unguarded-insertion-sort
                 linear-insert
                 insertion-sort
                 median-of-3))

(typed:defun unguarded-linear-insert ((data vint63) (last int63) (value int63)) nil
  (loop
     for to downfrom last
     for from downfrom (1- last)
     for element = (aref data from)
     while (< value element)
     do (setf (aref data to) element)
     finally (setf (aref data to) value)))

(typed:defun unguarded-insertion-sort ((data vint63) (first int63) (last int63)) nil
  (loop
     for i from first below last
     do (unguarded-linear-insert data i (aref data i))))

(typed:defun linear-insert ((data vint63) (first int63) (last int63) (value int63)) nil
  (if (< value (aref data first))
      (loop
         for to from last above first
         for from = (1- to)
         do (setf (aref data to) (aref data from))
         finally (setf (aref data first) value))
      (unguarded-linear-insert data last value)))

(typed:defun insertion-sort ((data vint63) (first int63) (last int63)) nil
  (unless (= first last)
    (loop
       for i from (1+ first) below last
       do (linear-insert data first i (aref data i)))))

(typed:defun median-of-3 ((a int63) (b int63) (c int63)) int63
  (if (< a b)
      (cond ((< b c) b)
            ((< a c) c)
            (t a))
      (cond ((< a c) a)
            ((< b c) c)
            (t b))))

(typed:defun unguarded-partition ((data vint63) (first int63) (last int63) (pivot int63)) int63
  (typed:labels ((next-first ((first int63)) int63
                   (loop
                      for next from first
                      while (< (aref data next) pivot)
                      finally (return next)))
                 (next-last ((last int63)) int63
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

(typed:defun quicksort-loop ((data vint63) (first int63) (last int63) (threshold int63)) nil
  (loop
     for len of-type int63 = (- last first)
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

(typed:defun quicksort ((data vint63) (first int63) (last int63)) nil
  (let ((len (- last first))
        (threshold 16))
    (if (<= len threshold)
        (insertion-sort data first last)
        (let ((middle (+ first threshold)))
          (quicksort-loop data first last threshold)
          (insertion-sort data first middle)
          (unguarded-insertion-sort data middle last)))))
