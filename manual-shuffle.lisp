(defpackage #:manual-shuffle
  (:use #:cl #:let-plus)
  (:export #:manual-shuffle))

(in-package #:manual-shuffle)

(defun random-permutation-list (n)
  (let ((seq (list* nil (loop for i below n collect i))))
    (loop for j from n downto 1
          for i = (random j)
          for tail = (nthcdr i seq)
          collect (car (cdr tail))
          do (setf (cdr tail) (cdr (cdr tail))))))

(defun random-permutation (n)
  (let ((a (make-array n :element-type 'fixnum)))
    (dotimes (i n a)
      (shiftf (aref a i) (aref a (random (1+ i))) i))))

(defun enumerate (seq)
  (loop for el in seq for i from 0 collect (cons i el)))

(defun denumerate (seq)
  (loop for (i . el) in seq
        collect i into is
        collect el into els
        finally (return (list is els))))

(defun manual-shuffle (n)
  (let* ((perm (coerce (random-permutation n) 'list))
         (heaps (list)))
    (dolist (el perm)
      (if (and heaps (> el (caar heaps)))
          (push el (car heaps))
          (push (list el) heaps)))
    (let+ ((heaps (nreverse (mapcar 'nreverse heaps)))
           ((ordering heaps) (denumerate (sort (enumerate heaps) '< :key 'cadr))))
      (values (loop for i below n collect (1+ (position-if (lambda (heap) (member i heap)) heaps)))
              (mapcar '1+ ordering)
              (mapcar '1+ perm)))))

(defun simple-shuffle (n m)
  (loop
    (multiple-value-bind (seq ordering) (manual-shuffle n)
      (when (<= (length ordering) m)
        (return (values seq ordering))))))
