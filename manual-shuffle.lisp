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

(defun reverse&group-ascending (seq)
  "(5 3 4 2 1) -> ((1 2 4) (3 5))
(Reversed because it is more convenient to put cards on top of formed heaps."
  (let ((groups (list)))
    (dolist (el seq)
      (if (and groups (< el (caar groups)))
          (push el (car groups))
          (push (list el) groups)))
    groups))

(defun inv-perm (perm &key (predicate '<) (key 'identity))
  "The permutation of the enumerated sequence 0A 1B 2C to B0 C1 A2 can be represented in two ways:
- 0A2 1B0 2C1 -> 2 0 1 when matched to the source sequence;
- 1B0 2C1 0A2 -> 1 2 0 when matched to the target sequence.
This function converts between the two representations.
KEY should extract permutation index from an element of PERM.
PREDICATE should be true when a pair of elements of PERM is not permuted and the reversed pair is permuted."
  (flet ((sort-key (v) (funcall key (cdr v))))
    (apply #'values (denumerate (stable-sort (enumerate perm) predicate :key #'sort-key)))))

(defun manual-shuffle (n)
  (let* ((perm (coerce (random-permutation n) 'list))
         (heaps (reverse&group-ascending perm)))
    (let+ (((&values ordering heaps) (inv-perm heaps :key #'first)))
      (values (loop for i below n collect (1+ (position-if (lambda (heap) (member i heap)) heaps)))
              (mapcar '1+ (inv-perm ordering))
              (mapcar '1+ perm)))))

(defun simple-shuffle (n m)
  (loop
    (multiple-value-bind (seq ordering) (manual-shuffle n)
      (when (<= (length ordering) m)
        (return (values seq ordering))))))
