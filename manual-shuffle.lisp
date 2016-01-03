(defpackage #:manual-shuffle
  (:use #:cl #:let-plus #:local-time)
  (:export #:manual-shuffle
           #:simple-shuffle
           #:shortest-shuffle
           #:random-positions
           #:list->ja
           #:list->rle))

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

(defun shortest-shuffle (n &key timeout)
  (let (best
        best-ordering-length
        (end-time (when timeout (timestamp+ (now) (round timeout (expt 10 -9)) :nsec))))
    (restart-case
        (loop
          (let* ((cur (multiple-value-list (manual-shuffle n)))
                 (cur-ordering-length (length (second cur))))
            (when (or (not best) (< cur-ordering-length best-ordering-length))
              (setf best cur
                    best-ordering-length cur-ordering-length)
              (unless end-time
                (print best-ordering-length)))
            (when (and end-time (timestamp> (now) end-time))
              (return (apply #'values best)))))
      (return-best ()
        :report "Return the shortest shuffle so far."
        (apply #'values best)))))

(defun random-positions (n m)
  "Find M random positions in a deck of N cards (to shuffle M cards back in the deck)."
  (sort (loop :repeat m :collect (random (+ n 1))) #'<))

(let* ((consonants "kstnhmrw")
       (vowels "aiueo")
       (syllables (loop :for c :in (list* "" (coerce consonants 'list))
                        :nconc (loop :for v :in (coerce vowels 'list)
                                     :collect (rutils:strcat c v))))
       (syllables (coerce syllables 'vector)))
  (defun n->ja (n)
    "Return a japanese syllable for a positive integer N."
    (if (<= 1 n (length syllables))
        (aref syllables (1- n))
        n)))

(defun list->ja (seq)
  (rutils:strjoin " " (mapcar #'n->ja seq)))

(defun fun->ja (fun &rest args)
  (let* ((vv (multiple-value-list (apply fun args)))
         (vv (mapcar #'list->ja vv)))
    (apply #'values vv)))

(defun list->rle (seq)
  (rle (list->next seq 1)))

(defun list->next (seq next)
  (mapcar (lambda (el)
            (if (/= el next) el
                (progn (incf next) :next)))
          seq))

(defun rle (seq)
  (labels ((val (el)
             (if (atom el) el (first el)))
           (cnt (el)
             (if (atom el) 1 (second el)))
           (recur (seq prev)
             (cond
               ((null seq)
                (list prev))
               ((eql (first seq) (val prev))
                (recur (rest seq) (list (first seq) (1+ (cnt prev)))))
               (t
                (list* prev (recur (rest seq) (first seq)))))))
    (recur (rest seq) (first seq))))
