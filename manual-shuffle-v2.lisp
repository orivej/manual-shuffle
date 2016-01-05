(in-package #:manual-shuffle)

(defclass* state ()
  ((next-heap :type integer :initform -1)
   (empty-heaps :type (list integer) :initform nil)
   (target-heaps :type (array integer))))

(defun make-state (n)
  (make-instance 'state
                 :target-heaps (make-array n
                                           :element-type 'integer
                                           :initial-element -1)))

(defun manual-shuffle-v2 (n &key perm)
  (let+ ((perm (coerce (or perm (random-permutation n)) 'list))
         (st (make-state n))
         (actions (qlist))
         ((&flet in-deck (i)
            (when (<= 0 i (1- n))
              i)))
         ((&flet heap-at (position)
            (let ((heap (aref (target-heaps-of st) position)))
              (when (/= heap -1)
                heap))))
         ((&flet (setf heap-at) (heap position)
            (setf (aref (target-heaps-of st) position) heap)))
         ((&flet allocate-heap ()
            (if (empty-heaps-of st)
                (pop (empty-heaps-of st))
                (incf (next-heap-of st)))))
         ((&flet free-heap (heap)
            (push heap (empty-heaps-of st))))
         ((&flet set-heap-at (position heap)
            (let ((old-heap (heap-at position)))
              (loop
                :while (and position (eql old-heap (heap-at position)))
                :do (progn
                      (setf (heap-at position) heap)
                      (setf position (in-deck (1- position)))))))))
    (dolist (target-position perm (values (rest actions) perm))
      (let+ ((next-position (in-deck (1+ target-position)))
             (prev-position (in-deck (1- target-position)))
             ;; Select a heap for the card.
             (heap (or (and next-position (heap-at next-position))
                       (allocate-heap)))
             (prev-heap (and prev-position (heap-at prev-position))))
        (setf (heap-at target-position) heap)
        (lenqueue actions heap)
        ;; Put preceding heap (if exists) on top of this card.
        (when prev-heap
          (set-heap-at prev-position heap)
          (free-heap prev-heap)
          (lenqueue actions (list :stack prev-heap)))))))
