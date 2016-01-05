(defpackage #:manual-shuffle.gui
  (:use #:cl+qt)
  (:export
   #:main))

(in-package #:manual-shuffle.gui)
(in-readtable :qtools)

(defvar +max-cards-number+ 999)
(defvar +output-font-family+ "Anivers")
(defvar +output-font-point-size+ 30)
(defvar +shuffling-timelimit+ 0.1)

(define-widget w (QWidget)
  ())

(define-subwidget (w button1) (q+:make-qpushbutton "Manual shuffle"))

(define-subwidget (w input-n) (q+:make-qlineedit)
  (setf (q+:placeholder-text input-n) "N")
  (setf (q+:validator input-n) (q+:make-qintvalidator 1 +max-cards-number+ w)))

(define-subwidget (w toggle-v2) (q+:make-qcheckbox "v2")
  (setf (q+:checked toggle-v2) t))

(define-subwidget (w toggle-ja) (q+:make-qcheckbox "as japanese syllables"))

(define-subwidget (w toggle-opt) (q+:make-qcheckbox "minimize # heaps"))

(define-subwidget (w output) (q+:make-qtextedit)
  (setf (q+:font-family output) +output-font-family+)
  (setf (q+:font-point-size output) +output-font-point-size+))

(define-subwidget (w button2) (q+:make-qpushbutton "Random positions"))

(define-subwidget (w input-m) (q+:make-qlineedit)
  (setf (q+:placeholder-text input-n) "M")
  (setf (q+:validator input-n) (q+:make-qintvalidator 1 +max-cards-number+ w)))

(define-subwidget (w layout) (q+:make-qvboxlayout w)
  (let ((layout1 (q+:make-qhboxlayout)))
    (q+:add-widget layout1 button1)
    (q+:add-widget layout1 input-n)
    (q+:add-widget layout1 toggle-v2)
    (q+:add-widget layout1 toggle-ja)
    (q+:add-widget layout1 toggle-opt)
    (q+:add-widget layout1 button2)
    (q+:add-widget layout1 input-m)
    (q+:add-stretch layout1 1)
    (q+:add-layout layout layout1))
  (q+:add-widget layout output)
  (q+:set-focus input-n))

(define-signal (w do-manual-shuffle) (int bool bool bool))

(define-slot (w button1) ()
  (declare (connected button1 (pressed)))
  (declare (connected input-n (return-pressed)))
  (q+:select-all input-n)
  (q+:set-focus input-n)
  (let ((n (parse-integer (q+:text input-n) :junk-allowed t)))
    (when n (signal! w (do-manual-shuffle int bool bool bool)
                     n
                     (q+:is-checked toggle-v2)
                     (q+:is-checked toggle-ja)
                     (q+:is-checked toggle-opt)))))

(define-slot (w do-manual-shuffle) ((n int) (v2 bool) (ja bool) (opt bool))
  (declare (connected w (do-manual-shuffle int bool bool bool)))
  (let ((*print-pretty* nil))
    (cond
      ((not v2)
       (multiple-value-bind (heaps ordering perm)
           (if opt
               (manual-shuffle:shortest-shuffle n :timeout +shuffling-timelimit+)
               (manual-shuffle:manual-shuffle n))
         (if ja
             (setf heaps (manual-shuffle:list->ja heaps)
                   ordering (manual-shuffle:list->ja ordering))
             (setf heaps (manual-shuffle:list->rle heaps)))
         (let ((nheaps (reduce #'max heaps)))
           (setf (q+:text output)
                 (format nil "~a heap~:p: ~a.~%Ordering: ~a.~%Permutation: ~a."
                         nheaps heaps ordering perm)))))
      (t
       (multiple-value-bind (actions perm nheaps)
           (manual-shuffle:manual-shuffle-v2 n)
         (setf actions (manual-shuffle:shorten-actions-text actions))
         (setf (q+:text output)
               (format nil "~a heap~:p: ~a.~%Permutation: ~a."
                       (1+ nheaps) actions perm)))))))

(define-signal (w do-random-positions) (int int))

(define-slot (w button2) ()
  (declare (connected button2 (pressed)))
  (declare (connected input-m (return-pressed)))
  (let ((n (parse-integer (q+:text input-n) :junk-allowed t))
        (m (parse-integer (q+:text input-m) :junk-allowed t)))
    (when (and n m)
      (signal! w (do-random-positions int int) n m))))

(define-slot (w do-random-positions) ((n int) (m int))
  (declare (connected w (do-random-positions int int)))
  (let ((*print-pretty* nil))
    (setf (q+:text output) (princ-to-string (manual-shuffle:random-positions n m)))))

(defun main ()
  (with-main-window (w (make-instance 'w))))
