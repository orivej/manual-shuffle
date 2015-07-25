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

(define-subwidget (w button1) (q+:make-qpushbutton "Manual shuffle" w))

(define-subwidget (w input1-n) (q+:make-qlineedit w)
  (setf (q+:placeholder-text input1-n) "N")
  (setf (q+:validator input1-n) (q+:make-qintvalidator 1 +max-cards-number+ w)))

(define-subwidget (w toggle1-ja) (q+:make-qcheckbox "as japanese syllables" w)
  (setf (q+:checked toggle1-ja) t))

(define-subwidget (w toggle1-opt) (q+:make-qcheckbox "minimize # heaps" w))

(define-subwidget (w output) (q+:make-qtextedit w)
  (setf (q+:font-family output) +output-font-family+)
  (setf (q+:font-point-size output) +output-font-point-size+))

(define-subwidget (w layout) (q+:make-qvboxlayout w)
  (let ((layout1 (q+:make-qhboxlayout w)))
    (q+:add-widget layout1 button1)
    (q+:add-widget layout1 input1-n)
    (q+:add-widget layout1 toggle1-ja)
    (q+:add-widget layout1 toggle1-opt)
    (q+:add-stretch layout1 2)
    (q+:add-layout layout layout1))
  (q+:add-widget layout output)
  (q+:set-focus input1-n))

(define-signal (w do-manual-shuffle) (int bool bool))

(define-slot (w button1) ()
  (declare (connected button1 (pressed)))
  (declare (connected input1-n (return-pressed)))
  (let ((v (parse-integer (q+:text input1-n) :junk-allowed t)))
    (when v (signal! w (do-manual-shuffle int bool bool)
                     v
                     (q+:is-checked toggle1-ja)
                     (q+:is-checked toggle1-opt)))))

(define-slot (w do-manual-shuffle) ((n int) (ja bool) (opt bool))
  (declare (connected w (do-manual-shuffle int bool bool)))
  (multiple-value-bind (heaps ordering perm)
      (if opt
          (manual-shuffle:shortest-shuffle n :timeout +shuffling-timelimit+)
          (manual-shuffle:manual-shuffle n))
    (let ((*print-pretty* nil)
          (nheaps (reduce #'max heaps)))
      (when ja
        (setf heaps (manual-shuffle:list->ja heaps)
              ordering (manual-shuffle:list->ja ordering)))
      (setf (q+:text output) (format nil "~a heap~:p: ~a.~%Ordering: ~a.~%Permutation: ~a."
                                     nheaps heaps ordering perm)))))

(defun main ()
  (with-main-window (w (make-instance 'w))))
