(defpackage #:manual-shuffle.gui
  (:use #:cl+qt))

(in-package #:manual-shuffle.gui)
(in-readtable :qtools)

(define-widget w (QWidget)
  ())

(define-subwidget (w button1) (q+:make-qpushbutton "Manual shuffle" w))

(define-subwidget (w input1-n) (q+:make-qlineedit w)
  (setf (q+:placeholder-text input1-n) "N")
  (setf (q+:validator input1-n) (q+:make-qintvalidator 1 999 w)))

(define-subwidget (w toggle1-ja) (q+:make-qcheckbox "as japanese syllables" w))

(define-subwidget (w output) (q+:make-qtextedit w)
  (setf (q+:font-point-size output) 30))

(define-subwidget (w layout) (q+:make-qvboxlayout w)
  (let ((layout1 (q+:make-qhboxlayout w)))
    (q+:add-widget layout1 button1)
    (q+:add-widget layout1 input1-n)
    (q+:add-widget layout1 toggle1-ja)
    (q+:add-stretch layout1 2)
    (q+:add-layout layout layout1))
  (q+:add-widget layout output))

(define-signal (w do-manual-shuffle) (int bool))

(define-slot (w button1) ()
  (declare (connected button1 (pressed)))
  (declare (connected input1-n (return-pressed)))
  (let ((v (parse-integer (q+:text input1-n) :junk-allowed t)))
    (when v (signal! w (do-manual-shuffle int bool)
                     v
                     (q+:is-checked toggle1-ja)))))

(define-slot (w do-manual-shuffle) ((n int) (ja bool))
  (declare (connected w (do-manual-shuffle int bool)))
  (multiple-value-bind (heaps ordering perm) (manual-shuffle:manual-shuffle n)
    (when ja
      (setf heaps (manual-shuffle:list->ja heaps)
            ordering (manual-shuffle:list->ja ordering)))
    (let ((*print-pretty* nil))
      (setf (q+:text output) (format nil "~a.~%~a.~%~a." heaps ordering perm)))))

(defun main ()
  (with-main-window (w (make-instance 'w))))
