(defpackage #:manual-shuffle.gui
  (:use #:cl+qt))

(in-package #:manual-shuffle.gui)
(in-readtable :qtools)

(define-widget main-window (QWidget)
  ())

(define-subwidget (main-window button1) (q+:make-qpushbutton "Manual shuffle" main-window))

(define-subwidget (main-window input1-n) (q+:make-qlineedit main-window)
  (setf (q+:placeholder-text input1-n) "N"))

(define-subwidget (main-window output) (q+:make-qtextedit main-window))

(define-subwidget (main-window layout) (q+:make-qvboxlayout main-window)
  (let ((layout1 (q+:make-qhboxlayout)))
    (q+:add-widget layout1 button1)
    (q+:add-widget layout1 input1-n)
    ;; (q+:add-spacer-item layout1 (q+:make-qspaceritem 0 0))
    (q+:add-layout layout layout1))
  (q+:add-widget layout output))

(define-signal (main-window button1-clicked) (int))

(define-slot (main-window button1) ()
  (declare (connected button1 (pressed)))
  (signal! main-window (button1-clicked int) (parse-integer (q+:text input1-n))))

(define-slot (main-window button1-clicked) ((n int))
  (declare (connected main-window (button1-clicked int)))
  (setf (q+:text output)
        (princ-to-string (manual-shuffle:manual-shuffle n))))

(defun main ()
  (with-main-window (w (make-instance 'main-window))))
