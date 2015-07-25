(asdf:defsystem manual-shuffle.gui
  :defsystem-depends-on (qtools)
  :depends-on (manual-shuffle
               qtcore qtgui)
  :serial t
  :components ((:file "gui"))
  :build-operation "qt-program-op"
  :build-pathname "manual-shuffle"
  :entry-point "manual-shuffle.gui:main")

;;; build with (asdf:load-system :manual-shuffle.gui) (qtools::build-qt-system :manual-shuffle.gui :force t)
