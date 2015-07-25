(asdf:defsystem manual-shuffle.gui
  :depends-on (manual-shuffle
               qtools qtcore qtgui)
  :serial t
  :components ((:file "gui")))
