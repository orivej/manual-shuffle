(asdf:defsystem manual-shuffle
  :depends-on (let-plus rutils local-time hu.dwim.defclass-star qlist)
  :serial t
  :components ((:file "manual-shuffle")
               (:file "manual-shuffle-v2"))
  :in-order-to ((test-op (load-op :manual-shuffle-test)))
  :perform (test-op (o c) (symbol-call :fiveam :run! :manual-shuffle)))

(asdf:defsystem manual-shuffle-test
  :depends-on (manual-shuffle fiveam)
  :components ((:file "test")))
