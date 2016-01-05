(defpackage #:manual-shuffle-test
  (:use #:cl #:5am)
  (:local-nicknames (#:m #:manual-shuffle)))

(in-package #:manual-shuffle-test)

(def-suite :manual-shuffle)
(in-suite :manual-shuffle)

(test manual-shuffle-v2
  (is (equal '(0 1 (:stack 0) 1)
             (m::manual-shuffle-v2 3 :perm '(1 2 0)))))
