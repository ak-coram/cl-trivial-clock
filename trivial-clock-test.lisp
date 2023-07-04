;;;; trivial-clock-test.lisp

(defpackage #:trivial-clock-test
  (:use #:cl #:fiveam))

(in-package #:trivial-clock-test)

(def-suite :trivial-clock)
(in-suite :trivial-clock)

(test now
  (finishes (trivial-clock:now)))

