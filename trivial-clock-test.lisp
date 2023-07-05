;;;; trivial-clock-test.lisp

(defpackage #:trivial-clock-test
  (:use #:cl #:fiveam))

(in-package #:trivial-clock-test)

(def-suite :trivial-clock)
(in-suite :trivial-clock)

(test now
  (let ((unix-time (- (get-universal-time)
                      trivial-clock:+universal-time-epoch-offset+)))
    (multiple-value-bind (seconds nanos)
        (trivial-clock:now)
      (is (numberp seconds))
      (is (plusp seconds))
      (is (<= unix-time seconds))
      (is (numberp nanos))
      (is (not (minusp nanos)))
      (is (<= nanos 999999999)))))
