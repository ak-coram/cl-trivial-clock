;;;; trivial-clock.asd

(asdf:defsystem #:trivial-clock
  :description
  "Common Lisp library to get accurate wall-clock times on multiple platforms"
  :author "Ákos Kiss <ak@coram.pub>"
  :license "MIT License"
  :depends-on (#:cffi)
  :components ((:file "trivial-clock"))
  :in-order-to ((test-op (test-op "trivial-clock/test"))))

(asdf:defsystem #:trivial-clock/test
  :depends-on (#:trivial-clock
               #:fiveam)
  :components ((:file "trivial-clock-test"))
  :perform (test-op (o c) (symbol-call :fiveam '#:run! :trivial-clock)))

(asdf:defsystem #:trivial-clock/*
  :depends-on (#:trivial-clock
               #:trivial-clock/test))
