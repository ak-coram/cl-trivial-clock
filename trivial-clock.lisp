(defpackage #:trivial-clock
  (:use #:cl)
  (:export now))

(in-package #:trivial-clock)

(defconstant +universal-time-epoch-offset+
  (encode-universal-time 0 0 0 1 1 1970 0))

#-windows
(progn
  (cffi:defcstruct timespec
    (tv-sec :uint64)
    (tv-nsec :uint64))

  (cffi:defcfun clock-gettime :int
    (clock-id :int)
    (out-timespec (:pointer (:struct timespec)))))

(defun now ()
  #-windows
  (cffi:with-foreign-object (p-timespec '(:pointer (:struct timespec)))
    (clock-gettime 0 p-timespec) ;; Use CLOCK_REALTIME
    (cffi:with-foreign-slots ((tv-sec tv-nsec)
                              p-timespec
                              (:struct timespec))
      (values tv-sec tv-nsec)))
  #+windows
  (values (- (get-universal-time)
             +universal-time-epoch-offset+)
          0))
