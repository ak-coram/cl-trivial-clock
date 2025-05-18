(defpackage #:trivial-clock
  (:use #:cl)
  (:export +universal-time-epoch-offset+ now))

(in-package #:trivial-clock)

(defconstant +universal-time-epoch-offset+
  (encode-universal-time 0 0 0 1 1 1970 0))

#+unix
(progn
  (cffi:defcstruct timespec
    (tv-sec :uint64)
    (tv-nsec :uint64))

  (cffi:defcfun clock-gettime :int
    (clock-id :int)
    (out-timespec (:pointer (:struct timespec)))))

#+windows
(progn
  (cffi:defcstruct filetime
    (low-dt :uint32)
    (hi-dt :uint32))

  (if (cffi:foreign-symbol-pointer "GetSystemTimePreciseAsFileTime")
      (cffi:defcfun ("GetSystemTimePreciseAsFileTime" get-system-time) :void
        (out-filetime (:pointer (:struct filetime))))
      (cffi:defcfun ("GetSystemTimeAsFileTime" get-system-time) :void
        (out-filetime (:pointer (:struct filetime))))))

(declaim (inline now)
         (ftype (function () (values (unsigned-byte 64)
                                     (integer 0 999999999)))
                now))
(defun now ()
  "Query OS for current wall-clock time

Returns number of seconds since the unix epoch and the number of
additional nanoseconds as a second value."
  #+unix
  (cffi:with-foreign-object (p-timespec '(:struct timespec))
    (clock-gettime 0 p-timespec) ;; Use CLOCK_REALTIME
    (cffi:with-foreign-slots ((tv-sec tv-nsec)
                              p-timespec
                              (:struct timespec))
      (values tv-sec tv-nsec)))
  #+windows
  (cffi:with-foreign-object (p-filetime '(:struct filetime))
    (get-system-time p-filetime)
    (cffi:with-foreign-slots ((low-dt hi-dt)
                              p-filetime
                              (:struct filetime))
      (multiple-value-bind (seconds 100nanos)
          (floor (logior (ash hi-dt 32) low-dt)
                 10000000)
        (values (- seconds 11644473600) (* 100nanos 100)))))
  #+nil
  (values (- (get-universal-time)
             +universal-time-epoch-offset+)
          0))
