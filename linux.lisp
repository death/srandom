;;;; +----------------------------------------------------------------+
;;;; | SRANDOM                                                        |
;;;; +----------------------------------------------------------------+

;;;; Linux random source

(in-package #:srandom)

(defun call-with-random-octet-stream (function)
  "Call FUNCTION with a stream representing a
cryptographically-acceptable source of random octets."
  (with-open-file (stream "/dev/urandom"
                          :direction :input
                          :element-type '(unsigned-byte 8))
    (funcall function stream)))
