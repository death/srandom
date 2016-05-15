;;;; +----------------------------------------------------------------+
;;;; | SRANDOM                                                        |
;;;; +----------------------------------------------------------------+

;;;; Implementation of external interface

(in-package #:srandom)

(unless (fboundp 'call-with-random-octet-stream)
  (error "SRANDOM is not implemented on this platform; patches welcome!"))

(defmacro with-random-octet-stream ((stream-var) &body forms)
  "Evaluate FORMS with STREAM-VAR bound to a stream representing a
cryptographically-acceptable source of random octets."
  `(call-with-random-octet-stream (lambda (,stream-var) ,@forms)))

(defun random-octets (n &optional stream)
  "Return an octet vector filled with N random octets."
  (check-type n (integer 0))
  (if (null stream)
      (with-random-octet-stream (stream)
        (random-octets n stream))
      (read-full (make-octet-vector n) stream)))

;; Inspired by Golang's crypto/rand implementation.  Modified so as to
;; only read a new most-significant-byte (rather than the whole vector
;; anew) when our search for a random integer within the expected
;; bounds does not bear fruit.
(defun random-integer (max &optional stream)
  "Return a random integer in the range [0, MAX)."
  (check-type max (integer 1))
  (if (null stream)
      (with-random-octet-stream (stream)
        (random-integer max stream))
      (multiple-value-bind (size mask)
          (unsigned-properties max)
        (let ((vector (make-octet-vector size)))
          (read-full vector stream)
          (loop (setf (aref vector 0) (logand (aref vector 0) mask))
                (let ((n (octets-to-unsigned vector)))
                  (when (< n max)
                    (return-from random-integer n)))
                (setf (aref vector 0) (read-byte stream)))))))
