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
      (let ((vector (make-octet-vector n)))
        (read-full vector stream))))

;; Inspired by Golang's crypto/rand implementation.
(defun random-integer (max &optional stream)
  "Return a random integer in the range [0, max)."
  (check-type max (integer 1))
  (if (null stream)
      (with-random-octet-stream (stream)
        (random-integer max stream))
      (multiple-value-bind (size mask)
          (unsigned-properties max)
        (let ((vector (make-octet-vector size)))
          (loop (read-full vector stream)
                (setf (aref vector 0) (logand (aref vector 0) mask))
                (let ((n (octets-to-unsigned vector)))
                  (when (< n max)
                    (return-from random-integer n))))))))