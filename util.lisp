;;;; +----------------------------------------------------------------+
;;;; | SRANDOM                                                        |
;;;; +----------------------------------------------------------------+

;;;; Internal utilities

(in-package #:srandom)

;; Consider whether we want to memoize this function.
(defun unsigned-properties (n)
  "Return the number of octets needed to get an integer in the range
[0, n), as well as the mask to remove unneeded bits from the result."
  (let* ((len (integer-length n))
         (b (mod len 8)))
    (values (floor (+ 7 len) 8)
            (1- (ash 1 (if (zerop b) 8 b))))))

(defun make-octet-vector (n)
  "Return an octet vector of size N."
  (make-array n :element-type '(unsigned-byte 8)))

(defun read-full (seq stream)
  "Fill the whole of SEQ with octets from STREAM.  If that is not
possible, signal an error."
  (if (= (read-sequence seq stream) (length seq))
      seq
      (error "Could not read ~D octets from stream." (length seq))))

(defun octets-to-unsigned (vector)
  "Return the Little-Endian unsigned integer representation of the
octets in VECTOR"
  (do ((n 0)
       (p (* 8 (1- (length vector))) (- p 8))
       (i 0 (1+ i)))
      ((= i (length vector)) n)
    (setf (ldb (byte 8 p) n) (aref vector i))))
