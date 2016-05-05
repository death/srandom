;;;; +----------------------------------------------------------------+
;;;; | SRANDOM                                                        |
;;;; +----------------------------------------------------------------+

;;;; Sanity check for RANDOM-INTEGER

(in-package #:srandom)

(defun sanity-check (max &optional (size 10000))
  "Sample SIZE random integers up to MAX and return their counts in a
vector.  The result can then be analyzed."
  (with-random-octet-stream (stream)
    (let ((table (make-hash-table))
          (counts (make-array max :initial-element 0)))
      (loop repeat size
            do (incf (gethash (random-integer max stream) table 0)))
      (maphash (lambda (n count)
                 (if (or (minusp n) (>= n max))
                     (error "RANDOM-INTEGER returned bogus result ~D for MAX ~D." n max)
                     (setf (aref counts n) count)))
               table)
      counts)))
