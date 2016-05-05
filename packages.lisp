;;;; +----------------------------------------------------------------+
;;;; | SRANDOM                                                        |
;;;; +----------------------------------------------------------------+

;;;; Package definition

(defpackage #:srandom
  (:use #:cl)
  (:export
   #:with-random-octet-stream
   #:random-octets
   #:random-integer))
