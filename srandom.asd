;;;; +----------------------------------------------------------------+
;;;; | SRANDOM                                                        |
;;;; +----------------------------------------------------------------+

;;;; System definition

;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(asdf:defsystem #:srandom
  :description "Generate random numbers in a cryptographically-acceptable way."
  :author "death <github.com/death>"
  :license "MIT"
  :depends-on ()
  :serial t
  :components
  ((:file "packages")
   #+linux (:file "linux")
   (:file "util")
   (:file "external")
   (:file "sanity")))
