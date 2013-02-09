(defpackage :scriptl.util
  (:use #:cl #:alexandria #:cl-ppcre)
  (:export

   ;; Regexp
   #:with-scanner #:print-matches #:grep-file #:do-grep-file
   #:do-grep

   ;; Streams
   #:ignore-eof
   #:read-lines #:read-lines-to-string))

(in-package :scriptl.util)
