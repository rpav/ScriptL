;;; This uses UNIX-OPTIONS because it seems to do the most and be the
;;; least presumptive about how we're going to do it.  You can, of
;;; course, use whatever you like.
(asdf:load-system :unix-options)

(defpackage :scriptl.example
  (:use #:cl #:scriptl #:unix-options)
  (:export #:test-cmd))

(in-package :scriptl.example)

;;; You can use this by any of the following:
;;;
;;; $ funcall scriptl.example:test-cmd --foo -b X
;;; 
;;; $ make-script test-cmd scriptl.example:test-cmd
;;; $ ./test-cmd --foo -b X
;;;
;;; In lisp: (scriptl.example:test-cmd "--foo" "-b" "X")
;;;
;;; This is a bit silly, so you probably want any sort of option
;;; parsing to pass to a lispier function.
;;; 
(defun test-cmd (&rest args)
  (with-cli-options (args "Usage:~%~@{~A~%~}~%")
      ((foo ((#\f "foo") nil "Foo, or do not foo"))
       (bar ((#\b "bar") "BAR" "Which bar?")))
    (when foo
      (format t "There will be foo.~%"))
    (format t "We shall be using the ~A bar.~%"
            (or bar "default"))))
