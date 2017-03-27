;;; This uses UNIX-OPTIONS because it seems to do the most and be the
;;; least presumptive about how we're going to do it.  You can, of
;;; course, use whatever you like.

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

;;; This is a replacement for the old EVAL.  It's a little more than
;;; just calling CL:EVAL, of course.
(defun test-eval (&optional string)
  (if string
      (progn
        (prin1 (eval (read-from-string string)))
        (fresh-line))
      (eval-usage)))

(defun eval-usage (&rest args)
  (declare (ignore args))
  (format t "Usage: Usage: eval LISP-FORM

Examples: eval '(+ 1 1)'
          eval '(princ \"hello world\")
          eval 'scriptl:*header*'
"))

;;; This is a replacement for the old FUNCALL1.  And it's nicer!
;;; Although technically it's APPLY. ;)
(defun test-funcall (function-name &rest args)
  (let ((actual-args (mapcar (lambda (x) (eval (read-from-string x)))
                             args)))
    (prin1 (apply (read-from-string function-name) actual-args))
    (fresh-line)))

(defun funcall-usage (&rest args)
  (declare (ignore args))
  (format t "Usage: funcall FUNCTION [arg1 ...]

Example: funcall PRINT \"hello world\"
         funcall + 1 1
         funcall format nil \\\"~~R\\\" 3

Note: Arguments are READ and EVAL'd before passing to FUNCTION.  This
      means any strings need escaped properly to your shell...
"))
