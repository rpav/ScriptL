(defpackage #:scriptl-test.io
  (:use #:cl
        #:FiveAM))

(in-package :scriptl-test.io)

(scriptl:start)
; there's a way for checking the server has started?
(sleep 1)

(defun eval-string (string)
  (eval (read-from-string string)))

(defun run-test (string)
  (uiop:call-with-temporary-file
    (lambda (tmp)
      (let ((tmpfile (namestring tmp)))
        (scriptl:make-script tmpfile 'eval-string)
        (inferior-shell:run/i `(,tmpfile ,string))))
    :want-stream-p nil
    :want-pathname-p t))

(defmacro test-finishes (name form)
  `(test ,name (finishes (run-test (prin1-to-string ',form)))))

(def-suite output-io-suite
  :description "The output I/O test suite.")

(in-suite output-io-suite)

(test-finishes format-t
               (format t "ok~%"))
(test-finishes inferior-shell-echo
               (inferior-shell:run '(/bin/echo ok)))
(test-finishes uiop-echo
               (uiop:run-program '("/bin/echo" "ok") :output t))

(run! 'output-io-suite)
