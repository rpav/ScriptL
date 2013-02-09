(in-package :scriptl.util)

(defmacro ignore-eof ((&optional eof-value) &body body)
  "Block which finishes when EOF is encountered.  Useful for LOOP, etc."
  `(handler-case
       (progn ,@body)
     (end-of-file (c)
       (declare (ignore c))
       ,eof-value)))

(defun read-lines (stream)
  "Read all lines from STREAM into a list."
  (let (line eofp)
    (loop while (multiple-value-setq (line eofp)
                  (read-line stream nil))
          collecting line into lines
          finally (return (values lines eofp)))))

(defun read-lines-to-string (stream)
  "Read STREAM into a string"
  (multiple-value-bind (lines eofp)
      (read-lines stream)
    (format nil "~{~%~A~}~:[~;~%~]" lines eofp)))
