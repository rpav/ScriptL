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

(defmacro with-io ((input-stream from-path output-stream to-path) &body body)
  (once-only (from-path to-path)
    `(let (,input-stream ,output-stream)
       (unwind-protect
            (progn
              (setf ,input-stream
                    (if ,from-path
                        (open ,from-path :direction :input)
                        *standard-input*))
              (setf ,output-stream
                    (if ,to-path
                        (open ,to-path :direction :output :if-exists :supersede)
                        *standard-output*))
              ,@body)
         (when ,input-stream (close ,input-stream))
         (when ,output-stream (close ,output-stream))))))
