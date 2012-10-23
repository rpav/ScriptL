(in-package :scriptl)

(defmethod decode-header-for ((version (eql 2)) stream)
  (let* ((cwd (cadr (read-from-string (read-packet stream))))
         (cmd (read-from-string (read-packet stream)))
         (err (cadr (read-from-string (read-packet stream)))))
    (let ((header (make-header :version 2 :cwd cwd :command cmd
                               :error-fun err)))
      (ecase (car cmd)
        (:funcall
         (setf (header-fun header) (cadr cmd)
               (header-command header) (caddr cmd))
         (let ((args (read-from-string (read-packet stream))))
           (loop for i from 0 below (cadr args)
                 collect (read-packet stream) into list
                 finally (setf (header-args header) list)))))
      header)))

(defmethod handle-client-for ((version (eql 2)) stream))
