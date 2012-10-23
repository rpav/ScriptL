(in-package :scriptl)

(defmethod decode-header-for ((version (eql 2)) stream)
  (let* ((cwd (cadr (read-from-packet stream)))
         (cmd (read-from-packet stream))
         (err (cadr (read-from-packet stream))))
    (let ((header (make-header :version 2 :cwd cwd :command cmd
                               :error-fun err)))
      (ecase (car cmd)
        (:funcall
         (setf (header-fun header) (cadr cmd))
         (let ((args (read-from-packet stream)))
           (loop for i from 0 below (cadr args)
                 collect (read-packet stream) into list
                 finally (setf (header-args header) list)))))
      header)))

(defmethod handle-client-for ((version (eql 2)) stream)
  (let* ((*standard-output* (make-instance 'packet-io-stream
                                           :stream stream))
         (*standard-input* *standard-output*)
         (cmd (header-command *header*)))
    (ecase (car cmd)
      (:funcall
       (let ((*script* (caddr cmd)))
         (apply (header-fun *header*) (header-args *header*)))))))
