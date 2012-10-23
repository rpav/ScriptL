(in-package :scriptl)

(defmacro sending-result (stream &body body)
  (with-gensyms (result output)
    (once-only (stream)
      `(let* ((,result)
              (,output
                (with-output-to-string (*standard-output*)
                  (setf ,result (multiple-value-list (progn ,@body))))))
         (send-packet ,stream ":ok")
         (if (cdr ,result)
             (send-packet ,stream (write-to-string (cons 'values ,result)))
             (send-packet ,stream (write-to-string (car ,result))))
         (send-packet ,stream ,output)))))

(defmethod decode-header-for ((version (eql 1)) stream)
  (let* ((cwd (cadr (read-from-packet stream)))
         (cmd (read-from-packet stream))
         (err (cadr (read-from-packet stream))))
    (let ((header (make-header :version 1 :cwd cwd :command cmd
                               :error-fun err)))
      (ecase (car cmd)
        (:eval)
        (:funcall
         (setf (header-fun header) (read-from-packet stream))
         (loop for i from 0 below (cadr cmd)
               collect (read-packet stream) into args
               finally (setf (header-args header) args))))
      header)))

(defmethod handle-client-for ((version (eql 1)) stream)
  (handler-bind ((error (lambda (e)
                          (when (header-error-fun *header*)
                            (catch 'unhandled
                              (sending-result stream
                                (unless (funcall (header-error-fun *header*) e)
                                  (throw 'unhandled nil)))
                              (throw 'error-handled nil))))))
    (ecase (car (header-command *header*))
      (:eval
       (let ((*script* (merge-pathnames (cadr (header-command *header*)))))
         (sending-result stream
           (eval (read-from-packet stream)))))
      (:funcall
       (let ((*script* (caddr (header-command *header*))))
         (sending-result stream
           (apply (header-fun *header*) (header-args *header*))))))))
