(in-package :scriptl)

(defvar *stream* nil)

(defmethod decode-header-for ((version (eql 2)) stream)
  (let* ((cwd (cadr (read-from-packet stream)))
         (cmd (read-from-packet stream))
         (err (cadr (read-from-packet stream))))
    (let ((header (make-header :version 2 :cwd cwd :command cmd
                               :error-fun err)))
      (ecase (car cmd)
        (:funcall
         (setf (header-fun header) (cadr cmd))
         (setf (header-system header) (cadddr cmd))
         (let ((args (read-from-packet stream)))
           (loop for i from 0 below (cadr args)
                 collect (read-packet stream) into list
                 finally (setf (header-args header) list)))))
      header)))

(defmethod handle-client-for ((version (eql 2)) stream)
  (let* ((*standard-output* (make-instance 'packet-io-stream
                                           :stream stream))
         (*standard-input* *standard-output*)
         (*stream* stream)
         (cmd (or (header-command *header*) ""))
         (sys (header-system *header*)))
    (handler-bind ((error
                     (lambda (e)
                       (when (header-error-fun *header*)
                         (catch 'unhandled
                           (unless (funcall (header-error-fun *header*) e)
                             (throw 'unhandled nil))
                           (throw 'error-handled nil))))))
      (ecase (car cmd)
        (:funcall
         (let ((*script* (caddr cmd)))
           (when (and sys (> (length sys) 0))
             (handler-case
                 (unless (asdf::component-loaded-p sys)
                   (asdf:load-system sys))
               (error (e)
                 (declare (ignore e))
                 (error "Error trying to load system: \"~A\"~@[ ~A~]" sys
                        (when (not (asdf:find-system sys nil))
                          "(non-existent system)")))))
           (let ((fun (read-from-string (header-fun *header*))))
             (apply fun (header-args *header*)))))))))

(defmethod make-script-for ((version (eql 2)) filename function error-fun
                            &key system &allow-other-keys)
  (let ((scriptlcom
          (namestring
           (merge-pathnames
            (make-pathname :name "scriptlcom"
                           :directory '(:relative "src"))
            (asdf:component-pathname
             (asdf:find-component :scriptl '("src" "scriptlcom")))))))
    (with-open-file (stream filename :direction :output
                                     :if-exists :supersede)
      (format stream *scriptl-text-v2*
              scriptlcom function error-fun (or system "")))))

 ;; Extensions

(defun exit-code (val)
  (declare (type (integer 0 255) val))
  (when *stream*
    (send-packet *stream* ":exit")
    (send-packet *stream* (format nil "~D" val))))

(defun readline (prompt)
  (if *stream*
      (progn
        (send-packet *stream* ":interactive-readline")
        (send-packet *stream* prompt)
        (read-packet *stream*))
      (progn
        (format t "~&~A" prompt)
        (read-line))))

(defun addhistory (line)
  (when *stream*
    (send-packet *stream* ":interactive-addhistory")
    (send-packet *stream* line)))

(defun getenv (var)
  (declare (type string var))
  (if *stream*
      (progn
        (send-packet *stream* ":getenv")
        (send-packet *stream* var)
        (let ((found-p (read-packet *stream*)))
          (when (string= found-p "t")
            (read-packet *stream*))))
      (osicat-posix:getenv var)))

