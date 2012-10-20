(in-package :scriptl)

(defvar *scriptl-port* 4010)

(defvar *header* nil
  "SCRIPTL:HEADER for the current command, or `NIL` if there is no
command.")

(defvar *script* nil
  "String indicating the script name for the current command, or `NIL`
if there is no command.")



(defmacro sending-result (stream &body body)
  (with-gensyms (result)
    (once-only (stream)
      `(send-packet stream
                    (with-output-to-string (*standard-output*)
                      (let ((,result (multiple-value-list (progn ,@body))))
                        (send-line ,stream ":ok")
                        (if (cdr ,result)
                            (send-packet ,stream (write-to-string (cons 'values ,result)))
                            (send-packet ,stream (write-to-string (car ,result))))))))))

(defmacro sending-errors (stream &body body)
  (once-only (stream)
    `(handler-case
         (progn ,@body)
       (error (c)
         (send-line ,stream ":error")
         (send-line ,stream (class-name (class-of c)))
         (send-packet ,stream c)))))

(defun client-loop (stream)
  (unwind-protect
       (catch 'error-handled
         (sending-errors stream
           (let* ((*header* (decode-header stream))
                  (*default-pathname-defaults* (header-cwd *header*)))
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
                      (eval (read-from-string (read-packet stream))))))
                 (:funcall
                  (let ((*script* (caddr (header-command *header*))))
                    (sending-result stream
                      (apply (header-fun *header*) (header-args *header*))))))))))
    (close stream)))

(defun server-loop ()
  (let ((server (socket-listen "localhost" *scriptl-port* :reuse-address t)))
    (unwind-protect
         (loop do
           (wait-for-input server)
           (let ((socket (socket-accept server)))
             (unwind-protect
                  (client-loop (socket-stream socket))
               (socket-close socket))))
      (socket-close server))))

(defun start ()
  (bt:make-thread #'server-loop :name "ScriptL Server"))
