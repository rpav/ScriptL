(in-package :scriptl)



(defun maybe-load-system (sys)
  (when (and sys (> (length sys) 0))
    (handler-case
        (unless (asdf::component-loaded-p sys)
          (asdf:load-system sys))
      (error (e)
        (declare (ignore e))
        (error "Error trying to load system: \"~A\"~@[ ~A~]" sys
               (when (not (asdf:find-system sys nil))
                 "(non-existent system)"))))))

(defmacro sending-errors (stream &body body)
  (with-gensyms (unhandled-error)
    (once-only (stream)
      `(catch ',unhandled-error
         (handler-bind
             ((error
                (lambda (c)
                  (send-packet ,stream ":error")
                  (send-packet ,stream (class-name (class-of c)))
                  (send-packet ,stream
                               (with-output-to-string (str)
                                 (format str "~A~%~%" c)
                                 (trivial-backtrace:print-backtrace-to-stream str)))
                  (throw ',unhandled-error t))))
           ,@body
           nil)))))

(defun client-loop (stream)
  (when (sending-errors stream
          (let ((header (decode-header stream)))
            (bt:make-thread
             (lambda ()
               (handler-case
                   (unwind-protect
                        (sending-errors stream
                          (catch 'error-handled
                            (let* ((*header* header)
                                   (*default-pathname-defaults* (header-cwd *header*)))
                              (handle-client-for (header-version *header*) stream))))
                     (close stream))
                 (iolib:hangup ())))
             :name (format nil "ScriptL client: ~A" (header-command header)))))
    (close stream)))

(defmacro with-open-socket ((var type) &body body)
  `(let ((,var (sockets:make-socket :connect :passive
                                    :address-family ,type
                                    :type :stream)))
     (unwind-protect
          (progn ,@body)
       (close ,var))))

(defun bind-sock (sock type)
  (ecase type
    (:internet
     (sockets:bind-address sock sockets:+loopback+
                           :port *scriptl-port*
                           :reuse-address t))
    (:local
     (let ((addr (namestring
                  (merge-pathnames *scriptl-uds*
                                   (user-homedir-pathname))))
           (umask (osicat-posix:umask #o077)))
       (ignore-errors
        (osicat-posix:unlink addr))
       (sockets:bind-address sock (sockets:make-address addr))
       (osicat-posix:umask umask)))))

(defun server-loop (type)
  (with-open-socket (server type)
    (bind-sock server type)
    (sockets:listen-on server :backlog 5)
    (loop as socket = (iolib.sockets:accept-connection server :wait t)
          do (handler-case
                 (client-loop socket)
               (iolib.streams:hangup (c) (declare (ignore c)))))))

(defvar *scriptl-threads* (make-hash-table))

(defun start (&optional (type :local))
  (ensure-gethash type *scriptl-threads*
                  (bt:make-thread (lambda () (server-loop type))
                                  :name "ScriptL Server")))
