(in-package :scriptl)

(defvar *scriptl-port* 4010)
(defvar *scriptl-uds* ".scriptl-sock")

(defvar *header* nil
  "SCRIPTL:HEADER for the current command, or `NIL` if there is no
command.")

(defvar *script* nil
  "String indicating the script name for the current command, or `NIL`
if there is no command.")



(defmacro sending-errors (stream &body body)
  (once-only (stream)
    `(handler-case
         (progn ,@body)
       (error (c)
         (send-packet ,stream ":error")
         (send-packet ,stream (class-name (class-of c)))
         (send-packet ,stream c)))))

(defun client-loop (stream)
  (unwind-protect
       (catch 'error-handled
         (sending-errors stream
           (let* ((*header* (decode-header stream))
                  (*default-pathname-defaults* (header-cwd *header*)))
             (handle-client-for (header-version *header*) stream))))
    (close stream)))

(defmacro with-open-socket ((var type) &body body)
  `(let ((,var (sockets:make-socket :connect :passive
                                    :address-family ,type
                                    :type :stream)))
     (unwind-protect
          (progn ,@body)
       (close ,var))))

(defun server-loop (type)
  (with-open-socket (server type)
    (ecase type
      (:internet
       (sockets:bind-address server sockets:+loopback+
                             :port *scriptl-port*
                             :reuse-address t))
      (:local
       (let ((addr (namestring
                    (merge-pathnames *scriptl-uds*
                                     (user-homedir-pathname))))
             (umask (osicat-posix:umask #o077)))
         (ignore-errors
          (osicat-posix:unlink addr))
         (sockets:bind-address server (sockets:make-address addr))
         (osicat-posix:umask umask))))
    (sockets:listen-on server :backlog 5)
    (loop do
      (sockets:with-accept-connection (socket server :wait t)
        (handler-case
            (client-loop socket)
          (iolib.streams:hangup (c) (declare (ignore c))))))))

(defun start ()
  (bt:make-thread (lambda () (server-loop :internet)) :name "ScriptL Server"))
