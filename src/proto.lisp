(in-package :scriptl)

 ;; Variables

(defvar *scriptl-port* 4010)
(defvar *scriptl-uds* ".scriptl-sock")

(defvar *header* nil
  "SCRIPTL:HEADER for the current command, or `NIL` if there is no
command.")

(defvar *script* nil
  "String indicating the script name for the current command, or `NIL`
if there is no command.")

 ;; Types

(deftype octet () '(unsigned-byte 8))
(deftype octet-vector () '(simple-array octet (*)))
(deftype index () `(integer 0 ,array-total-size-limit))

 ;; GFs

(defgeneric decode-header-for (version stream))
(defgeneric handle-client-for (version stream))

 ;; Errors

(define-condition version-error (error)
  ((found-version :initarg :found-version :reader version-error-found-version)
   (want-version :initarg :want-version :reader version-error-want-version))
  (:report (lambda (c s)
             (with-slots (found-version want-version) c
               (format s "Found version: ~A  Want version: ~A"
                       found-version want-version)))))

 ;; Input header

(defstruct header
  version cwd command system fun args error-fun)

(defun decode-header (stream)
  (let* ((ver (cadr (read-from-packet stream))))
    (unless (<= ver 2)
      (error 'version-error :want-version 2 :found-version ver))
    (decode-header-for ver stream)))

 ;; Low level

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun to-octets (value)
    (let ((result (etypecase value
                    (string (string-to-utf-8-bytes value))
                    ((unsigned-byte 8) (make-array 1 :element-type '(unsigned-byte 8)
                                                     :initial-element value))
                    (octet-vector value)
                    (t (string-to-utf-8-bytes (princ-to-string value))))))
      ;; Under the LispWorks we have to provide statically allocated vectors
      ;; when writing to the socket. See details here:
      ;; https://github.com/rpav/ScriptL/issues/10#issuecomment-904430041
      #+lispworks
      (make-array (array-dimensions result)
                  :element-type (array-element-type result)
                  :initial-contents result
                  :allocation :static)
      #-lispworks
      result)))

(defun send-packet (stream value)
  (let ((octets (to-octets value)))
    (write-sequence (to-octets (format nil "~8,'0X" (length octets))) stream)
    (write-sequence octets stream)
    (finish-output stream)))

(defun send-line (stream value)
  (let ((octets (to-octets value)))
    (write-sequence octets stream)
    (write-sequence #.(to-octets (make-string 1 :initial-element #\Newline))
                    stream)))

(defun read-packet (stream &optional (raw-p nil))
  (let ((length-string (make-string 8)))
    (if (< (read-sequence length-string stream) 8)
        (error 'end-of-file :stream stream))
    (let ((len (parse-integer length-string :radix 16)))
      (when (> len 0)
        (let ((input (if raw-p
                         (make-array (the index len)
                                     :element-type '(unsigned-byte 8))
                         (make-string len))))
          (if (< (read-sequence input stream) len)
              (error 'end-of-file :stream stream))
          input)))))

(defun read-from-packet (stream)
  (read-from-string (read-packet stream)))
