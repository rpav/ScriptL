(in-package :scriptl)

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
  version cwd command fun args error-fun)

(defun decode-header (stream)
  (let* ((ver (cadr (read-from-packet stream))))
    (unless (<= ver 2)
      (error 'version-error :want-version 2 :found-version ver))
    (decode-header-for ver stream)))

 ;; Low level

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun to-octets (value)
    (etypecase value
      (string (string-to-utf-8-bytes value))
      ((unsigned-byte 8) (make-array 1 :element-type '(unsigned-byte 8)
                                       :initial-element value))
      (octet-vector value)
      (t (string-to-utf-8-bytes (princ-to-string value))))))

(defun send-packet (stream value)
  (let ((octets (to-octets value)))
    (write-sequence (to-octets (format nil "~8,'0X" (length octets))) stream)
    (write-sequence octets stream)))

(defun send-line (stream value)
  (let ((octets (to-octets value)))
    (write-sequence octets stream)
    (write-sequence #.(to-octets (make-string 1 :initial-element #\Newline))
                    stream)))

(defun read-packet (stream &optional (raw-p nil))
  (let ((length-string (make-string 8)))
    (read-sequence length-string stream)
    (let* ((len (parse-integer length-string :radix 16))
           (input (if raw-p
                      (make-array (the index len)
                                  :element-type '(unsigned-byte 8))
                      (make-string len))))
      (read-sequence input stream)
      input)))

(defun read-from-packet (stream)
  (read-from-string (read-packet stream)))
