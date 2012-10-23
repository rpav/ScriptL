(in-package :scriptl)

 ;; Types

(deftype octet () '(unsigned-byte 8))
(deftype octet-vector () '(simple-array octet (*)))
(deftype index () `(integer 0 ,array-total-size-limit))

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
  (let* ((ver (cadr (read-from-string (read-line stream))))
         (cwd (cadr (read-from-string (read-line stream))))
         (cmd (read-from-string (read-line stream)))
         (err (cadr (read-from-string (read-line stream)))))
    (unless (= ver 1)
      (error 'version-error :found-version ver :want-version 1))
    (let ((header (make-header :version ver :cwd cwd :command cmd
                               :error-fun err)))
      (ecase (car cmd)
        (:eval)
        (:funcall
         (setf (header-fun header) (read-from-string (read-line stream)))
         (loop for i from 0 below (cadr cmd)
               collect (read-packet stream) into args
               finally (setf (header-args header) args))))
      header)))

 ;; Low level

(defun to-octets (value)
  (etypecase value
    (string (string-to-utf-8-bytes value))
    (octet-vector value)
    (t (string-to-utf-8-bytes (princ-to-string value)))))

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
