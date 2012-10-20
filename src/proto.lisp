(in-package :scriptl)

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

(defun send-packet (stream value)
  (let ((string (if (stringp value) value (princ-to-string value))))
    (format stream "~8,'0X~A" (length string) string)))

(defun send-line (stream value)
  (let ((string (if (stringp value) value (princ-to-string value))))
    (write-sequence string stream)
    (write-sequence #(#\Newline) stream)))

(defun read-packet (stream)
  (let ((length-string (make-string 8)))
         (read-sequence length-string stream)
         (let ((input (make-string (parse-integer length-string :radix 16))))
           (read-sequence input stream)
           input)))
