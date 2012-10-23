(in-package :scriptl)

(defun make-script (filename function &optional error-fun)
  (let ((function (if (stringp function)
                      (read-from-string function)
                      function))
        (error-fun (if (stringp error-fun)
                       (read-from-string error-fun)
                       error-fun)))
    (format t "~&Creating script ~A for function ~A~%" filename function)
    (when error-fun
      (format t "Error function: ~A~%" error-fun))
    (with-open-file (stream filename :direction :output
                                     :if-exists :supersede)
      (format stream *script-text-v1*
              (format nil "~A::~A"
                      (package-name (symbol-package error-fun))
                      (symbol-name error-fun))
              (format nil "~A::~A"
                      (package-name (symbol-package function))
                      (symbol-name function))))
    (nix:chmod (merge-pathnames filename) #o755))
  (values))

(defun make-script-usage (e)
  (declare (ignore e))
  (unless (= 2 (length (header-args *header*)))
    (format t "Usage: make-script SCRIPT-NAME FUNCTION-NAME [ERROR-HANDLER]

Example: make-script make-script \\                # script name
                     scriptl:make-script \\        # function
                     scriptl:make-script-usage    # gets called with errors~%"))
  t)
