(in-package :scriptl)

(defvar *scriptl-scripts* (make-hash-table))

(defclass script ()
  ((symbol :initarg :symbol :reader script-symbol)
   (function :initarg :function)
   (description :initarg :description)))

 ;; API

(defun script-register (symbol function description)
  (let ((script (make-instance 'script
                  :symbol symbol
                  :function function
                  :description description)))
    (setf (gethash symbol *scriptl-scripts*) script)
    nil))

(defun script-list ()
  (hash-table-values *scriptl-scripts*))

(defun script-make (symbol)
  (let ((script (gethash symbol *scriptl-scripts*)))
    (when script
      (with-slots (function) script
        (funcall function))
      t)))

(defmacro register (symbol function descr)
  `(eval-when (:load-toplevel :execute)
     (script-register ,symbol ,function ,descr)))

 ;; scriptl script

(defun scriptl-errors (e)
  (format t "~A~%" e))

(defun scriptl-make-script ()
  (scriptl:make-script "scriptl" 'scriptl-cmd 'scriptl-errors))

(defun scriptl-cmd (&rest args)
  (let ((cmd (car args)))
    (switch (cmd :test #'equal)
      ("list" (scriptl-list-scripts))
      ("make"
       (if (cadr args)
           (let ((name (read-from-string (cadr args))))
             (unless (script-make name)
               (format t "Error: couldn't find script ~S~%" name)
               (scriptl-help)))
           (progn
             (format t "Error: must specify script~%")
             (scriptl-help))))
      (otherwise (scriptl-help)))))

(defun scriptl-list-scripts ()
  (let ((list (sort (script-list)
                    (lambda (a b)
                      (string< (format nil "~S" (script-symbol a))
                               (format nil "~S" (script-symbol b)))))))
    (format t "~30A   ~A~%~%" "Script" "Description")
    (loop for script in list
          do (with-slots (symbol description) script
               (let ((str (format nil "~S" symbol))
                     (descr (ppcre:split "\\s+" description)))
                 (if (> (length str) 30)
                     (format t "~A~%~A~%" str
                             (format nil "~30T - ~{~<~%~33T~1,80:;~A~> ~}" descr))
                     (format t "~A~%"
                             (format nil "~30A - ~{~<~%~33T~1,80:;~A~> ~}" str descr))))))))

(defun scriptl-help ()
  (format t "~
Syntax: scriptl <cmd> <arg>

Commands:

  list        - List scripts
  make        - Create a script
"))

(eval-when (:load-toplevel :execute)
  (script-register 'scriptl 'scriptl-make-script "ScriptL management command: scriptl")
  (let ((*default-pathname-defaults* (asdf:component-pathname (asdf:find-component 'scriptl "bin"))))
    (script-make 'scriptl)))
