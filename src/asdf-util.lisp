(in-package :scriptl)

(defclass mk-cmd (asdf:static-file)
  ((package :initarg :package :initform nil)
   (function :initarg :function :initform nil)
   (errors :initarg :errors :initform nil)
   (system :initarg :system :initform nil)))

(defmethod asdf:perform ((op asdf:compile-op) (c mk-cmd))
  (with-slots (package function errors system) c
    (let ((fun (intern (string function) package))
          (err (intern (string errors) package))
          (path (asdf:component-pathname c)))
      (scriptl:make-script path fun err system))))

(defmethod asdf:operation-done-p ((op asdf:compile-op) (c mk-cmd))
  (probe-file (asdf:component-pathname c)))
