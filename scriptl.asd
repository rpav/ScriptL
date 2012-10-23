(defpackage :scriptl.asdf
  (:use #:cl #:asdf))

(in-package :scriptl.asdf)

(defclass autoconf-module (module)
  ((prefix :initarg :prefix :initform nil)
   (configure-args :initarg :configure-args :initform nil)
   (test-target :initarg :test-target :initform nil
                :documentation "Quick check to see if things are built")))

(defmethod perform ((op compile-op) (c autoconf-module))
  (with-slots (prefix configure-args test-target) c
    (let ((path (asdf:component-pathname c)))
      (unless (probe-file (merge-pathnames test-target path))
        (let ((*verbose-out* *standard-output*))
          (run-shell-command "cd ~A ; ./configure ~@[--prefix=~A~] ~A ; make"
                             path prefix configure-args))))))

(defmethod operation-done-p ((op compile-op) (c autoconf-module))
  nil)

(defsystem :scriptl
  :description "Scripting, Common Lisp style"
  :author "Ryan Pavlik"
  :license "LLGPL"

  :depends-on (:alexandria :bordeaux-threads :trivial-utf-8 :iolib :osicat)
  :pathname "src"
  :serial t

  :components
  ((:file "package")
   (:file "proto")
   (:file "server")
   (:file "script-text")
   (:file "make-script")

   #+-
   (:autoconf-module "scriptlcom"
    :pathname "scriptlcom"
    :test-target "scriptlcom")))
