(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:load-system :scriptl))

(in-package :scriptl.asdf)

(defsystem :scriptl-examples
  :description "Build example scripts for ScriptL"
  :author "Ryan Pavlik"
  :license "LLGPL"

  :depends-on (:scriptl)
  :pathname "examples"
  :serial t

  :components
  ((:file "example")
   (scriptl:mk-cmd "make-script"
                   :function #:make-script
                   :errors #:make-script-usage
                   :package #:scriptl)
   (scriptl:mk-cmd "test-cmd"
                   :function #:test-cmd
                   :package #:scriptl.example)))
