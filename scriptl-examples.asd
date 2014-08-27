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
                   :package #:scriptl.example
                   :system "scriptl-examples")
   (scriptl:mk-cmd "eval"
                   :function #:test-eval
                   :errors #:eval-usage
                   :package #:scriptl.example
                   :system "scriptl-examples")
   (scriptl:mk-cmd "funcall"
                   :function #:test-funcall
                   :errors #:funcall-usage
                   :package #:scriptl.example
                   :system "scriptl-examples")))
