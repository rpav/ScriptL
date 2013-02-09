(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:load-system :scriptl))

(in-package :scriptl.asdf)

(defsystem :scriptl-util
  :description "Various utilities for doing scriptly things with ScriptL"
  :author "Ryan Pavlik"
  :license "LLGPL"

  :depends-on (:scriptl :cl-ppcre)
  :pathname "src/util"
  :serial t

  :components
  ((:file "package")
   (:file "regexp")
   (:file "streams")))
