(defsystem :scriptl
  :description "Scripting, Common Lisp style"
  :author "Ryan Pavlik"
  :license "LLGPL"

  :depends-on (:alexandria :bordeaux-threads :usocket :osicat)
  :pathname "src"
  :serial t

  :components
  ((:file "package")
   (:file "proto")
   (:file "server")
   (:file "script-text")
   (:file "make-script")))
