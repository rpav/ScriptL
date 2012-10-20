(defpackage :scriptl
  (:use #:cl #:alexandria #:usocket)
  (:export #:start #:*header* #:*script* #:*scriptl-port*

           #:header
           #:header-version #:header-cwd #:header-command
           #:header-fun #:header-args

           #:make-script #:make-script-usage))
