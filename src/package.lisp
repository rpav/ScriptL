(defpackage :scriptl
  (:use #:cl #:alexandria #:trivial-utf-8)
  (:export #:start #:*header* #:*script* #:*scriptl-port*

           #:header
           #:header-version #:header-cwd #:header-command
           #:header-fun #:header-args

           #:make-script #:make-script-usage

           #:readline #:addhistory

           #:mk-cmd))
