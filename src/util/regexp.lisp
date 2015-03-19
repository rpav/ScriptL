(in-package :scriptl.util)

(defun regex-mode (mode)
  (values (and (find #\i mode) t)
          (and (find #\m mode) t)
          (and (find #\s mode) t)
          (and (find #\x mode) t)))

(defmacro with-scanner ((scanner regex &optional (mode "")) &body body)
  (with-gensyms (ci multi single extend)
    `(multiple-value-bind (,ci ,multi ,single ,extend)
         (regex-mode ,mode)
       (let ((,scanner (create-scanner ,regex
                                       :case-insensitive-mode ,ci
                                       :multi-line-mode ,multi
                                       :single-line-mode ,single
                                       :extended-mode ,extend)))
         ,@body))))

#+make-util (make-util:util-depends 'with-scanner 'regex-mode)

(defun print-matches (line matches)
  (declare (ignore matches))
  (format t "~&~A~%" line))

(defun grep (regex stream
             &key (function 'print-matches) (mode "") (output *standard-output*))
  (with-scanner (scanner regex mode)
    (let ((*standard-output* output))
      (loop as line = (read-line stream nil)
            while line do
              (multiple-value-bind (match regs)
                  (scan-to-strings scanner line)
                (when match
                  (funcall function line regs)))))))

#+make-util (make-util:util-depends 'grep 'with-scanner 'print-matches)

(defun grep-file (regex file
                  &key (function 'print-matches) (mode "") (output *standard-output*))
  (with-open-file (stream file)
    (grep regex stream :function function :mode mode :output output)))

#+make-util (make-util:util-depends 'grep-file 'grep)

(defmacro with-grep-env ((fun line regs user-body) &body body)
  `(let ((line-var (or ,line (gensym "LINE")))
         (regs-var (or ,regs (gensym "REGS"))))
     `(flet ((,,fun (,line-var ,regs-var)
               (declare (ignorable ,line-var ,regs-var))
               (let ((,(intern "$0") ,line-var)
                     ,@(loop for i from 0 below 9
                             as var = (symbolicate "$" (princ-to-string (1+ i)))
                             collect `(,var (and (> (length ,regs-var) ,i)
                                                 (elt ,regs-var ,i)))))
                 (declare (ignorable
                           ,@(loop for i from 0 upto 9
                                   collect (symbolicate "$" (princ-to-string i)))))
                 ,@,user-body)))
        ,,@body)))

(defmacro do-grep-file ((regex file
                         &key line regs mode (output '*standard-output*))
                        &body body)
  (with-gensyms (fun)
    (with-grep-env (fun line regs body)
      `(grep-file ,regex ,file :mode ,mode :function #',fun :output ,output))))

#+make-util (make-util:util-depends 'do-grep-file
                                    'with-grep-env 'grep-file)

(defmacro do-grep ((regex &key line regs mode
                    (input '*standard-input*) (output '*standard-output*))
                   &body body)
  (with-gensyms (fun)
    (with-grep-env (fun line regs body)
      `(grep ,regex ,input :function #',fun :mode ,mode
             :output ,output))))

#+make-util (make-util:util-depends 'do-grep 'with-grep-env 'grep)
