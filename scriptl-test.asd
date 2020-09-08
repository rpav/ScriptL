(defsystem scriptl-test
  :author "Gian Piero Carrubba <gpiero@rm-rf.it>"
  :license "LLGPL"
  :depends-on (:scriptl
               :FiveAM
               :inferior-shell)
  :components ((:module "t"
                :serial t
                :components
                ((:file "io")))))
