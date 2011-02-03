;;;; slime-project.asd

(asdf:defsystem #:slime-project
  :serial t
  :components ((:file "package")
               (:file "slime-project")))
