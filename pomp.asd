(defsystem pomp
  :depends-on (lisp-binary flexi-streams)
  :components ((:file "package")
               (:file "utils")
               (:file "macros")
               (:file "encoding")
               (:file "binary")
               (:file "pomp")))
