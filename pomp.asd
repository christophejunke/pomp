(defsystem pomp
  :depends-on (varint lisp-binary flexi-streams)
  :components ((:file "package")
               (:file "pomp-utils")
               (:file "pomp")))
