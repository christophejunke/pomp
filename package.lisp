(defpackage :pomp
     (:use :alexandria :cl :lisp-binary :varint)
     (:import-from :flexi-streams
                   #:with-output-to-sequence
                   #:with-input-from-sequence))
