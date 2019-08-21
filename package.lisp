(defpackage :pomp
  (:use #:alexandria
        #:cl
        #:split-sequence
        #:lisp-binary
        #:iomux
        #:iolib)
  (:import-from :flexi-streams
                #:with-output-to-sequence
                #:with-input-from-sequence)
  (:export #:make-message
           #:argument
           #:ascii
           #:buffer
           #:pomp-message-id
           #:encode-message
           #:decode-message
           #:make-bytes
           ;; TODO: add rest
           ))
