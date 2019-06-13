(in-package :pomp)

(defun map-tokens (function pomp-format-string)
  (let ((size 32)
        (counter 0)
        (modifier 0)
        (factor 1)
        (state nil)
        (start 0)
        (position 0))
    (labels ((prefix (ratio next-state)
               (setf factor ratio)
               (setf counter 0)
               (augment-prefix)
               (setf state next-state))

             (expect (expected &key then because)
               (setf state
                     (lambda (char)
                       (assert (char= char expected)
                               ()
                               "Unexpected char ~s in place of ~s~
                               ~@[ ~a~]."
                               char
                               expected
                               (case because
                                 (:buffer "in buffer format (%p%u)")))
                       (etypecase then
                         (keyword (token then))
                         (function (setf state then))))))

             (augment-prefix ()
               (incf counter)
               (setf modifier (clamp (* modifier factor) 1/4 2)))

             (token (&rest token)
               (funcall function token)
               (setf start position)
               (setf state #'state/%))

             (size-for-prefix ()
               (* size modifier))

             (state/dispatch (c)
               (setf modifier 1)
               (ecase c
                 (#\l (prefix 2   #'state/long))
                 (#\h (prefix 1/2 #'state/half))
                 ((#\d #\i) (token :signed size))
                 (#\u (token :unsigned size))
                 ((#\f #\F #\g #\G #\e #\E) (token :single))
                 (#\s (token :string))
                 (#\m (setf state #'state/string))
                 (#\p (setf state
                            (expect #\%
                                    :then (expect #\u
                                                  :then :buffer
                                                  :because :buffer)
                                    :because :buffer)))
                 (#\x (token :file-descriptor))))

             (state/string (c)
               (ecase c
                 (#\s (token :malloc-string))))

             (state/half (c)
               (ecase c
                 (#\h (augment-prefix))
                 ((#\i #\d) (token :signed (size-for-prefix)))
                 (#\u (token :unsigned (size-for-prefix)))))

             (state/long (c)
               (ecase c
                 (#\l (augment-prefix)
                  (assert (<= counter 2) () "Too many prefix characters."))
                 ((#\i #\d) (token :signed (size-for-prefix)))
                 (#\u (token :unsigned (size-for-prefix)))
                 ((#\f #\F #\g #\G #\e #\E)
                  (assert (= counter 1)
                          ()
                          "Unexpected float format with more than one prefix %l.")
                  (ecase counter
                    (1 (token :double))))))

             (state/% (c)
               (ecase c
                 (#\%
                  (setf state #'state/dispatch)))))
      (loop
        initially (setf state #'state/%)
        for char across pomp-format-string
        do (incf position)
           (handler-case (funcall state char)
             (error (condition)
               (error
                "~&Invalid character ~s in ~<~s at position ~d.~@:_~
                ~:*~v@T ^~:>~%~a"
                char
                (list pomp-format-string
                      position)
                condition)))
        finally
           (assert (eq state #'state/%)
                   ()
                   "Not enough input for sequence ~s started at ~
                    position ~d of ~s."
                   (subseq pomp-format-string start)
                   start
                   pomp-format-string)))))
