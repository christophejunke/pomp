(in-package :pomp)

(defun tokenize (pomp-format-string)
  (let ((tokens)
        (size 32)
        (modifier 0)
        (factor 1)
        (state nil))
    (labels ((prefix (ratio next-state)
               (setf factor ratio)
               (augment-prefix)
               (setf state next-state))

             (expect (expected &key then)
               (setf state (lambda (c)
                             (assert (char= c expected))
                             (etypecase then
                               (keyword (token then))
                               (function (setf state then))))))

             (augment-prefix ()
               (setf modifier (clamp (* modifier factor) 1/4 2)))

             (token (&rest args)
               (push args tokens)
               (setf state #'state/%))

             (counter-size () (* size modifier))

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
                                                  :then :buffer))))
                 (#\x (token :file-descriptor))))

             (state/string (c)
               (ecase c
                 (#\s (token :malloc-string))))

             (state/half (c)
               (ecase c
                 (#\h (augment-prefix))
                 ((#\i #\d) (token :signed (counter-size)))
                 (#\u (token :unsigned (counter-size)))))

             (state/long (c)
               (ecase c
                 (#\l (augment-prefix))
                 ((#\i #\d) (token :signed (counter-size)))
                 (#\u (token :unsigned (counter-size)))
                 ((#\f #\F #\g #\G #\e #\E)
                  (ecase modifier
                    (1 (token :single))
                    (2 (token :double))))))

             (state/% (c)
               (ecase c
                 (#\%
                  (setf state #'state/dispatch)))))
      (loop
        initially (setf state #'state/%)
        for c across pomp-format-string
        do (funcall state c)
        finally
           (assert (eq state #'state/%) () "Not enough input")
           (return (nreverse tokens))))))
