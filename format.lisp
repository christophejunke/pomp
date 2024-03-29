(in-package :pomp)

(defun token-as-types (token)
  (destructuring-bind (tag &optional arg) (ensure-list token)
    (ecase tag
      (:buffer
       (values tag '(simple-array (ub 8) (*))))
      (:file-descriptor
       (values tag '(ub 32)))
      (:single
       (values :single-float 'single-float))
      (:double
       (values :double-float 'double-float))
      (:signed
       (values (ecase arg
		 (8 :signed-byte)
		 (16 :signed-short)
		 (32 :signed-zig-varint)
		 (64 :signed-zig-varlong))
	       `(sb ,arg)))
      (:unsigned
       (values (ecase arg
		 (8 :unsigned-byte)
		 (16 :unsigned-short)
		 (32 :unsigned-varint)
		 (64 :unsigned-varlong))
	       `(ub ,arg)))
      ((:malloc-string :string)
       (values :ascii 'simple-string)))))

(define-condition token () ((token :initarg :token :reader token)))

(defun map-tokens (function pomp-format-string)
  (let ((size 32)
	(counter 0)
	(modifier 0)
	(factor 1)
	(state nil)
	(start 0)
	(if-empty nil)
	(position 0))
    (handler-bind ((token (lambda (token)
			    (funcall function (token token))
			    (invoke-restart 'continue))))
      (labels (;; HELPERS

	       (prefix (ratio next-state ident)
		 (setf factor ratio)
		 (setf counter 0)
		 (setf if-empty ident)
		 (augment-prefix)
		 (setf state next-state))

	       (expect (chars token &aux (stack (coerce chars 'list)))
		 (assert stack)
		 (setf if-empty `(,token ,chars))
		 (setf state
		       (lambda (char &aux (expected (pop stack)))
			 (assert (char= char expected)
				 ()
				 "Unexpected char ~s in place of \"~a~{~a~}\"~
				  ~@[ ~a~]."
				 char
				 expected
				 stack
				 (case token
				   (:buffer
				    "in buffer format (%p%u)")
				   (:malloc-string
				    "in malloc-string format (%m%s)")))
			 (unless stack
			   (token token)))))

	       (augment-prefix ()
		 (incf counter)
		 (assert (<= counter 2) () "Too many prefix characters.")
		 (setf modifier (clamp (* modifier factor) 1/4 2)))

	       (token (&rest token)
		 ;; handler-bind + signal so that errors that happen when
		 ;; calling function are not caught by the state-machine error
		 ;; handler below (handler-case).
		 (restart-case (signal 'token :token token)
		   (continue ()))
		 (setf if-empty nil)
		 (setf start position)
		 (setf state #'state/wait-for-%))

	       (size-for-prefix ()
		 (* size modifier))

	       (feed-fsm (char)
		 (funcall state char)
		 (incf position))

	       ;; STATE FUNCTIONS

	       (state/dispatch (c)
		 (setf modifier 1)
		 (ecase c
		   (#\l (prefix 2   #'state/long :long))
		   (#\h (prefix 1/2 #'state/half :half))
		   ((#\d #\i) (token :signed size))
		   (#\u (token :unsigned size))
		   ((#\f #\F #\g #\G #\e #\E) (token :single))
		   (#\s (token :string))
		   (#\m (setf state (expect "%s" :malloc-string)))
		   (#\p (setf state (expect "%u" :buffer)))
		   (#\x (token :file-descriptor))))

	       (state/half (c)
		 (ecase c
		   (#\h (augment-prefix))
		   ((#\i #\d) (token :signed (size-for-prefix)))
		   (#\u (token :unsigned (size-for-prefix)))))

	       (state/long (c)
		 (ecase c
		   (#\l (augment-prefix))
		   ((#\i #\d) (token :signed (size-for-prefix)))
		   (#\u (token :unsigned (size-for-prefix)))
		   ((#\f #\F #\g #\G #\e #\E)
		    (assert (= counter 1)
			    ()
			    "Unexpected float format with more than one prefix %l.")
		    (ecase counter
		      (1 (token :double))))))

	       (state/wait-for-% (c)
		 (ecase c
		   (#\%
		    (incf start)
		    (setf state #'state/dispatch)))))
	(loop
	   initially (setf state #'state/wait-for-%)
	   for char across pomp-format-string
	   do (handler-case (feed-fsm char)
		(error (condition)
		  ;; Any error caught here is a parsing error.
		  (error "~&Invalid character ~s in ~<~s at position ~d.~@:_~
			  ~:*~v@T ^~:>~%~a"
			 char
			 (list pomp-format-string
			       position)
			 condition)))
	   finally
	     (let ((fragment (subseq pomp-format-string start)))
	       (assert (eq state #'state/wait-for-%)
		       ()
		       "Not enough input in ~<~s for token started ~
			at position ~d.~@:_~:*~v@T ^~:>~%~
			~a"
		       (list pomp-format-string start)
		       (etypecase if-empty
			 ((member :half :long)
			  (format nil
				  "Incomplete '~(~a~)' modifier."
				  if-empty))
			 (cons (destructuring-bind (tok expect) if-empty
				 (format nil
					 "Incomplete ~(~a~) token, ~
					  expected \"~a~a\" instead."
					 tok
					 fragment
					 expect))))
		       fragment
		       start)))))))

(defmacro do-tokens ((token string &optional result) &body body)
  (with-gensyms (list rest)
    `(block nil
       (map-tokens
	,(etypecase token
	   (null `(lambda (,list) (declare (ignore ,list)) ,@body))
	   (symbol `(lambda (,token) ,@body))
	   (cons (destructuring-bind (token &optional arg) token
		   `(lambda (,list)
		      (destructuring-bind (,token ,@(if arg
							`(&optional ,arg)
							`(&rest ,rest)))
			  ,list
			,@(unless arg `((declare (ignore ,rest))))
			,@body)))))
	,string)
       ,result)))

(defun tokens (format &aux tokens)
  (when format
    (do-tokens (token format (nreverse tokens))
      (push token tokens))))

;; (tokens "%p%u%f%d%hhu")
;; => ((:BUFFER) (:SINGLE) (:SIGNED 32) (:UNSIGNED 8))

;; (mapcar #'token-as-types '((:BUFFER) (:SINGLE) (:SIGNED 32) (:UNSIGNED 8)))
;; => (:BUFFER :SINGLE-FLOAT :SIGNED-ZIG-VARINT :UNSIGNED-BYTE)
