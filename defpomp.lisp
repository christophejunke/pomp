(in-package :pomp)

(defun expand-binder-form (arguments message body)
  `(destructuring-bind ,arguments (extract-arguments (pomp-decode ,message))
     ,@body))

(defun message-maker-form (name identifier arguments format)
  (loop
     with original = arguments
     for token in (tokens format)
     for index from 1
     for (arg form) = (multiple-value-bind (pomp-type lisp-type)
			  (token-as-types token)
			(let ((argument
			       (or (pop arguments)
				   (error
				    "~@<Not enough arguments in ~a.~@:_~
 				     ~@(~:r~) token of type ~a has no ~
 				     associated argument.~:>"
				    original
				    index
				    (first token)))))
			  (list argument
				(make-argument-form pomp-type
						    `(coerce ,argument
							     ',lisp-type)))))
     collect arg into args
     collect form into arg-forms
     finally
       (when arguments
	 (error "Too many arguments: ~a" arguments))
       (return `(defun ,name (,@args)
		  (make-message
		   ,identifier
		   (list ,@arg-forms))))))

(defgeneric extract-arg (argument)
  (:method ((argument pomp-ascii))
    (pomp-ascii-text argument))
  (:method ((argument pomp-buffer))
    (pomp-buffer-bytes argument))
  (:method (argument) argument)
  (:method ((argument pomp-argument))
    (extract-arg (pomp-argument-data argument))))

(defun extract-arguments (args)
  (mapcar #'extract-arg args))

(defun check-collision (id name)
  (let ((current (gethash id *pomp-messages* name)))
    (restart-case
	(assert (eq name current)
		()
		"Identifier ~d already corresponds to message ~s."
		id current)
      (replace ()
	:report "Replace known message for this identifier."))))

(defmacro defpomp (name (&rest arguments) &key id (format ""))
  `(progn (check-collision ',id ',name)
	  (defpomp%% ,name (,@arguments) :id ,id :format ,format)))

(defmacro defpomp%% (name (&rest arguments) &key id format)
  (check-type id (unsigned-byte 32))
  (with-gensyms (message body)
    (let ((with (symbolicate 'with- name)))
      `(progn
	 (declaim (inline ,name))
	 (setf (gethash ,id *pomp-messages*) ',name)
	 ,(message-maker-form name id arguments format)
	 (setf (get ',name 'pomp-metadata) ',with)
	 (defmacro ,with (,arguments ,message &body ,body)
	   (expand-binder-form (list ,@arguments) ,message ,body))
	 (list ',name ',with)))))

;; reusing constructor syntax as maching clauses
;; (which enables ElDoc for pattern matching).
;; TODO: group messages by families (package?)
;; (ids are unique in families, but not necessarily globally unique).
;; (alternatively: array of callback functions indexed by numeric id).
;; TODO: exhaustive check when matching on a family of messages
(defmacro pomp-match (message &body clauses)
  (once-only (message)
    `(case (pomp-id ,message)
       ,@(loop
	    for clause in clauses
	    collect
	      (if (member (first clause) '(t otherwise))
		  clause
		  (destructuring-bind ((name &rest args) &rest body) clause
		    (let ((with (get name 'pomp-metadata)))
		      (list name
			    `(,with ,args ,message ,@body)))))))))

(defmacro define-pomp-messages ((&optional family) &body body)
  (declare (ignore family))
  (let ((counter 0)
	(ids (make-hash-table)))
    (flet ((parse (name &key (id nil idp) format)
	     (if idp
		 (setf counter id)
		 (setf id counter))
	     (check-type id (unsigned-byte 32))
	     (prog1 `(,@(and fp (list :format format))
                      :id ,id)
	       (incf counter)
	       (when-let (existing (gethash id ids))
		 (error "Duplicate ID ~d for ~a and ~a."
			id name existing))
	       (setf (gethash id ids) name))))
      (loop
	for (name args . rest) in body
	for plist = (apply #'parse name rest)
	collect `(defpomp%% ,name ,args ,@plist) into defs
	collect `(check-collision ',(getf plist :id) ',name) into chks
	finally (return `(progn ,@chks (list ,@defs)))))))
