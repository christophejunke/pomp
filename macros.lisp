(in-package :pomp)

(defmacro define-zig-zag-functions (n encoder decoder)
  (check-type n (integer 1))
  (let* ((optimizations '((speed 3) (safety 0) (debug 0) (compilation-speed 0)))
         (muffle #+sbcl '((sb-ext:muffle-conditions sb-ext:compiler-note)))
         (declarations `((declare (optimize ,@optimizations)))))
    (with-gensyms (in)
      `(locally (declare ,@muffle)
         (declaim (inline ,encoder decoder))
         (declaim (ftype (fun (sb ,n) (ub ,n)) ,encoder)
                  (ftype (fun (ub ,n) (sb ,n)) ,decoder))
         (defun ,encoder (,in)
           ,@declarations
           (logxor (ldb (byte ,n 0) (ash ,in 1))
                   (ldb (byte ,n 0) (ash ,in ,(- 1 n)))))
         (defun ,decoder (,in)
           ,@declarations
           (logxor (ash ,in -1)
                   (- (ldb (byte 1 0) ,in))))))))

(defmacro define-varint-functions (size encoder decoder)
  (let ((qualities '((speed 3) (debug 0) (safety 0) (compilation-speed 0))))
    (multiple-value-bind (ceiling neg) (ceiling size 7)
      (let ((remaining (+ 7 neg)) (multiple (* 7 (1- ceiling))))
        (with-gensyms (stream decoded byte counter position more value)
          `(locally
               (declare #+sbcl
                        (sb-ext:muffle-conditions sb-ext:compiler-note))
             (declaim (inline ,encoder ,decoder))

             (defun ,encoder (,value ,stream)
               ,(format nil
                        "Variable-length encoder of ~d bits VALUE to STREAM."
                        size)
               (check-type ,value (ub ,size))
               (let ((,counter 0)
                     (,byte 0)
                     (,more))
                 (declare (type (integer 0 ,ceiling) ,counter)
                          (type u8 ,byte)
                          (optimize ,@qualities))
                 (loop
                   (setf ,byte (ldb (byte 7 0) ,value))
                   (setf ,value (ash ,value -7))
                   (setf ,more (plusp ,value))
                   (when ,more
                     (setf (logbitp 7 ,byte) t))
                   (write-byte ,byte ,stream)
                   (incf ,counter)
                   (unless ,more
                     (return ,counter)))))

             (defun ,decoder (,stream &aux (,decoded 0) (,byte 0))
               ,(format nil
                        "Variable-length decoder for ~d bits values ~
                         from STREAM.~%~%~
                         Returns the numbers of bytes read from STREAM ~
                         as a secondary return value."
                        size)
               (declare (type u8 ,byte)
                        (type (ub ,size) ,decoded)
                        (optimize ,@qualities))
               (do ((,counter 1 (+ 1 ,counter))
                    (,position 0 (+ 7 ,position)))
                   ((= ,counter ,ceiling))
                 (declare (type (integer 0 ,ceiling) ,counter)
                          (type (integer 0 ,multiple) ,position))
                 (setf ,byte (read-byte ,stream))
                 (setf (ldb (byte 7 ,position) ,decoded) ,byte)
                 (unless (logbitp 7 ,byte)
                   (return-from ,decoder (values ,decoded ,counter))))
               ,@(when (plusp remaining)
                   `((setf ,byte (read-byte ,stream))
                     (setf (ldb (byte ,remaining ,multiple) ,decoded) ,byte)
                     (unless (logbitp 7 ,byte)
                       (return-from ,decoder (values ,decoded ,ceiling)))))
               (error "overflow"))))))))
