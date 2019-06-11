(in-package :pomp)

(deftype i8 () '(signed-byte 8))
(deftype u8 () '(unsigned-byte 8))
(deftype sb (n) `(signed-byte ,n))
(deftype ub (n) `(unsigned-byte ,n))
(deftype fun (in out) `(function (,in) (values ,out &optional)))

(defun make-bytes (spec &optional fill-pointer)
  (flet ((make (&optional initial (size (length initial)))
           (apply #'make-array
                  size
                  :fill-pointer fill-pointer
                  :adjustable nil
                  :element-type 'u8
                  (etypecase initial
                    (number (list :initial-element initial))
                    (sequence (list :initial-contents initial))))))
    (etypecase spec
      ((integer 0) (make 0 spec))
      (string (make (map 'vector #'char-code spec)))
      (sequence (make spec)))))

(defmacro defzigzag (n encoder decoder)
  (check-type n (integer 1))
  (with-gensyms (in)
    `(progn
       (declaim (inline ,encoder decoder))
       (declaim (ftype (fun (sb ,n) (ub ,n)) ,encoder)
                (ftype (fun (ub ,n) (sb ,n)) ,decoder))
       (defun ,encoder (,in)
         (declare (optimize . #1=((speed 3) (safety 1) (debug 3))))
         (logxor (ldb (byte ,n 0) (ash ,in 1))
                 (ldb (byte ,n 0) (ash ,in ,(- 1 n)))))
       (defun ,decoder (,in)
         (declare (optimize . #1#))
         (logxor (ash ,in -1) (- (ldb (byte 1 0) ,in)))))))

(defun varint-encode-32 (value stream)
  (let* ((buffer (make-bytes 5))
         (size (varint:encode-uint32 buffer 0 value)))
    (declare (type (simple-array (ub 8) (5)) buffer))
    (prog1 size
      (write-sequence buffer stream :end size))))

(defun varint-encode-64 (value stream)
  (let* ((buffer (make-bytes 10))
         (size (varint:encode-uint64 buffer 0 value)))
    (declare (type (simple-array (ub 8) (10)) buffer))
    (prog1 size
      (write-sequence buffer stream :end size))))

(defun varint-decode-32 (stream)
  (let ((byte 0)
        (decoded-value 0)
        (position 0)
        (counter 0))
    (declare (type (ub 32) decoded-value)
             (type (ub 8) byte)
             (type (integer 0 28) position)
             (type (integer 0 5) counter)
             (optimize (speed 3) (debug 1) (safety 1)))
    (loop (setf byte (read-byte stream))
          (case counter
            (5 (error "too long"))
            (4 (setf (ldb (byte 3 28) decoded-value) byte))
            (t (setf position
                     (locally
                         (declare (type (integer 0 21) position))
                       (setf (ldb (byte 7 position) decoded-value)
                             byte)
                       (+ position 7)))))
          (incf counter)
          (unless (logbitp 7 byte)
            (return (values decoded-value counter))))))

(defun varint-decode-64 (stream)
  (let ((byte 0)
        (decoded-value 0)
        (position 0)
        (counter 0))
    (declare (type (ub 64) decoded-value)
             (type (ub 8) byte)
             (type (integer 0 63) position)
             (type (integer 0 10) counter)
             (optimize (speed 3) (debug 1) (safety 1)))
    (loop (setf byte (read-byte stream))
          (case counter
            (10 (error "too long"))
            (9 (setf (ldb (byte 1 63) decoded-value) byte))
            (t (setf position
                     (locally (declare (type (integer 0 56) position))
                       (setf (ldb (byte 7 position) decoded-value) byte)
                       (+ position 7)))))
          (incf counter)
          (unless (logbitp 7 byte)
            (locally (declare (#+sbcl sb-ext:muffle-conditions t))
              (return (values decoded-value counter)))))))

