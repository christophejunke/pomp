(in-package :pomp)

(deftype sb (n) `(signed-byte ,n))
(deftype ub (n) `(unsigned-byte ,n))
(deftype u8 () '(ub 8))
(deftype fun (in out) `(function (,in) (values ,out &optional)))

(defun make-bytes (spec &optional fill-pointer)
  "Make a byte array based on SPEC with an optional FILL-POINTER.

When FILL-POINTER is NIL, the resulting array is an unidimensional SIMPLE-ARRAY
of octets, and as such, is suitable to be used with byte sequences in various
encoding/decoding functions.

If SPEC is an integer, it specifies the size of the resultin buffer.
If SPEC is a string, the string is coerced to an array of char-codes.
If SPEC is an sequence, it is used as the initial content of the resulting array."
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
