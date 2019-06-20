(in-package :pomp)

;; ZIG-ZAG

(define-zig-zag-functions 32 %zz32e %zz32d)
(define-zig-zag-functions 64 %zz64e %zz64d)

;; VARINT
;;
;; variable length encoding of unsigned values
;;

(define-varint-functions 16 %varint16e %varint16d)
(define-varint-functions 32 %varint32e %varint32d)
(define-varint-functions 64 %varint64e %varint64d)

;; ZIG-ZAG of VARINT
;;
;; variable length encoding of signed values
;;

(declaim (inline %zv32e %zv32d %zv64e %zv64d))

(defun %zv32e (v s)
  (%varint32e (%zz32e v) s))

(defun %zv64e (v s)
  (%varint64e (%zz64e v) s))

(defun %zv32d (s)
  (multiple-value-bind (v c) (%varint32d s)
    (values (%zz32d v) c)))

(defun %zv64d (s)
  (multiple-value-bind (v c) (%varint64d s)
    (values (%zz64d v) c)))
