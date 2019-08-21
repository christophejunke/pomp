(in-package :pomp)

(define-constant +magic+ "POMP" :test #'string=)

(define-enum pomp-argument-type 1 ()
  (:signed-byte #x01) ; 8-bit signed integer, data size is 1 byte.
  :unsigned-byte      ; 8-bit unsigned integer, data size is 1 byte.
  :signed-short       ; 16-bit signed integer, little endian, data size is 2 bytes.
  :unsigned-short     ; 16-bit unsigned integer, little endian, data size is 2 bytes.
  :signed-zig-varint  ; 32-bit signed integer, zigzag varint, data size is 1-5 bytes.
  :unsigned-varint    ; 32-bit unsigned integer, varint, data size is 1-5 bytes.
  :signed-zig-varlong ; 64-bit signed integer, zigzag varint, data size is 1-10 bytes.
  :unsigned-varlong   ; 64-bit unsigned integer, varint, data size is 1-10 bytes.
  :ascii              ; 8-bit character string with a null character at the end.
  :buffer             ; raw bytes.
  :single-float       ; 32-bit floating point, little endian, IEEE 754, data size is 4 bytes.
  :double-float       ; 64-bit floating point, little endian, IEEE 754, data size is 8 bytes.
  :file-descriptor)   ; file descriptor, little endian, data size is 4 bytes.

(defbinary pomp-message (:byte-order :little-endian)
  (magic +magic+ :type (magic :actual-type (fixed-length-string 4) :value +magic+))
  (id 0 :type (unsigned-byte 32))
  (size 0 :type (unsigned-byte 32))
  (payload #() :type (eval `(simple-array (unsigned-byte 8) (,(- size 12))))))

(defbinary pomp-buffer ()
  (size 0 :type (custom
                 :reader '%varint32d
                 :writer '%varint32e
                 :lisp-type (unsigned-byte 32)))
  (bytes #() :type (simple-array (unsigned-byte 8) (size))))

(defbinary pomp-ascii ()
  (size 0 :type (custom
                 :reader '%varint16d
                 :writer '%varint16e
                 :lisp-type (unsigned-byte 16)))
  (text "" :type (terminated-string 1 :terminator 0)))

(defbinary pomp-argument ()
  (type :buffer
        :type pomp-argument-type)
  (data 0 :type (eval
                 (ecase type
                   (:signed-byte '(signed-byte 8))
                   (:unsigned-byte '(unsigned-byte 8))
                   (:signed-short '(signed-byte 16))
                   (:unsigned-short '(unsigned-byte 16))
                   (:signed-zig-varint '(custom
                                         :reader '%zv32d
                                         :writer '%zv32e
                                         :lisp-type (signed-byte 32)))
                   (:unsigned-varint '(custom
                                       :reader '%varint32d
                                       :writer '%varint32e
                                       :lisp-type (unsigned-byte 32)))
                   (:signed-zig-varlong '(custom
                                          :reader '%zv64d
                                          :writer '%zv64e
                                          :lisp-type (signed-byte 64)))
                   (:unsigned-varlong '(custom
                                        :reader '%varint64d
                                        :writer '%varint64e
                                        :lisp-type (unsigned-byte 64)))
                   (:ascii 'pomp-ascii)
                   (:buffer 'pomp-buffer)
                   (:single-float 'single-float)
                   (:double-float 'double-float)
                   (:file-descriptor '(unsigned-byte 32))))))
