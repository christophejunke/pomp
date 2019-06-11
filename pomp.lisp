(in-package :pomp)

(define-enum payload-argument-type 1 ()
  (:signed-byte 1)    ; 8-bit signed integer, data size is 1 byte.
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
  :file-descriptor)   ; File descriptor, little endian, data size is 4 bytes.

(define-constant +magic+ "POMP" :test #'string=)

(defbinary %message (:byte-order :little-endian)
  (magic +magic+ :type (magic :actual-type (fixed-length-string 4) :value +magic+))
  (id 0 :type (unsigned-byte 32))
  (size 0 :type (unsigned-byte 32))
  (payload #() :type (eval `(simple-array (unsigned-byte 8) (,(- size 12))))))

(defbinary pomp-buffer ()
  (size 0 :type (custom
                 :reader 'varint-decode-32
                 :writer 'varint-encode-32
                 :lisp-type (unsigned-byte 32)))
  (bytes #() :type (simple-array (unsigned-byte 8) (size))))

(defbinary payload-argument ()
  (type :buffer :type payload-argument-type)
  (data 0
        :type (eval
               (ecase type
                 (:signed-byte '(signed-byte 8)) 
                 (:unsigned-byte '(unsigned-byte 8)) 
                 (:signed-short '(signed-byte 16)) 
                 (:unsigned-short '(unsigned-byte 16)) 
                 (:signed-zig-varint '(custom
                                       :reader (zig-zag-varint-decode 32)
                                       :writer (zig-zag-varint-encode 32)
                                       :lisp-type (sb 32))) 
                 (:unsigned-varint '(custom
                                     :reader 'varint-decode-32
                                     :writer 'varint-encode-32
                                     :lisp-type (unsigned-byte 32))) 
                 (:signed-zig-varlong '(custom
                                        :reader (zig-zag-varint-decode 64)
                                        :writer (zig-zag-varint-encode 64)
                                        :lisp-type (sb 64))) 
                 (:unsigned-varlong '(custom
                                      :reader 'varint-decode-64
                                      :writer 'varint-encode-64
                                      :lisp-type (unsigned-byte 64))) 
                 (:ascii '(terminated-string 1)) 
                 (:buffer 'pomp-buffer)                
                 (:single-float 'single-float) 
                 (:double-float 'double-float) 
                 (:file-descriptor '(unsigned-byte 32))))))

;; HELPER

(defun buffer (elements)
  (make-pomp-buffer :size (length elements)
                    :bytes (make-bytes elements)))

(defun argument (type data)
  (make-payload-argument :type type :data data))

(defun argument-encode (type data)
  (with-output-to-sequence (o)
    (write-binary (argument type data) o)))

(defun make-payload (arguments)
  (with-output-to-sequence (o)
    (map () (lambda (a) (write-binary a o)) arguments)))

(defun make-message-using-payload (id payload)
  (make-%message :size (+ 12 (length payload))
                 :payload payload
                 :id id))

(defun decode-payload (buffer)
  (with-input-from-sequence (stream buffer)
    (loop
      while (listen stream)
      collect (read-binary 'payload-argument stream))))

;;;; Main functions

(defun make-message (id arguments)
  (make-message-using-payload id (make-payload arguments)))

(defun decode-message (message)
  (values (%message-id message)
          (decode-payload (%message-payload message))))

;; TEST

(let* ((arguments (list
                   (argument :signed-byte 127) 
                   (argument :unsigned-byte 255) 
                   (argument :signed-short -10201) 
                   (argument :unsigned-short 4000) 
                   (argument :signed-zig-varint -75000) 
                   (argument :unsigned-varint 32010) 
                   (argument :signed-zig-varlong -50505050505050) 
                   (argument :unsigned-varlong 10000000000999484838) 
                   (argument :ascii "AZERTYUIOP") 
                   (argument :buffer (buffer #(10 0 30 0 50 10 20)))                
                   (argument :single-float 1e9) 
                   (argument :double-float pi) 
                   (argument :file-descriptor 53)))
       (message (make-message 64 arguments)))
  (multiple-value-bind (id args) (decode-message message)
    (assert (equalp id 64))
    (assert (equalp args arguments))
    (assert (equalp (with-output-to-sequence (stream)
                      (write-binary (make-message id args) stream))
                    #(80 79 77 80 64 0 0 0 89 0 0 0 1 127 2 255 3 39 216 4 160 15 5 239 147 9 6 138
                      250 1 7 179 253 193 249 227 251 22 8 166 219 235 171 204 224 200 227 138 1 9
                      65 90 69 82 84 89 85 73 79 80 0 10 7 10 0 30 0 50 10 20 11 40 107 110 78 12
                      24 45 68 84 251 33 9 64 13 53 0 0 0)))))

