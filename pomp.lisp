(in-package :pomp)

(defun buffer (elements)
  (make-pomp-buffer :size (length elements)
                    :bytes (make-bytes elements)))

(defun ascii (string)
  (make-pomp-ascii :size (1+ (length string)) :text string))

(defun argument (type data)
  (make-pomp-argument :type type :data data))

(defgeneric make-argument-form-for (type expression)
  (:method ((type (eql :ascii)) expression)
    `(ascii ,expression))
  (:method ((type (eql :buffer)) expression)
    `(buffer ,expression))
  (:method (type expression) expression))

(defun make-argument-form (type expression)
  `(argument ,type ,(make-argument-form-for type expression)))

(defun argument-encode (type data)
  (with-output-to-sequence (o)
    (write-binary (argument type data) o)))

(defun make-payload (arguments)
  (with-output-to-sequence (o)
    (map () (lambda (a) (write-binary a o)) arguments)))

(defun make-message-using-payload (id payload)
  (make-pomp-message :size (+ 12 (length payload))
                     :payload payload
                     :id id))

(defun decode-payload (buffer)
  (with-input-from-sequence (stream buffer)
    (loop
      while (listen stream)
      collect (read-binary 'pomp-argument stream))))

(defun make-message (id arguments)
  (make-message-using-payload id (make-payload arguments)))

(defun decode-message (message)
  (values
   (decode-payload (pomp-message-payload message))
   (pomp-message-id message)))

(declaim (inline pomp-write pomp-read))

(defun pomp-read (in)
  (typecase in
    (vector (with-input-from-sequence (in in)
              (read-binary-type 'pomp-message in)))
    (stream (read-binary-type 'pomp-message in))))

(defun pomp-write (message &optional binary-stream)
  (let ((stream (typecase binary-stream
                  (stream binary-stream)
                  ((eql t) *standard-output*))))
    (values (if (and stream (subtypep (stream-element-type stream)
                                      '(unsigned-byte 8)))
                (write-binary message stream)
                (let ((array 
                       (with-output-to-sequence (o)
                         (write-binary message o))))
                  (if stream
                      (prin1 array stream)
                      array)))
            message)))
