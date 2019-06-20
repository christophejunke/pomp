(in-package :pomp)

(defun buffer (elements)
  (make-pomp-buffer :size (length elements)
                    :bytes (make-bytes elements)))

(defun ascii (string)
  (make-str :size (1+ (length string)) :text string))

(defun argument (type data)
  (make-pomp-argument :type type :data data))

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
  (values (pomp-message-id message)
          (decode-payload (pomp-message-payload message))))

