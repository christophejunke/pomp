(in-package :pomp)

(assert (= 3 (let ((c 0))
               (do-tokens (nil "%d%u%hhd" c)
                 (incf c)))))

(do-tokens (tok "%d%u%hhd")
  (assert (consp tok)))

(do-tokens ((tok) "%d%u%hhd")
  (assert (keywordp tok)))

(do-tokens ((tok arg) "%d%u%hhd")
  (check-type tok keyword)
  (check-type arg (member 8 16 32 64 nil)))

(do-tokens (token "%d")
  (multiple-value-bind (btype ltype) (token-as-types token)
    (print btype)
    (print ltype)))

(block test-varint
  (time
   (dotimes (i 10000)
     (let ((v (random #.(1- (expt 2 64)))))
       (with-input-from-sequence (in
                                  (with-output-to-sequence (out)
                                    (%varint64e v out)))
         (assert (= v (%varint64d in)))))))
  (time
   (dotimes (i 10000)
     (let ((v (random #.(1- (expt 2 32)))))
       (with-input-from-sequence (in
                                  (with-output-to-sequence (out)
                                    (%varint32e v out)))
         (assert (= v (%varint32d in))))))))

(block there-and-back-again
  (let* ((id (random #xFFFFFFFF))
         (arguments (list
                     (argument :signed-byte 127)
                     (argument :unsigned-byte 255)
                     (argument :signed-short -10201)
                     (argument :unsigned-short 4000)
                     (argument :signed-zig-varint -75000)
                     (argument :unsigned-varint 32010)
                     (argument :signed-zig-varlong -50505050505050)
                     (argument :unsigned-varlong 10000000000999484838)
                     (argument :ascii (ascii "AZERTYUIOP"))
                     (argument :buffer (buffer #(10 0 30 0 50 10 20)))
                     (argument :single-float 1e9)
                     (argument :double-float pi)
                     (argument :file-descriptor 53)))
         (message (make-message id arguments)))
    (multiple-value-bind (args id%) (pomp-decode message)
      (assert (equalp id% id))
      (assert (equalp args arguments))
      (assert 
       (equalp 
	(with-output-to-sequence (stream)
          (write-binary (make-message id args) stream))
        (concatenate 'simple-vector
                     #(80 79 77 80)
                     (with-output-to-sequence (o)
                       (write-binary-type id '(unsigned-byte 32) o))
                     #(90 0 0 0 1 127 2 255 3 39 216 4 160 15 5
                       239 147 9 6 138 250 1 7 179 253 193 249 227 
		       251 22 8 166 219 235 171 204 224 200 227 138 
		       1 9 11 65 90 69 82 84 89 85 73 79
                       80 0 10 7 10 0 30 0 50 10 20 11 40 107 110 78 
		       12 24 45 68 84 251 33 9 64 13 53 0 0 0)))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-pomp-messages
    (ping (pid text) :format "%hu%s" :id 100)
    (pong (pid text) :format "%hu%s")))

(let ((id0 42) (text0 "test"))
  (let ((ping (ping id0 text0)))
    (with-ping (pid text) ping
      (let ((pong (pong pid text)))
	(with-pong (pid text) pong
	  (assert (= (pomp-message-id ping) 100))
	  (assert (= (pomp-message-id pong) 101))
	  (assert (= pid id0))
	  (assert (string= text text0)))))))

(assert (eq :ping
	    (pomp-match (ping 1 "")
	      ((ping id text)
	       :ping)
	      ((pong id text)
	       :pong)
	      (t :no))))

