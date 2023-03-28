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

;; (with-input-from-sequence (in (mapcar (lambda (s) (parse-integer s :radix 16))
;;                                       (ppcre:split '(:greedy-repetition 1 nil (:alternation #\space #\newline))
;;                                                    "50 4f 4d 50 01 00 ef 60 41 00 00 00 0a 33 0a 31 
;; 0a 0c 64 65 66 61 75 6c 74 2e 67 72 69 64 1a 21 
;; 0a 1f 0d cd cc cc 3e 10 40 18 20 20 64 28 9c ff 
;; ff ff ff ff ff ff ff 01 30 06 38 01 45 cd cc cc 
;; 3f 50 4f 4d 50 01 00 ef 60 41 00 00 00 0a 33 0a 
;; 31 0a 0c 64 65 66 61 75 6c 74 2e 67 72 69 64 1a 
;; 21 0a 1f 0d cd cc cc 3e 10 40 18 20 20 64 28 9c 
;; ff ff ff ff ff ff ff ff 01 30 06 38 01 45 cd cc")))
;;   (read-binary 'pomp-message in))
;;
;; #S(POMP-MESSAGE
;;    :MAGIC "POMP"
;;    :ID 1626275841
;;    :SIZE 65
;;    :PAYLOAD #(10 51 10 49 10 12 100 101 102 97 117 108 116 46 103 114 105 100
;;               26 33 10 31 13 205 204 204 62 16 64 24 32 32 100 40 156 255 255
;;               255 255 255 255 255 255 1 48 6 56 1 69 205 204 204 63))
;; 65

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
  (define-pomp-messages ()
    (ping (pid text) :format "%hu%s" :id 100)
    (pong (pid text) :format "%hu%s")
    (void ())))

(defpomp test-msg () :id 1024)

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

