(defpackage cledis
  (:use :cl :usocket :bt-semaphore)
  (:local-nicknames (:us :usocket)
                    (:as :cl-async))
  (:export #:server #:client))
(in-package :cledis)

;; TODOs
;; Thread Pool: https://github.com/kiuma/thread-pool

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Encoding
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
(defun uint32-to-le-bytes (num)
  "Produce a vector of bytes representing the number in little endian"
  (let ((buffer (make-array 4 :element-type '(unsigned-byte 8)))) 
    (dotimes (n 4 buffer)
      (setf (aref buffer n) (ldb (byte 8 (* 8 n)) num)))))

(defun uint32-to-be-bytes (num)
  "Produce a vector of bytes representing the number in big endian"
  (let ((buffer (make-array 4 :element-type '(unsigned-byte 8)))) 
    (dotimes (n 4 buffer)
      (setf (aref buffer n) (ldb (byte 8 (- 32 (* 8 (+ 1 n)))) num)))))

(defun be-bytes-to-uint32 (vec)
  (logior
   (ash (aref vec 0) 24)
   (ash (aref vec 1) 16)
   (ash (aref vec 2) 8)
   (ash (aref vec 3) 0)))

(defun le-bytes-to-uint32 (vec)
  (logior
   (ash (aref vec 3) 24)
   (ash (aref vec 2) 16)
   (ash (aref vec 1) 8)
   (ash (aref vec 0) 0)))
 
(defun str-to-bytes (str)
  (map 'vector (lambda (c) (char-code c)) str))

(defun bytes-to-str (bytes)
  (map 'string (lambda (c) (code-char c)) bytes))

;; This will break for non ASCII text, I would have to use UTF-8 encoding
(defun str-coerce-bytes (str)
  (coerce (str-to-bytes str) '(vector (unsigned-byte 8))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Protocol
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun push-num (vec n)
  (let ((bytes (uint32-to-be-bytes n)))
    (dotimes (i (length bytes))
      (vector-push-extend (aref bytes i) vec (+ 1 (length vec))))))

(defun push-str (vec str)
  (let ((bytes (str-coerce-bytes str)))
    (push-num vec (length bytes))
    (dotimes (i (length bytes))
      (vector-push-extend (aref bytes i) vec (+ 1 (length vec))))))

(defun naive-encode-msg (ops)
  "Encode a list of operations into bytes. The input looks like this: '((set one 1) (get two))"
  (let* ((vec (make-array (* (length ops) 30) :element-type '(unsigned-byte 8) :fill-pointer 0 :adjustable t))
         (str (princ-to-string ops))) 
    (push-str vec str)
    (let ((buffer (make-array (length vec) :element-type '(unsigned-byte 8))))
     (dotimes (i (length vec) buffer)
       (setf (aref buffer i) (aref vec i))))))

(defun naive-decode-msg (buffer stream)
  "Encode a list of operations into bytes. The input looks like this: '((set one 1) (get two))"
  (let* ((len (rec-int buffer stream))
         (str (rec-payload buffer stream len)))
    (read-from-string str)))


; I could use read-from-string & princ-to-string to simplify parsing
(defun push-op (vec op)
  "Push the encoding of a PUT operation, with a key and a value, e.g. '(thing . 23)"
  (let* ((key (symbol-name (car op)))
         (val (cdr op))
         (str (format nil "SET ~a = ~a" key val)))
    (push-str vec str)))

(defun encode-msg (ops)
  "Encode a list of SET operations into bytes. The input looks like this: '((one 1) (two 2))"
  (let* ((len (length ops))
         (vec (make-array (* len 30) :element-type '(unsigned-byte 8) :fill-pointer 0 :adjustable t)))
    (push-num vec len)
    (dolist (op ops vec)
      (push-op vec op))))



(defun send-int (n socket)
  (as:write-socket-data socket (uint32-to-be-bytes n)))

(defun rec-int (buffer stream)
  (read-sequence buffer stream :end 4)
  (be-bytes-to-uint32 buffer))

(defun send-payload (str socket)
  (as:write-socket-data socket (str-coerce-bytes str)))

(defun rec-payload (buffer stream length)
  (read-sequence buffer stream :end length)
  (bytes-to-str (subseq buffer 0 length)))

(defun send-msg (str socket)
  (send-int (length str) socket)
  (send-payload str socket))

(defun rec-msg (buffer stream)
  (let ((len (rec-int buffer stream)))
    (rec-payload buffer stream len)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Execution 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-ops (ops)
  (mapcar (lambda (op)
            (case (car op)
              (set (progn
                     (key-value:put-value (cadr op) (caddr op))
                     'ok))
              (get (cons (cadr op) (key-value:get-value (cadr op))))))
          ops))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *server* nil)

(defun start-server (port)
  (format t "Starting Server~%")
  (setf *server* 
        (cl-async:tcp-server nil port
                             (lambda (socket stream)
                               (format t "[SERVER] Ready to read~%")
                               (let* ((buffer (make-array 1024 :element-type '(unsigned-byte 8)))
                                      (ops (naive-decode-msg buffer stream)))
                                 (format t "[SERVER] This is the message: ~a~%" ops)
                                 (send-msg (princ-to-string (run-ops ops)) socket))
                               (format t "[SERVER] That was it.~%"))
                             :event-cb (lambda (err) (format t "[SERVER] Event: ~a~%" err))
                             :stream t))
  (cl-async:signal-handler 2 (lambda (sig)
                               (declare (ignore sig))
                               (cl-async:exit-event-loop))))
(defun stop-server ()
  (when *server* (as:close-tcp-server *server*)))

(defun launch-async-server (port)
  (when *server*
    (format t "Restarting Server~%")
    (as:close-tcp-server *server*)
    (setf *server* nil))
  (bt:make-thread (lambda ()
                    (cl-async:start-event-loop (lambda () (start-server port)))) :name "Server"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; client
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun start-client (port query)
  (as:tcp-connect "127.0.0.1" port
                  (lambda (socket stream)
                    (declare (ignore socket))
                    (let ((buffer (make-array 1024 :element-type '(unsigned-byte 8))))
                      (format t "[CLIENT] Respose: ~a~%" (rec-msg buffer stream))
                      (as:exit-event-loop)))
                  :event-cb (lambda (event) (format t "[CLIENT] Event received: ~a~%" event))
                  :stream t
                  :data (naive-encode-msg query)
                  :read-timeout 5))

;; TODO This should be done using promises instead
;; http://orthecreedence.github.io/blackbird/
(defun run-query (port query)
   (as:start-event-loop (lambda () (start-client port query))))


(defun launch-client (port)
  (let ((query '((SET one 1) (GET one) (SET one "dog") (GET one))))
    (bt:make-thread (lambda () (as:start-event-loop (lambda () (start-client port query)))) :name "Client")))



(defun create-bin-client (port)
  (usocket:with-client-socket (socket stream "127.0.0.1" port :element-type '(unsigned-byte 8))
    (unwind-protect
         (let ((buffer (make-array 1024 :element-type '(unsigned-byte 8))))
           (write-sequence (uint32-to-be-bytes 2) stream)
           (write-sequence #(67 67) stream)
           (force-output stream)
           (format t "[CLIENT] Writing to socket, sent a 2 and two letters: ~%")
           (usocket:wait-for-input socket)
           (read-sequence buffer stream :end 4)
           (read-sequence buffer stream :end 10)
           (format t "[CLIENT] Got something: ~a~%" (bytes-to-str (subseq buffer 0 10))))
      (progn
        (format t "[CLIENT] Client done!~%")
        (usocket:socket-close socket)))))

(defun launch-bin-client (port)
  (bt:make-thread (lambda () (create-bin-client port)) :name "Client"))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; entry points 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun server ()
  (flamegraph:save-flame-graph ("~/server.stack")
    (when *server*
      (format t "Restarting Server~%")
      (as:close-tcp-server *server*)
      (setf *server* nil))
    (handler-case
        (cl-async:start-event-loop (lambda () (start-server 12342)))
      (error (c)
        (format t "ERROR: ~a~%" c)))))

(defun run-client (name times) 
  (bt:make-thread (lambda ()
                    (format t "Running ~a!~%" name)
                    (dotimes (n times)
                      (handler-case 
                          (run-query 12342 `((set ,(format nil "~a~a" name n) ,n) (get ,(format nil "~a~a" name n))))
                        (error (c)
                          (format t "ERROR: ~a~%" c)))))
                  :name name))

(defun client ()
  (let ((t1 (run-client "one" 10000))
        (t2 (run-client "two" 10000))
        (t3 (run-client "three" 10000))
        (t4 (run-client "four" 10000)))
    (bt:join-thread t1)
    (bt:join-thread t2)
    (bt:join-thread t3)
    (bt:join-thread t4)))

