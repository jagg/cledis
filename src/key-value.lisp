(defpackage key-value 
  (:use :cl :bt-semaphore)
  (:export #:put-value #:get-value))
(in-package :key-value)

(defvar *store* (make-hash-table :test 'equal))
(defvar *lock* (bt:make-lock))

(defun put-value (key val)
  (bt:with-lock-held (*lock*)
    (setf (gethash key *store*) val)))

(defun get-value (key)
  (bt:with-lock-held (*lock*)
    (gethash key *store*)))
