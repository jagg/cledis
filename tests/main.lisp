(defpackage cledis/tests/main
  (:use :cl
        :cledis
        :rove))
(in-package :cledis/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :cledis)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
