(defsystem "cledis"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on (:bt-semaphore :usocket :cl-async)
  :components ((:module "src"
                :components
                ((:file "main" :depends-on ("key-value"))
                 (:file "key-value"))))
  :description ""
  :in-order-to ((test-op (test-op "cledis/tests"))))

(defsystem "cledis/tests"
  :author ""
  :license ""
  :depends-on ("cledis"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for cledis"
  :perform (test-op (op c) (symbol-call :rove :run c)))
