build:

	sbcl --eval '(ql:quickload :cledis)' \
       --eval "(sb-ext:save-lisp-and-die #p\"server\" :toplevel #'cledis:server :executable t)"

	sbcl --eval '(ql:quickload :cledis)' \
       --eval "(sb-ext:save-lisp-and-die #p\"client\" :toplevel #'cledis:client :executable t)"
