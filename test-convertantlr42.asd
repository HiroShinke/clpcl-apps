

(in-package :cl-user)
(defpackage test-convertantlr4-asd
  (:use :cl :asdf :uiop))
(in-package :test-convertantlr4-asd)

(defsystem "test-convertantlr4"
  :version "0.0.1"
  :author "hiro.shinke"
  :depends-on (:convertantlr42
  	       :fiveam
	       :cl-heredoc
	       )
  :components ((:file "testconvertantlr4"))
  :perform (test-op (o s)
		    (symbol-call :fiveam :run! :convertantlr4)))

	
