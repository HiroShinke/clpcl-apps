

(in-package :cl-user)
(defpackage testcobol85-asd
  (:use :cl :asdf :uiop))
(in-package :testcobol85-asd)

(defsystem "testcobol85"
  :version "0.0.1"
  :author "hiro.shinke"
  :depends-on (:clpcl
	       :convertantlr4
	       :cl-heredoc
	       :fiveam
	       :antlr4-cobol
	       )
  :components ((:file "testcobol85"))
  :perform (test-op (o s)
		    (symbol-call :fiveam :run! :testcobol85))  
)


	
	       
