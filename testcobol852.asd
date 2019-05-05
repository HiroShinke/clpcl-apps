

(in-package :cl-user)
(defpackage testcobol852-asd
  (:use :cl :asdf :uiop))
(in-package :testcobol852-asd)

(defsystem "testcobol852"
  :version "0.0.1"
  :author "hiro.shinke"
  :depends-on (:clpcl2
	       :convertantlr42
	       :cl-heredoc
	       :fiveam
	       :antlr4-cobol2
	       )
  :components ((:file "testcobol85"))
  :perform (test-op (o s)
		    (symbol-call :fiveam :run! :testcobol85))  
)


	
	       
