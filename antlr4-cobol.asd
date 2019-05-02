

(in-package :cl-user)
(defpackage antlr4-cobol-asd
  (:use :cl :asdf :uiop))
(in-package :antlr4-cobol-asd)

(defsystem "antlr4-cobol"
  :version "0.0.1"
  :author "hiro.shinke"
  :depends-on (:clpcl
	       :convertantlr4
	       )
  :components ((:file "antlr4-cobol"))
)


	
	       
