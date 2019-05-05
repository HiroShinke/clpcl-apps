

(in-package :cl-user)
(defpackage antlr4-cobol2-asd
  (:use :cl :asdf :uiop))
(in-package :antlr4-cobol2-asd)

(defsystem "antlr4-cobol2"
  :version "0.0.1"
  :author "hiro.shinke"
  :depends-on (:clpcl2
	       :convertantlr42
	       )
  :components ((:file "antlr4-cobol")
	       (:static-file "Cobol85.mini.g4"))
  :in-order-to ((test-op (test-op testcobol852)))
  )
	
	       
