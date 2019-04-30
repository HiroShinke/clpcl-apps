

(in-package :cl-user)
(defpackage convertantlr4-asd
  (:use :cl :asdf :uiop))
(in-package :convertantlr4-asd)

(defsystem "convertantlr4"
  :version "0.0.1"
  :author "hiro.shinke"
  :depends-on (:clpcl
	       :optima
	       )
  :components ((:file "convertantlr4"))
)

