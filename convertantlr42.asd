

(in-package :cl-user)
(defpackage convertantlr42-asd
  (:use :cl :asdf :uiop))
(in-package :convertantlr42-asd)

(defsystem "convertantlr42"
  :version "0.0.1"
  :author "hiro.shinke"
  :depends-on (:clpcl2
	       :optima
	       )
  :components ((:file "convertantlr4"))
  :in-order-to ((test-op (test-op test-convertantlr4)))
)

