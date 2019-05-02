

(in-package :cl-user)
(defpackage antlr4-cobol
  (:use :cl :convertantlr4 :clpcl)
  (:export
   :parse
   ))
(in-package :antlr4-cobol)


(defparameter *antlr4-grammar* "Cobol85.mini.g4")

(defun init-parser ()
  (let* ((path (asdf:system-relative-pathname
		:convertantlr4 *antlr4-grammar*))
	 (str (uiop:read-file-string path)))
    (eval (antlr4-str-to-parser str))
    )
  )

(defparameter *cobol85parser* (init-parser))

(defun parse (str)
  (clpcl-parse *cobol85parser* str)
  )
