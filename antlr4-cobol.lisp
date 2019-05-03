

(in-package :cl-user)
(defpackage antlr4-cobol
  (:use :cl :convertantlr4 :clpcl)
  (:export
   :parse
   :parser-expr
   :grammar-ast
   ))
(in-package :antlr4-cobol)


(defparameter *antlr4-grammar* "Cobol85.mini.g4")

(defun grammar-ast ()
  (let* ((path (asdf:system-relative-pathname
		:convertantlr4 *antlr4-grammar*))
	 (str (uiop:read-file-string path)))
    (antlr4-str-to-grammar str)
    )
  )

(defun parser-expr ()
  (multiple-value-bind (g table)
      (grammar-ast)
    ;;    (build-parser g table :debug-all)
    (build-parser g table)    
    )
  )

(defun init-parser ()
  (eval (parser-expr))
  )

(defparameter *cobol85parser* (init-parser))

(defun parse (str)
  (clpcl-parse *cobol85parser* str)
  )
