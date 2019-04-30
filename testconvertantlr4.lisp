


(in-package :cl-user)
(defpackage convertantlr4-test
  (:use :cl :fiveam :convertantlr4 :clpcl :cl-heredoc))
(in-package :convertantlr4-test)

(def-suite :convertantlr4)
(in-suite :convertantlr4)

(defun parse (file text)
  (let ((path (asdf:system-relative-pathname :convertantlr4 file)))
    (antlr4-grammar-parse path text)
    )
  )

(defun grammar (file)
  (let ((path (asdf:system-relative-pathname :convertantlr4 file)))
    (antlr4-grammar-to-parser path)
    )
  )


;;;;;

(test simple
  "llllllllll"
  (is
   (equalp
    (success 1 '("a" "b" "c"))
    (parse "test1.txt" "abcd")
    )
   )
  )

(test grammar
  "llllllllll"
  (is
   (equalp
    (success 1 '("a" "b" "c"))
    (grammar "test1.txt")
    )
   )
  )

(test complex
  "llllllllll"
  (is
   (equalp
    (success 1 '("a" "b" "c"))
    (parse "test2.txt" "abcd")
    )
   )
  )

