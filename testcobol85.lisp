

(in-package :cl-user)
(defpackage testcobol85
  (:use :cl :fiveam :antlr4-cobol :clpcl :cl-heredoc))
(in-package :testcobol85)

(fiveam:def-suite :testcobol85)
(fiveam:in-suite :testcobol85)

(eval-when (:compile-toplevel)
  (set-dispatch-macro-character #\# #\> #'cl-heredoc:read-heredoc)
  )

(defun parse-result (str)
  (optima:match (parse str)
    ((success :value r)
     r)
    ((failure :pos pos)
     pos)
    )
  )

(test simple-parse
  (is
   (equalp
    '((("IDENTIFICATION" "DIVISION" "."
	("PROGRAM-ID" "." "DDDD" NIL "." NIL) NIL)
       NIL ("DATA" "DIVISION" "." NIL) NIL NIL NIL))

   (parse-result #>eof>

IDENTIFICATION DIVISION. 
PROGRAM-ID. DDDD. 
DATA DIVISION. 

eof)
   )
   )
  )

(test procedure-division
  (is
   (equalp
    '((("IDENTIFICATION" "DIVISION" "." ("PROGRAM-ID" "." "DDDD" NIL "." NIL) NIL)
       NIL ("DATA" "DIVISION" "." (("FILE" "SECTION" "." NIL)))
       ("PROCEDURE" "DIVISION" NIL NIL "." (((((("EXIT" NIL)) ".")) NIL) NIL)) NIL
       NIL))

    (parse-result #>eof>

IDENTIFICATION DIVISION. 
PROGRAM-ID. DDDD. 
DATA DIVISION. 
FILE SECTION. 
PROCEDURE DIVISION. 
EXIT. 
eof)
   )
   )
  )



