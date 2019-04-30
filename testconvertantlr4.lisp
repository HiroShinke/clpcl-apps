


(in-package :cl-user)
(defpackage convertantlr4-test
  (:use :cl :fiveam :convertantlr4 :clpcl :cl-heredoc))
(in-package :convertantlr4-test)

(def-suite :convertantlr4)
(in-suite :convertantlr4)

(defun parse-file (file text)
  (let ((path (asdf:system-relative-pathname :convertantlr4 file)))
    (antlr4-file-parse path text)
    )
  )

(defun parse (str text)
  (antlr4-grammar-parse str text)
  )

(defun grammar-file (file)
  (let ((path (asdf:system-relative-pathname :convertantlr4 file)))
    (antlr4-file-to-parser path)
    )
  )

(defun grammar (str)
  (antlr4-grammar-to-parser str)
  )


(set-dispatch-macro-character #\# #\> #'cl-heredoc:read-heredoc)
;;;;;

(test simple-parse
  "llllllllll"
  (is
   (equalp
    (success 3 '("a" "b" "c"))
    (parse #>eof>

grammar abc;
abc : a b c;
a   : 'a';
b   : 'b';
c   : 'c';

eof
     "abcd")
    )
   )
  )

(test simple-grammar
  "llllllllll"
  (is
   (equalp

    '(CLPCL-DEF-PARSERS
      ((|abc| (CLPCL-SEQ |a| |b| |c|))
       (|a| (CONVERTANTLR4::TOKEN-REGEXP "a"))
       (|b| (CONVERTANTLR4::TOKEN-REGEXP "b"))
       (|c| (CONVERTANTLR4::TOKEN-REGEXP "c")))
      |abc|)

    (grammar #>eof>

grammar abc;
abc : a b c;
a   : 'a';
b   : 'b';
c   : 'c';

eof
)
    )
   )
  )


(test many-parse
  "llllllllll"
  (is
   (equalp
    (success 5 '(("a" "a" "a") "b" "c"))
    (parse #>eof>

grammar abc;
abc : a+ b c;
a   : 'a';
b   : 'b';
c   : 'c';

eof
     "aaabcd")
    )
   )
  )

(test many-grammar
  "llllllllll"
  (is
   (equalp

    '(CLPCL-DEF-PARSERS
      ((|abc| (CLPCL-SEQ (CLPCL-MANY-1 |a|) |b| |c|))
       (|a| (CONVERTANTLR4::TOKEN-REGEXP "a"))
       (|b| (CONVERTANTLR4::TOKEN-REGEXP "b"))
       (|c| (CONVERTANTLR4::TOKEN-REGEXP "c")))
      |abc|)

    (grammar #>eof>

grammar abc;
abc : a+ b c;
a   : 'a';
b   : 'b';
c   : 'c';

eof
)
    )
   )
  )



