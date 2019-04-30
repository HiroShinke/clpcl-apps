


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
  "simple"
  (let ((g #>eof>

grammar abc;
abc : a b c;
a   : 'a';
b   : 'b';
c   : 'c';

eof))

    (is
     (equalp
      '(CLPCL-DEF-PARSERS
	((|abc| (CLPCL-SEQ |a| |b| |c|))
	 (|a| (CONVERTANTLR4::TOKEN-REGEXP "a"))
	 (|b| (CONVERTANTLR4::TOKEN-REGEXP "b"))
	 (|c| (CONVERTANTLR4::TOKEN-REGEXP "c")))
	|abc|)
      (grammar g)
      )
     )
    (is
     (equalp
      (success 3 '("a" "b" "c"))
      (parse g "abcd")
      )
     )
    )
  )

(test many-parse
  "simple"
  (let ((g #>eof>

grammar abc;
abc : a+ b c;
a   : 'a';
b   : 'b';
c   : 'c';

eof))

    (is
     (equalp
      '(CLPCL-DEF-PARSERS
	((|abc| (CLPCL-SEQ (CLPCL-MANY-1 |a|) |b| |c|))
	 (|a| (CONVERTANTLR4::TOKEN-REGEXP "a"))
	 (|b| (CONVERTANTLR4::TOKEN-REGEXP "b"))
	 (|c| (CONVERTANTLR4::TOKEN-REGEXP "c")))
	|abc|)
      (grammar g)
      )
     )
    (is
     (equalp
      (success 5 '(("a" "a" "a") "b" "c"))
      (parse g "aaabcd")
      )
     )
    )
  )

(test class-parse
  "simple"
  (let ((g #>eof>

grammar abc;
abc : a b c;
a   : [a-z];
b   : 'b';
c   : 'c';

eof))

    (is
     (equalp
      '(CLPCL-DEF-PARSERS
	((|abc| (CLPCL-SEQ |a| |b| |c|))
	 (|a| (CONVERTANTLR4::TOKEN-REGEXP "[a-z]"))
	 (|b| (CONVERTANTLR4::TOKEN-REGEXP "b"))
	 (|c| (CONVERTANTLR4::TOKEN-REGEXP "c")))
	|abc|)
      (grammar g)
      )
     )
    (is
     (equalp
      (success 3 '("z" "b" "c"))
      (parse g "zbcd")
      )
     )
    )
  )


(test or-parse
  "simple"
  (let ((g #>eof>

grammar abc;
abc : a (b | c);
a   : 'a';
b   : 'b';
c   : 'c';

eof))

    (is
     (equalp
      '(CLPCL-DEF-PARSERS
	((|abc| (CLPCL-SEQ |a| (CLPCL-OR |b| |c|)))
	 (|a| (CONVERTANTLR4::TOKEN-REGEXP "a"))
	 (|b| (CONVERTANTLR4::TOKEN-REGEXP "b"))
	 (|c| (CONVERTANTLR4::TOKEN-REGEXP "c")))
	|abc|)
      (grammar g)
      )
     )
    (is
     (equalp
      (success 2 '("a" "b"))
      (parse g "abcd")
      )
     )
    (is
     (equalp
      (success 2 '("a" "c"))
      (parse g "acbd")
      )
     )
    )
  )


(test or-parse
  "simple"
  (let ((g #>eof>

grammar abc;
abc : a (b | c)+;
a   : 'a';
b   : 'b';
c   : 'c';

eof))

    (is
     (equalp
      '(CLPCL-DEF-PARSERS
	((|abc| (CLPCL-SEQ |a| (CLPCL-MANY-1 (CLPCL-OR |b| |c|))))
	 (|a| (CONVERTANTLR4::TOKEN-REGEXP "a"))
	 (|b| (CONVERTANTLR4::TOKEN-REGEXP "b"))
	 (|c| (CONVERTANTLR4::TOKEN-REGEXP "c")))
	|abc|)
      (grammar g)
      )
     )
    (is
     (equalp
      (success 3 '("a" ("b" "c")))
      (parse g "abcd")
      )
     )
    (is
     (equalp
      (success 3 '("a" ("c" "b")))
      (parse g "acbd")
      )
     )
    )
  )


(test or2-parse
  "simple"
  (let ((g #>eof>

grammar abc;
abc : a b+;
a   : 'a';
b   : 'b'|'c';

eof))

    (is
     (equalp
      '(CLPCL-DEF-PARSERS
	((|abc| (CLPCL-SEQ |a| (CLPCL-MANY-1 |b|)))
	 (|a| (CONVERTANTLR4::TOKEN-REGEXP "a"))
	 (|b| (CLPCL-OR
                (CONVERTANTLR4::TOKEN-REGEXP "b")
                (CONVERTANTLR4::TOKEN-REGEXP "C"))))
	|abc|)
      (grammar g)
      )
     )
    (is
     (equalp
      (success 3 '("a" ("b" "c")))
      (parse g "abcd")
      )
     )
    (is
     (equalp
      (success 3 '("a" ("c" "b")))
      (parse g "acbd")
      )
     )
    )
  )


(test or3-parse
  "simple"
  (let ((g #>eof>

grammar abc;
abc : a b+;
a   : 'a';
b   : 'b'|'c';

eof))

    (is
     (equalp
      '(CLPCL-DEF-PARSERS
	((|abc| (CLPCL-SEQ |a| (CLPCL-MANY-1 |b|)))
	 (|a| (CONVERTANTLR4::TOKEN-REGEXP "a"))
	 (|b| (CLPCL-OR
                (CONVERTANTLR4::TOKEN-REGEXP "b")
                (CONVERTANTLR4::TOKEN-REGEXP "c"))))
	|abc|)
      (grammar g)
      )
     )
    (is
     (equalp
      (success 3 '("a" ("b" "c")))
      (clpcl-parse (eval (grammar g)) "abcd")
      )
     )
    (is
     (equalp
      (success 3 '("a" ("c" "b")))
      (clpcl-parse (eval (grammar g)) "acbd")
      )
     )
    )
  )









