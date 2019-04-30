
(in-package :cl-user)
(defpackage convertantlr4
  (:use :cl :clpcl :optima)
  (:export
   :test
   :antlr4-grammar-to-parser
   :antlr4-grammar-parse
   ))
(in-package :convertantlr4)

(defstruct (<grammar-def>
	     (:constructor <grammar-def>(name)))
  name
  parsers
  )

(defstruct (<grammar>
	     (:constructor <grammar>(name rhs)))
  name
  rhs
  )

(defstruct (<fragment>
	     (:constructor <fragment>(name rhs)))
  name
  rhs
  )

(defstruct (<ident>
	     (:constructor <ident>(name)))
  name
  )

(defstruct (<literal>
	     (:constructor <literal>(literal)))
  literal
  )

(defstruct (<char-class>
	     (:constructor <char-class>(char-class)))
  char-class
  )

(defstruct (<not-support>
	     (:constructor <not-support>))
  )

(defstruct (<factor>
	     (:constructor <factor>(not-flag body rep-flag)))
  not-flag
  body
  rep-flag
  )

(defstruct (<or>
	     (:constructor <or>(parsers)))
  parsers
  )

(defstruct (<seq>
	     (:constructor <seq>(parsers)))
  parsers
  )


(defun token2 (p)
  (let* (
	 (comment (clpcl-let ((cb (clpcl-regexp "/\\*"))
			      (rest (clpcl-many-till
				     (clpcl-regexp "(.|\\n)")
				     (clpcl-regexp "\\*/"))))
			     nil ))
	 
	 (comment2 (clpcl-let ((cb (clpcl-regexp "//"))
			       (rest (clpcl-many-till
				      (clpcl-regexp ".")
				      (clpcl-regexp "\\n"))))
			      nil ))
	 (spaces   (clpcl-many
		    (clpcl-or
		     (clpcl-regexp "\\s+")
		     comment
		     comment2
		     )))
	 )
    (clpcl-token p spaces)
    )
  )

(defun token-regexp (regexp)
  (token2 (clpcl-regexp regexp)))

(defun token-string ()
  (token2 (clpcl-string #\')))

(defun token-char-class ()
  (token2 (clpcl-regexp "\\[[^\\]]+\\]")))


(defun antlr4 ()

  (clpcl-def-parsers

   ((ident (clpcl-let ((x (token-regexp "[a-zA-Z][_a-zA-Z\\d]*")))
		      (<ident> x)))

    (grammarDef (clpcl-let ((x (token-regexp "grammar"))
			    (s ident)
			    (z (token-regexp ";"))
			    )
			(<grammar-def> s)))


    (fragment (clpcl-let ((nil (token-regexp "fragment"))
			  (x ident)
			  (y (token-regexp ":"))
			  (o orseq)
			  (z (token-regexp ";"))
			  )
			 (<fragment> x o)))

    (grammar (clpcl-let ((x ident)
			 (y (token-regexp ":"))
			 (o orseq)
			 (z (token-regexp ";"))
			 )
			(<grammar> x o)))

    (seq (clpcl-let ((xs (clpcl-many factor1)))
		    (<seq> xs)))
		    
    (orseq (clpcl-let ((xs (clpcl-sep-by-1
			    seq
			    (token-regexp "\\|")
			    )))
		      (<or> xs)))
    
    (factor (clpcl-or (clpcl-let ((x (token-regexp "channel\\(HIDDEN\\)")))
				 (<not-support>))
		      ident
		      (clpcl-let ((x (token-string)))(<literal> x))
		      (clpcl-let ((x (token-char-class))) (<char-class> x))
		      (clpcl-let ((x (token-regexp "->"))) (<not-support>))
		      (clpcl-paren (token-regexp "\\(")
				   orseq
				   (token-regexp "\\)"))))

    (factor1 (clpcl-let ((o0 (clpcl-option (token-regexp "~")))
			 (x factor)
			 (o (clpcl-option
			     (clpcl-or
			      (token-regexp "\\+")
			      (token-regexp "\\*")
			      (token-regexp "\\?")))))
			(<factor> o0 x o)))

    
    (parser (clpcl-let ((g grammarDef)
			(parsers (clpcl-many
				  (clpcl-or 
				   fragment
				   grammar
				   ))))
		       (setf (<grammar-def>-parsers g) parsers)
		       g
		       ))
    )
   parser
   )
  )

(defun antlr4-grammar-to-parser (path)
  (let*((str (uiop:read-file-string path))
	(parser (antlr4))
	(rs (clpcl-parse parser str))
	)
    (match rs
      ((success :value r)
       (build-parser r)))))

(defun antlr4-grammar-parse (path text)
  (let ((p (antlr4-grammar-to-parser path)))
    (if p
	(clpcl-parse (eval p) text))))


(defun test (&optional (file "test.txt") (text "abc") )
  (let ((path (asdf:system-relative-pathname :convertantlr4 file)))
    (antlr4-grammar-parse path text)
    )
  )

(defun build-parsers (xs)
  (mapcar #'build-parser
	  (remove-if (lambda (x)
		       (match x
			 ((<not-support>)
			  x))
		       )
		     xs))
  )

(defun build-parser (g)
  (match g
    ((<grammar-def> :name x :parsers ps)
     `(clpcl-def-parsers
       (,@(mapcar #'build-parser ps))
       ,(intern (<ident>-name x))))
    ((<grammar> :name x :rhs xs)
     (list (intern (<ident>-name x))
	   (build-parser xs)))
    ((<fragment> :name x :rhs xs)
     (list (intern (<ident>-name x))
	   (build-parser xs)))
    ((<or> :parsers xs)
     (if (= (length xs) 1)
	 (build-parser (car xs))
	 `(clpcl-or ,@(build-parsers xs))))
    ((<seq> :parsers xs)
     (if (= (length xs) 1)
	 (build-parser (car xs))
	 `(clpcl-seq ,@(build-parsers xs))))
    ((<literal> literal)
     `(token-regexp ,(subseq literal
			     1
			     (- (length literal) 1) )))
    ((<char-class> char-class)
     `(token-regexp ,char-class))
    ((<factor> :not-flag n :body b :rep-flag rep)
     (cond
       ((string= "*" rep)
	`(clpcl-many ,(build-parser b)))
       ((string= "+" rep)
	`(clpcl-many-1 ,(build-parser b)))
       (t
	(build-parser b))
       ))
    ((<ident> :name name)
     (intern name))
    )
  )

