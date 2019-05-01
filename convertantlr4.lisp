
(in-package :cl-user)
(defpackage convertantlr4
  (:use :cl :clpcl :optima)
  (:export
   :antlr4-grammar-to-parser
   :antlr4-grammar-parse
   :antlr4-file-to-parser
   :antlr4-file-parse
   ))
(in-package :convertantlr4)

(defstruct (<grammar-def>
	     (:constructor <grammar-def>(name)))
  name
  start
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

;;;;;;;;

(defun token2 (p)
  (let* (
	 (comment (clpcl-let ((cb (clpcl-regexp "/\\*"))
			      (rest (clpcl-many-till
				     (clpcl-regexp "(.|\\n)")
				     (clpcl-regexp "\\*/"))))
			     (declare (ignore cb rest))
			     nil ))
	 
	 (comment2 (clpcl-let ((cb (clpcl-regexp "//"))
			       (rest (clpcl-many-till
				      (clpcl-regexp ".")
				      (clpcl-regexp "\\n"))))	
		      	      (declare (ignore cb rest))
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
			   (declare (ignore x z))
			   (<grammar-def> s)))


    (fragment (clpcl-let ((nil (token-regexp "fragment"))
			  (x ident)
			  (y (token-regexp ":"))
			  (o orseq)
			  (z (token-regexp ";"))
			  )
			 (declare (ignore y z))
			 (<fragment> x o)))

    (grammar (clpcl-let ((x ident)
			 (y (token-regexp ":"))
			 (o orseq)
			 (z (token-regexp ";"))
			 )
			(declare (ignore y z))
			(<grammar> x o)))

    (seq (clpcl-let ((xs (clpcl-many factor1)))
		    (<seq> xs)))
		    
    (orseq (clpcl-let ((xs (clpcl-sep-by-1
			    seq
			    (token-regexp "\\|")
			    )))
		      (<or> xs)))
    
    (factor (clpcl-or (clpcl-let ((x (token-regexp "channel\\(HIDDEN\\)")))
				 (declare (ignore x))
				 (<not-support>))
		      ident
		      (clpcl-let ((x (token-string)))(<literal> x))
		      (clpcl-let ((x (token-char-class))) (<char-class> x))
		      (clpcl-let ((x (token-regexp "->")))
				 (declare (ignore x))
				 (<not-support>)
				 )
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
		       (setf (<grammar-def>-start g)
			     (parser-name (car parsers)))
		       (setf (<grammar-def>-parsers g) parsers)
		       g
		       ))
    )
   parser
   )
  )

(defun parser-name (p)
  (match p
    ((<grammar> name)
     (<ident>-name name))
    ((<fragment> name)
     (<ident>-name name))
    ))

(defun antlr4-file-to-parser (path)
  (let ((str (uiop:read-file-string path)))
    (antlr4-grammar-to-parser str)))

(defun antlr4-grammar-to-parser (str)
  (let*((parser (antlr4))
	(rs (clpcl-parse parser str))
	)
    (match rs
      ((success :value r)
       (build-parser r)))))

(defun antlr4-file-parse (path text)
  (let ((p (antlr4-file-to-parser path)))
    (if p
	(clpcl-parse (eval p) text))))

(defun antlr4-grammar-parse (str text)
  (let ((p (antlr4-grammar-to-parser str)))
    (if p
	(clpcl-parse (eval p) text))))


(defun test (&optional (file "test.txt") (text "abc") )
  (let ((path (asdf:system-relative-pathname :convertantlr4 file)))
    (antlr4-file-parse path text)
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
    ((<grammar-def> :start s :parsers ps)
     `(clpcl-def-parsers
       (,@(mapcar #'build-parser ps))
       ,(intern s)))
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
     (declare (ignore n))
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

