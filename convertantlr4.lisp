
(in-package :cl-user)
(defpackage convertantlr4
  (:use :cl :clpcl :optima)
  (:export
   :antlr4-str-to-parser
   :antlr4-str-parse
   :antlr4-str-to-grammar
   :antlr4-file-to-parser
   :antlr4-file-parse
   ))
(in-package :convertantlr4)

(defstruct (<grammar-def>
	     (:constructor <grammar-def>(name)))
  name
  start
  grammars
  )

(defstruct (<grammar>
	     (:constructor <grammar>(name rhs)))
  name
  rhs
  lexical
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
			   (<grammar-def> (<ident>-name s))))


    (fragment (clpcl-let ((nil (token-regexp "fragment"))
			  (x ident)
			  (y (token-regexp ":"))
			  (o orseq)
			  (z (token-regexp ";"))
			  )
			 (declare (ignore y z))
			 (<fragment> (<ident>-name x) o)))

    (grammar (clpcl-let ((x ident)
			 (y (token-regexp ":"))
			 (o orseq)
			 (z (token-regexp ";"))
			 )
			(declare (ignore y z))
			(<grammar> (<ident>-name x) o)))

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
			(grammars (clpcl-many
				  (clpcl-or 
				   fragment
				   grammar
				   ))))
		       (setf (<grammar-def>-start g)
			     (parser-name (car grammars)))
		       (setf (<grammar-def>-grammars g) grammars)
		       g
		       ))
    )
   parser
   )
  )

(defun parser-name (p)
  (match p
    ((<grammar> name)
     name)
    ((<fragment> name)
     name)
    )
  )

(defun antlr4-file-to-parser (path)
  (let ((str (uiop:read-file-string path)))
    (antlr4-str-to-parser str)))

(defun antlr4-str-to-grammar (str)
  (let*((parser (antlr4))
	(rs (clpcl-parse parser str))
	(table (make-hash-table :test #'equal))
	)
    (match rs
      ((success :value g)
       (build-table table g)
       (update-grammar-lexical table g)
       g))))

(defun antlr4-str-to-parser (str)
  (build-parser (antlr4-str-to-grammar str)))

(defun antlr4-file-parse (path text)
  (let ((p (antlr4-file-to-parser path)))
    (if p
	(clpcl-parse (eval p) text))))

(defun antlr4-str-parse (str text)
  (let ((p (antlr4-str-to-parser str)))
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

(defun split-quote (lit)
  (subseq lit 1 (- (length lit) 1)))


(defun build-table (table g)

  (labels ((helper (g)
	     (match g
	       ((<grammar-def> :start s :grammars ps)
		(declare (ignore s))
		(mapcar #'helper ps))
	       ((<grammar> :name x)
		(setf (gethash x table) g))
	       ((<fragment> :name x)
		(setf (gethash x table) g))
	       )
	     ))
    (helper g)
    )
  )

(defun update-grammar-lexical (table g)

  (flet ((update-grammar (g)
	   (traverse-grammar
	    g
	    (lambda (xx)
	      (match xx
		((<ident> :name n)
		 (let ((x (gethash n table)))
		   (match x
		     ((<fragment>)
		      (setf (<grammar>-lexical g) t))
		     )
		   )
		 ))))))

    (traverse-grammar
     g
     (lambda (x)
       (match x
	 ((<grammar>)
	  (update-grammar x))
	 )
       )
     )
    )
  )

(defun build-parser (g)

  ;;(format t "build-parser: ~S~%" g)
  
  (match g
    ((<grammar-def> :start s :grammars gs)
     `(clpcl-def-parsers
       (,@(build-parsers gs))
       ,(intern s)))
    ((<grammar> :name x :rhs xs)
     (list (intern x)
	   (build-parser xs)))
    ((<fragment> :name x :rhs xs)
     (list (intern x)
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
     `(token-regexp ,(split-quote literal)))
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

(defun traverse-grammar(g func)

  (match g
    ((or (<grammar-def> :grammars xs)
	 (<or>          :parsers xs)
	 (<seq>         :parsers xs)
	 )
     (funcall func g)
     (loop for x in xs
	do (traverse-grammar x func)
	  ))

    ((or (<grammar> :rhs x)
	 (<fragment> :rhs x)
	 (<factor>   :body x))
     (funcall func g)
     (traverse-grammar x func))

    ((or (<literal>)
 	 (<char-class>)
 	 (<ident>))
      (funcall func g))

    )
  )


	  
