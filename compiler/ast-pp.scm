; file: "ast-pp.scm"

; Copyright (C) 1998-1999 Universite de Montreal, All Rights Reserved.

; AST pretty printer

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Printing AST in Erlang

(define (lines+str lines str)
  (if (null? lines)
      (list str)
      (let ((hd (car lines))
	    (tl (cdr lines)))
	(if (null? tl)
	    (list (string-append hd str))
	    (cons hd
		  (lines+str tl str))))))

(define (lines+lines lines1 lines2)
  (if (null? lines1)
      lines2
      (if (null? lines2)
	  lines1
	  (if (null? (cdr lines1))
	      (cons (string-append (car lines1) (car lines2))
		    (map (lambda (str)
			   (string-append 
			    (make-string (string-length (car lines1))
					 #\space)
			    str))
			 (cdr lines2)))
	      (cons (car lines1)
		    (lines+lines (cdr lines1) lines2))))))

(define (str+lines str lines)
  (cons (string-append str (car lines)) (cdr lines)))

(define (lines-add-lines lines1 lines2)
  (if (null? lines1)
      lines2
      (cons (car lines1)
	    (lines-add-lines (cdr lines1) lines2))))

(define (lines-length lines)
  (if (null? lines)
      0
      (apply max (map string-length lines))))

(define (lines-indent lines n)
  (if (> n 0)
      (map (lambda (str) (string-append (make-string n #\space) str))
	   lines)
      (map (lambda (str) (substring str (- n) (string-length str)))
	   lines)))


;; Way 1: a,b,c,d
;; Way 2: a,
;;        b,
;;        c,
;;        d
(define (astlist->lines-glue astslines separator glue)
  (if (null? astslines)
      '()
      (let loop ((astlines (cdr astslines))
		 (lines (car astslines)))
	(if (null? astlines)
	    lines
	    (loop (cdr astlines)
		  (glue (lines+str lines separator)
			(car astlines)))))))

(define (astlist->lines astlist separator maxlen decide?)
  (let ((astslines (map (lambda (ast)
			  (ast->lines ast maxlen decide?))
			astlist)))
    (let ((first (astlist->lines-glue astslines separator lines+lines)))
      (if (and (<= (lines-length first) maxlen)
	       (= 1 (length first)))
	  first
	  (astlist->lines-glue astslines separator lines-add-lines)))))


(define (astlist->lines-column astlist separator maxlen decide?)
  (let ((astslines (map (lambda (ast)
			  (ast->lines ast maxlen decide?))
			astlist)))
    (astlist->lines-glue astslines separator lines-add-lines)))


; returns a string representing i in base 16 over 4 chars.
;old code:
;(define (integer->string4 i)
;  (apply 
;   string
;   (reverse
;    (let loop ((i (modulo i 65536)) (n 1))
;      (let* ((digit (modulo i 16))
;	     (c (integer->char (if (< digit 10)
;				   (+ (char->integer #\0) digit)
;				   (+ (char->integer #\a) (- digit 10))))))
;	(if (= n 4)
;	    (list c)
;	    (cons c (loop (quotient i 16) (+ n 1)))))))))

(define (integer->string4 n)
  (let loop ((n n) (i 4) (lst '()))
    (if (= i 0)
        (list->string lst)
        (let ((c (string-ref "0123456789abcdef" (modulo n 16))))
          (loop (quotient n 16) (- i 1) (cons c lst))))))

; convert a term to an erlang representation
(define (term->string term)
  (cond
   ((number? term) (number->string term))
   ((symbol? term)
    (let* ((str (symbol->string term))
	   (strlst (string->list str)))
      (if (and (char-lower-case? (car strlst))
	       (null? (filter (lambda (chr)
				(not (or (char-alphabetic? chr)
					 (char-numeric? chr)
					 ;; Not exactly, but nice for output
					 (memq chr '(#\@ #\/ #\_ #\-)))))
			      (cdr strlst))))
	  str
	  (string-append "'" str "'"))))
   ((char? term) 
    (case term
      ((#\8) "$\\b");************wrong!
      ((#\9) "$\\t");************wrong!
      ((#\10) "$\\n")
      ((#\11) "$\\v")
      ((#\12) "$\\f")
      ((#\13) "$\\r")
      ((#\27) "$\\e")
      ((#\32) "$\\s")
      ((#\34) "$\\\"")
      ((#\39) "$\\'")
      ((#\92) "$\\\\")
      ((#\127) "$\\d")
      (else (if (and (char>? #\32) (char<? #\127))
		(string #\$ term)
		(string-append "$\\u"
			       (integer->string4 (char->integer term)))))))
   ((erl-str? term)
    (if (null? term)
	"[]" ;; I prefer this representation...
	(string-append "\"" (list->string term) "\"")))
   ((list? term)
    (if (null? term)
	"[]"
	(if (null? (cdr term))
	    (string-append "[" (term->string (car term)) "]")
	    (string-append "["
			   (term->string (car term))
			   (apply string-append
				  (map (lambda (elt)
					 (string-append ","
							(term->string elt)))
				       (cdr term)))
			   "]"))))
   ((pair? term) (string-append "["
				(term->string (car term))
				"|"
				(term->string (cdr term))
				"]"))
   ((vector? term)
    (let ((elts (cdr (vector->list term))))
      (if (null? elts) 
	  "{}"
	  (if (null? (cdr elts))
	      (string-append "{" (term->string (car elts)) "}")
	      (string-append "{" 
			     (term->string (car elts))
			     (apply string-append
				    (map (lambda (elt)
					   (string-append ","
							  (term->string elt)))
					 (cdr elts)))
			     "}")))))
   (else (internal-error!!! "ast-pp:(term->string " term ")"))))

(define (sequence->lines prefix astlist suffix separator maxlen decide?)
  (let ((res1 (sequence->lines-flat prefix astlist suffix 
				    separator maxlen #f)))
    (if decide?
	(let ((res2 (sequence->lines-column prefix astlist suffix
					    separator maxlen #f)))
	  (if (or (> (lines-length res1) maxlen)
		  (> (length res1) (length res2)))
	      (sequence->lines-column prefix astlist suffix
				      separator maxlen #t)
	      (sequence->lines-flat prefix astlist suffix
				    separator maxlen #t)))
	res1)))
	    

(define (sequence->lines-flat prefix astlist suffix separator maxlen decide?)
  (let ((plen (lines-length prefix))
	(slen (lines-length suffix)))
    (let ((res1
	   (lines+lines
	    prefix
	    (lines+lines (astlist->lines astlist 
					 separator
					 (- maxlen plen slen)
					 #f) ; no decisions taken here
			 suffix))))
      (if decide?
	  (let ((res2
		 (sequence->lines-column prefix
					 astlist
					 suffix
					 separator
					 maxlen
					 #f))) ; no decisions here
	    (if (or (> (lines-length res1) maxlen)
		    (> (length res1) (length res2)))
		res2
		res1))
	  res1))))

(define (sequence->lines-column prefix astlist suffix separator maxlen decide?)
  (let ((plen (lines-length prefix))
	(slen (lines-length suffix)))
    (lines-add-lines
     prefix
     (lines-add-lines
      (lines-indent (astlist->lines-column astlist
					   separator
					   (- maxlen 2)
					   decide?)
		    2)
      suffix))))

(define (trueguards? astlist)
  (if (null? astlist)
      #t
      (let ((g (car astlist)))
	(and (and (eq? (node-type g) 'const)
		  (eq? (node-val1 g) 'true))
	     (trueguards? (cdr astlist))))))

(define (transform-apply ast)
  (let ((func (node-val1 ast)))
    (and (eq? (node-type func) 'const)
	 (let ((fname (node-val1 func))
	       (args (node-val2 ast)))
	   (or (and (eq? fname 'erl-cons)
		    (newast-cons (car args) (cadr args)))
	       (and (eq? fname 'erl-tuple)
		    (newast-tuple (node-val2 ast))))))))

;; Returns a list of ast if ast is a list
;; false otherwise
(define (ast->list ast)
  (or (and (eq? (node-type ast) 'const)
	   (eq? (node-val1 ast) '())
	   '())
      (and (eq? (node-type ast) 'cons)
	   (let ((tail (ast->list (node-val2 ast))))
	     (and tail
		  (cons (node-val1 ast) tail))))
      (and (eq? (node-type ast) 'apply)
	   (let ((func (node-val1 ast))
		 (args (node-val2 ast)))
	     (and (eq? (node-type func) 'const)
		  (eq? (node-val1 func) 'erl-cons)
		  (let ((tail (ast->list (cadr args))))
		    (and tail
			 (cons (car args) tail))))))))

;; Returns a list of chars if ast is a string term
;; false otherwise
(define (ast->string ast)
  (or (and (eq? (node-type ast) 'const)
	   (eq? (node-val1 ast) '())
	   '())
      (and (eq? (node-type ast) 'cons)
	   (let ((head (node-val1 ast)))
	     (and (eq? (node-type head) 'const)
		  (char? (node-val1 head))
		  (let ((tail (ast->string (node-val2 ast))))
		    (and tail
			 (cons (node-val1 head) tail))))))
      (and (eq? (node-type ast) 'apply)
	   (let ((func (node-val1 ast))
		 (args (node-val2 ast)))
	     (and (eq? (node-type func) 'const)
		  (eq? (node-val1 func) 'erl-cons)
		  (let ((head (car args)))
		    (and (eq? (node-type head) 'const)
			 (char? (node-val1 head))
			 (let ((tail (ast->string (cadr args))))
			   (and tail
				(cons (node-val1 head) tail))))))))))


(define (ast->lines ast maxlen decide?)
  (case (node-type ast)
    ((const) (list (term->string (node-val1 ast))))
    ((var)
     (let* ((str (symbol->string (node-val1 ast)))
	    (chrlst (string->list str))
	    (chr (car chrlst)))
       (if (or (char-upper-case? chr)
	       (char=? chr #\_))
	   (list str)
	   (list (list->string (cons #\X chrlst))))))
    ((universal) (list "_"))
    ((error) (list "<*ERROR*>"))
    ((tuple)
     (sequence->lines (list "{") (node-val1 ast) 
		      (list "}") "," maxlen decide?))
    ((cons)
     (let ((lst (ast->list ast)))
       (if lst
	   (let ((chrs (ast->string ast)))
	     (if chrs
		 (list (string-append "\"" (list->string chrs) "\""))
		 (sequence->lines (list "[") lst 
				  (list "]") "," maxlen decide?)))
	   (sequence->lines (list "[") (list (node-val1 ast) (node-val2 ast))
			    (list "]") "|" maxlen decide?))))
    ((match)
     (sequence->lines (list "") (list (node-val1 ast) (node-val2 ast))
		      (list "") "=" maxlen decide?))
    ((compare binop)
     (sequence->lines (list "") 
		      (list (node-val2 ast) (node-val3 ast))
		      (list "") 
		      (symbol->string (node-val1 ast))
		      maxlen
		      decide?))
    ((apply)
     (let ((newast (transform-apply ast)))
       (if newast
	   (ast->lines newast maxlen decide?)
	   (sequence->lines (sequence->lines (list "")
					     (list (node-val1 ast))
					     (list "(")
					     ""
					     maxlen
					     decide?)
			    (node-val2 ast)
			    (list ")")
			    ","
			    maxlen
			    decide?))))
    ((block)
     (let* ((tag (node-val1 ast))
	    (ifcond? (memq tag '(if cond)))
	    (astlist (node-val2 ast)))
       (if (and (eq? tag 'begin)
		(null? (cdr astlist)))
	   (ast->lines (car astlist) maxlen decide?)
	   (let ((prefix (list (string-append (symbol->string (node-val1 ast))
					      " "))))
	     ((if ifcond? sequence->lines-column sequence->lines)
	      prefix
	      (node-val2 ast)
	      (list (if ifcond? "end" " end"))
	      (if ifcond? ";" ",")
	      maxlen
	      decide?)))))
    ((case fcase try)
     (let* ((ntype (node-type ast))
	    (prefix (list (string-append (symbol->string 
					  (if (eq? ntype 'fcase) 'case ntype))
					 " "))))
       (sequence->lines-column
	(sequence->lines prefix 
			 (list (node-val1 ast)) 
			 (list (if (eq? ntype 'try) " catch" " of"))
			 ""
			 maxlen
			 decide?)
	(node-val2 ast)
	(list "end")
	";"
	maxlen
	decide?)))
    ((receive)
     (sequence->lines-column
      (list "receive ")
      (node-val1 ast)
      (sequence->lines (list "after ")
		       (list (node-val2 ast))
		       (sequence->lines (list " -> ")
					(list (node-val3 ast))
					(list " end")
					""
					maxlen
					decide?)
		       ""
		       maxlen
		       decide?)
      ";"
      maxlen
      decide?))
    ((namedfunclause)
     (sequence->lines (list (symbol->string (node-val1 ast)))
		      (list (node-val2 ast))
		      (list "")
		      ""
		      maxlen
		      decide?))
    ((funclause)
     (let* ((guards (node-val2 ast))
	    (noguards? (trueguards? guards)))
       (sequence->lines
	(sequence->lines 
	 (sequence->lines (list "(")
			  (node-val1 ast)
			  (list (if noguards? ")" ") when "))
			  ","
			  maxlen
			  decide?)
	 (if noguards? '() guards)
	 (list " -> ")
	 ","
	 maxlen
	 decide?)
	(list (node-val3 ast))
	(list "")
	""
	maxlen
	decide?)))
    ((ifclause)
     (sequence->lines
      (sequence->lines (list "")
		       (node-val1 ast)
		       (list " -> ")
		       ","
		       maxlen
		       decide?)
      (list (node-val2 ast))
      (list "")
      ""
      maxlen
      decide?))
    ((cdclause)
     (sequence->lines
      (sequence->lines (list "")
		      (list (node-val1 ast))
		      (list " -> ")
		      ""
		      maxlen)
      (list (node-val2 ast))
      (list "")
      ""
      maxlen
      decide?))
    ((crtclause)
     (let* ((guards (node-val2 ast))
	    (noguards? (trueguards? guards)))
       (sequence->lines
	(sequence->lines
	 (sequence->lines (list "")
			  (list (node-val1 ast))
			  (list (if noguards? "" " when "))
			  ""
			  maxlen
			  decide?)
	 (if noguards? '() guards)
	 (list " -> ")
	 ","
	 maxlen
	 decide?)
	(list (node-val3 ast))
	(list "")
	""
	maxlen
	decide?)))
    ((topfundef)
     (sequence->lines (list "")
		      (node-val1 ast)
		      (list ".")
		      ";"
		      maxlen
		      decide?))
    ((funref)
     (let ((sa (node-val1 ast)))
       (sequence->lines (list "fun ")
			(list (car sa) (cdr sa))
			(list "")
			"/"
			maxlen
			decide?)))
    ((fundef)
     (sequence->lines (list "fun ")
		      (node-val1 ast)
		      (list " end")
		      ";"
		      maxlen
		      decide?))
    ((catch catch2)
     (sequence->lines (list (string-append (symbol->string (node-type ast))
					   " "))
		      (list (node-val1 ast))
		      (list "")
		      ""
		      maxlen
		      decide?))
;     ((fundefarit)
;      (sequence->lines (list (string-append "fun/" 
; 					   (number->string (node-val1 ast))
; 					   " "))
; 		      (list (node-val3 ast))
; 		      (list " end")
; 		      ""
; 		      maxlen
; 		      decide?))
;     ((namedfundef)
;      (sequence->lines (list (string-append (symbol->string (node-val1 ast))
; 					   " := "))
; 		      (list (node-val2 ast))
; 		      (list "")
; 		      ""
; 		      maxlen
; 		      decide?))
    ((fundefarit)
     (sequence->lines (list (string-append "fun/" 
					   (number->string (node-val1 ast))
					   " "))
		      (list (node-val3 ast))
		      (list " end")
		      ""
		      maxlen
		      decide?))
    ((namedfundef)
     (let ((fundefarit (node-val2 ast)))
       (sequence->lines
	(sequence->lines
	 (list (string-append
		(symbol->string (fetch-fname (node-val1 ast))) "("))
	 (genvarlist (node-val2 fundefarit) (node-val1 fundefarit) 1)
	 (list ") -> ")
	 ","
	 maxlen
	 decide?)
	(list (node-val3 fundefarit))
	(list ".")
	""
	maxlen
	decide?)))
     ((prog) ; top-level
     (let loop ((topfdefs (node-val1 ast)))
       (if (null? topfdefs)
	   '()
	   (let ((topfdef (car topfdefs)))
	     (lines-add-lines
	      (ast->lines topfdef maxlen decide?)
	      (cons "" (loop (cdr topfdefs))))))))
    (else (internal-error!!! "ast-pp:ast->lines " (node-type ast)))))


;; Output an AST in Erlang
(define (pp-ast ast)
  (let ((lines (ast->lines ast 80 #t)))
    (for-each (lambda (line) (display line) (newline))
	      lines)))

