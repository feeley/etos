; file: "ast-build.scm"

; Copyright (C) 1998-1999 Universite de Montreal, All Rights Reserved.

; AST node contruction.

(define (node? x)
  (and (vector? x)
       (> (vector-length x) 0)
       (eq? (vector-ref x 0) 'node)))

(define (node-type node)     (vector-ref node 1))
(define (node-location node) (vector-ref node 2))
(define (node-input node)    (vector-ref node 3))
(define (node-output node)   (vector-ref node 4))
(define (node-fvb node)      (vector-ref node 5))
(define (node-fva node)      (vector-ref node 6))
(define (node-val1 node)     (vector-ref node 7))
(define (node-val2 node)     (vector-ref node 8))
(define (node-val3 node)     (vector-ref node 9))

(define (node-type-set! node val)     (vector-set! node 1 val))
(define (node-location-set! node val) (vector-set! node 2 val))
(define (node-input-set! node val)    (vector-set! node 3 val))
(define (node-output-set! node val)   (vector-set! node 4 val))
(define (node-fvb-set! node val)      (vector-set! node 5 val))
(define (node-fva-set! node val)      (vector-set! node 6 val))
(define (node-val1-set! node val)     (vector-set! node 7 val))
(define (node-val2-set! node val)     (vector-set! node 8 val))
(define (node-val3-set! node val)     (vector-set! node 9 val))

(define node-copy copy-vector)

(define (node-newbnd node)
  (let ((input-s (map node-val1 (node-input node))))
    (filter (lambda (v) (not (memq (node-val1 v) input-s)))
	    (node-output node))))

;; Node creation (tag location val1 val2 ...)
(define (make-node tag location . values)
  (apply vector (append (list 'node tag location #f #f #f #f)
			values)))

(define (new-node tag . values)
  (apply vector (append (list 'node tag #f #f #f #f #f)
			values)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The AST structure

;; Every node contains location information for the debugger/error handler
;; A star or plus or % denotes a 'basic' nodes, every other can be 
;; reduced to 'basic' ones
;; A plus denotes 'basic container' nodes, needed for 'basic' 
;; nodes, but with no semantic attached.
;; A % denotes 'pattern basic' nodes, only present in patterns, never
;; in expressions.
;;*#(const        k:object)
;;*#(var          vname:symbol)
;;*#(universal)
;;%#(tuple        elts:astlist)
;;%#(cons         hd:ast          tl:ast)
;; #(catch        expr:ast)
;;%#(match        expr1:ast       expr2:ast)
;; #(unop         op:symbol       expr:ast)
;; #(binop        op:symbol       expr1:ast        expr2:ast)
;; #(compare      op:symbol       expr1:ast        expr2:ast)
;;*#(apply        expr:ast        args:astlist)
;; #(block        tag:symbol      body:astlist)
;; #(case         expr:ast        clauses:astlist)
;;*#(fcase        expr:ast        clauses:astlist) ; exhaustive case
;; #(receive      clauses:astlist aexpr:ast        abody:ast)
;; #(try          expr:ast        clauses:astlist)
;; #(funref       ref:(symbol . arity))
;; #(fundef       fclauses:astlist)
;;*#(fundefarit   arity:integer   prefix:string    expr:ast)
;; #(topfundef    nfclauses:astlist)
;;*#(namedfclause name:symbol     fclause:ast)
;; #(funclause    pats:astlist    guard:astlist    body:ast)
;; #(cdclause     expr:ast        body:ast)
;; #(ifclause     guard:astlist   body:ast)
;;+#(crtclause    pat:ast         guard:astlist    body:ast)
;;*#(error)       ; to be ignored until compilation to scheme...
;;*#(catch2       body:ast)
;;*#(namedfundef  name/a:symbol   fundefarit:ast)
;; #(lc           expr:ast        qualifiers:astlist)
;; #(generator    pat:ast         expr:ast)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AST construction

;; Shortcuts for building new nodes
(define (newast-const k) (new-node 'const k))
(define (newast-var v) (new-node 'var v))
(define (newast-universal) (new-node 'universal))
(define (newast-tuple lst) (new-node 'tuple lst))
(define (newast-cons hd tl) (new-node 'cons hd tl))
(define (newast-apply f args) (new-node 'apply f args))
(define (newast-crtclause pat guards body)
  (new-node 'crtclause pat guards body))
(define (newast-compare op a1 a2) (new-node 'compare op a1 a2)) 
(define (newast-fcase expr cls) (new-node 'fcase expr cls))
(define (newast-fundefarit arity prefix expr)
  (new-node 'fundefarit arity prefix expr))
(define (newast-namedfundef name fundef)
  (new-node 'namedfundef name fundef))
(define (newast-match pat expr) (new-node 'match pat expr))
(define (newast-error) (new-node 'error))
(define (newast-topfundef nfclauses) (new-node 'topfundef nfclauses))
(define (newast-namedfclause name fclause)
  (new-node 'namedfclause name fclause))
(define (newast-funclause pats guards body)
  (new-node 'funclause pats guards body))
(define (newast-block tag astlist)
  (new-node 'block tag astlist))
(define (newast-lc expr qualifiers)
  (new-node 'lc expr qualifiers))
(define (newast-generator pat expr)
  (new-node 'generator pat expr))

(define (ast-genvar)
  (newast-var (erl-atom<-string (symbol->string (gensymbol 'v)))))

;; Constants

(define (tok->constnode tok)
  (make-node
   'const
   (token-location tok)
   (let ((kind (token-kind tok)))
     (cond ((or (= kind QuotedAtomLiteral-tok)
                (= kind NotQuotedAtomLiteral-tok))
            (erl-atom<-string (token-value tok)))
           ((= kind FloatLiteral-tok)
            (erl-float<-real (token-value tok)))
           ((= kind CharLiteral-tok)
            (erl-char<-char (token-value tok)))
           ((or (= kind UnsignedDecimalLiteral-tok)
                (= kind NotUnsignedDecimalIntegerLiteral-tok))
            (erl-int<-exact-integer (token-value tok)))
           (else
            (internal-error!!! "tok->constnode"))))))

(define (make-const-cons lbracket hds tl rbracket)
  (make-node
   'const
   (generic-location-join lbracket rbracket)
   (let loop ((hds (node-val1 hds)))
     (let ((hd (node-val1 (car hds))))
       (if (null? (cdr hds))
           (cons hd (node-val1 tl))
           (cons hd (loop (cdr hds))))))))

(define (force-term-const ast)
  (case (node-type ast)
    ((const)
     ast)
    ((cons)
     (make-node
      'const
      (node-location ast)
      (erl-cons (node-val1 (force-term-const (node-val1 ast)))
                (node-val1 (force-term-const (node-val2 ast))))))
    (else
     (internal-error!!! "force-term-const"))))


;; Variables
(define (tok->varnode tok)
  (make-node 'var
	     (token-location tok)
	     (erl-atom<-string (token-value tok))))

;; Universal pattern
(define (make-universal tok)
  (make-node 'universal (token-location tok)))

;; Tuple
(define (make-tupleast lbrace elts rbrace)
  (make-node 'tuple (generic-location-join lbrace rbrace) elts))

;; Cons
(define (make-cons lbracket hds tl rbracket)
  (let loop ((hds hds) (first? #t))
    (let ((hd (car hds)))
      (if (null? (cdr hds))
	  (make-node 'cons
		     (generic-location-join (if first? lbracket hd) rbracket)
		     hd
		     tl)
	  (make-node 'cons
		     (generic-location-join hd rbracket)
		     hd
		     (loop (cdr hds) #f))))))

(define (stringtok->ast str_tok)
  (let ((str (token-value str_tok))
	(loc (token-location str_tok)))
    (if (string=? str "")
	(make-node 'const loc '())
	(let ((lst (string->list str)))
	  (make-node 'cons
		     loc
		     (newast-const (car lst))
		     (let loop ((lst (cdr lst)))
		       (if (null? lst)
			   (newast-const '())
			   (newast-cons (newast-const (car lst))
					(loop (cdr lst))))))))))

(define (stringnode-append str1 str2)
  (if (eq? (node-type str1) 'cons)
      (make-node 'cons
		 (node-location str1)
		 (node-val1 str1)
		 (stringnode-append (node-val2 str1) str2))
      str2))

;; Catch
(define (make-catch tok expr)
  (make-node 'catch (generic-location-join tok expr) expr))

;; Match
(define (make-match expr1 expr2)
  (make-node 'match 
	     (generic-location-join expr1 expr2)
	     expr1
	     expr2))

;; Binop
(define (make-binop op expr1 expr2)
  (make-node 'binop
	     (generic-location-join expr1 expr2)
	     op
	     expr1
	     expr2))

;; Unop
(define (make-unop op tok expr)
  (make-node 'unop
	     (generic-location-join tok expr)
	     op
	     expr))

;; Compare
(define (make-compare op expr1 expr2)
  (make-node 'compare
	     (generic-location-join expr1 expr2)
	     op
	     expr1
	     expr2))

;; Generator
(define (make-generator pat expr)
  (make-node 'generator
	     (generic-location-join pat expr)
	     pat
	     expr))
;; Apply
(define (make-apply expr args rparen)
  (make-node 'apply
	     (generic-location-join expr rparen)
	     (newast-const 'erl-fun-apply)
	     (cons expr
		   (cons (newast-const (length args))
			 args))))

(define (make-remoteapply mod fun args rparen)
  (make-node 'apply
	     (generic-location-join mod rparen)
	     (newast-const 'erl-remote-apply)
	     (cons (make-node 'apply
			      (generic-location-join mod rparen)
			      (newast-const 'erl-mfa->fname)
			      (list mod fun (newast-const (length args))))
		   args)))

(define (make-apply-symb symb args rparen)
  (make-node 'apply
	     (generic-location-join symb rparen)
	     (make-symbolarity-atom (cons symb
					  (newast-const (length args))))
	     args))

;; Send(transform to apply node...)
(define (make-send expr1 expr2)
  (make-node 'apply
	     (generic-location-join expr1 expr2)
	     (newast-const 'erl-send/2)
	     (list expr1 expr2)))

;; Block(begin, all_true, some_true, ...)
(define (make-block tag start body end)
  (make-node 'block
	     (generic-location-join start end)
	     tag
	     body))

(define (make-body lst)
  (make-node 'block
	     (generic-location-join-multi lst)
	     'begin
	     lst))

(define (make-prog fdefs)
  (new-node 'prog fdefs))

;; Case
(define (make-case start body clauses end)
  (make-node 'case
	     (generic-location-join start end)
	     body
	     clauses))

;; Receive
(define (make-receive start clauses aexpr abody end)
  (make-node 'receive
	     (generic-location-join start end)
	     clauses
	     aexpr
	     abody))

(define (make-try start body clauses end)
  (make-node 'try
             (generic-location-join start end)
             body
             clauses))

(define (symbol-arity->symbol symb arit)
  (string->symbol
   (string-append (symbol->string symb)
		  "/"
		  (number->string arit))))

(define (fetch-fname symb)
  (let loop ((lst (string->list (symbol->string symb)))
	     (revres '()))
    (if (null? lst)
	(string->symbol (list->string (reverse revres)))
	(let ((chr (car lst)))
	  (if (char=? chr #\/)
	      (loop '() revres)
	      (loop (cdr lst) (cons chr revres)))))))

;; sa= (cons func:atomic arity:integer)
(define (make-symbolarity-atom sa)
  (let ((symb (car sa)) (arit (cdr sa)))
    (let ((s (node-val1 symb)))
      (if (symbol? s)
	  (make-node 'const
		     (generic-location-join symb arit)
		     (symbol-arity->symbol s (node-val1 arit)))
	  (begin
            (parser-error-invalid-function-name symb)
            (newast-error))))))


(define (make-funref start sa)
  (make-node 'funref (generic-location-join start (cdr sa)) sa))

(define (make-fundef start fclauses end)
  (make-node 'fundef (generic-location-join start end) fclauses))

(define (make-topfundef nfclauses full-stop)
  (make-node 'topfundef
             (generic-location-join (car nfclauses) full-stop)
             nfclauses))

(define (make-namedfunclause name clause)
  (make-node 'namedfunclause
	     (generic-location-join name clause)
	     (node-val1 name)
	     clause))

(define (make-funclause lparen pats guards body)
  (make-node 'funclause
	     (generic-location-join lparen body)
	     pats
	     guards
	     body))

(define (make-cdclause expr body)
  (make-node 'cdclause
	     (generic-location-join expr body)
	     expr
	     body))

(define (make-ifclause guards body)
  (make-node 'ifclause
	     (generic-location-join (generic-location-join-multi guards) body)
	     guards
	     body))

(define (make-crtclause pat guards body)
  (make-node 'crtclause
	     (generic-location-join pat body)
	     pat
	     guards
	     body))

(define (make-catchall exitsymbol)
  (newast-crtclause (univn)
		    (list (truen))
		    (make-kcall 'erl-exit/1 exitsymbol)))

(define (make-kcall f . args)
  (newast-apply (newast-const f)
		(map (lambda (a) (newast-const a)) args)))



(define (make-record-idx rec_expr sharp rec_name rec_field)
  (let ((recdef (get-record-definition rec_name)))
    (if recdef
	(let loop ((idx 2) (lst (cdr recdef)))
	  (if (null? lst)
              (begin
                (parser-error-undefined-record-field rec_field)
                (newast-error))
	      (let ((match (car lst)) (field (node-val1 rec_field)))
		(if (eq? (car match) field)
		    (if rec_expr
			(make-node 'apply
				   (generic-location-join rec_expr rec_field)
				   (newast-const 'element/2)
				   (list (newast-const idx)
					 rec_expr))
			(make-node 'const
				   (generic-location-join sharp rec_field)
				   idx))
		    (loop (+ idx 1) (cdr lst))))))
        (begin
          (parser-error-undefined-record-type rec_name)
          (newast-error)))))


(define (make-record-pat sharp rec_name assoclist)
  (let ((recdef (get-record-definition rec_name)))
    (if recdef
	(let ((field_exprs (cdr recdef)))
	  (let loop ((lst (map car assoclist)))
	    (if (null? lst)
		(let ((assqlist (map (lambda (fp) (cons (node-val1 (car fp))
							(cdr fp)))
				     assoclist)))
		  (newast-tuple
		   (cons rec_name
			 (let loop ((lst field_exprs))
			   (if (null? lst)
			       '()
			       (let* ((fe (car lst))
				      (found (assq (car fe)
						   assqlist)))
				 (cons (if found
					   (cdr found)
					   (univn))
				       (loop (cdr lst)))))))))
		(let ((field (car lst)))
		  (if (assq (node-val1 field) field_exprs)
		      (loop (cdr lst))
                      (begin
                        (parser-error-undefined-record-field field)
                        (newast-error)))))))
	(begin
          (parser-error-undefined-record-type rec_name)
          (newast-error)))))


(define (make-record-upd rec_expr rec_name assoclist)
  (let ((recdef (get-record-definition rec_name)))
    (if recdef
	; first, check all field names
	(let ((field_exprs (cdr recdef)))
	  (let loop ((lst (map car assoclist)))
	    (if (null? lst)
		; next, build tuple
		(let ((assqlist (map (lambda (fe) (cons (node-val1 (car fe))
							(cdr fe)))
				     assoclist)))
		  (if rec_expr
		      (let ((pes
			     (cons
			      (cons rec_name rec_name)
			      (let loop ((lst field_exprs))
				(if (null? lst)
				    '()
				    (let* ((fe (car lst))
					   (found (assq (car fe) assqlist)))
				      (cons
				       (if found
					   (cons (univn)
						 (cdr found))
					   (let ((var (ast-genvar)))
					     (cons var var)))
				       (loop (cdr lst)))))))))
			(newast-fcase
			 rec_expr
			 (list
			  (newast-crtclause (newast-tuple (map car pes))
					    (list (truen))
					    (newast-tuple (map cdr pes)))
			  (make-catchall 'badarg))))
		      (newast-tuple
		       (cons rec_name
			     (let loop ((lst field_exprs))
			       (if (null? lst)
				   '()
				   (let* ((fe (car lst))
					  (found (assq (car fe) 
						       assqlist)))
				     (cons (if found
					       (cdr found)
					       (cdr fe))
					   (loop (cdr lst))))))))))
		(let ((field (car lst)))
		  (if (assq (node-val1 field) field_exprs)
		      (loop (cdr lst))
                      (begin
                        (parser-error-undefined-record-field field)
                        (newast-error)))))))
	(begin
          (parser-error-undefined-record-type rec_name)
          (newast-error)))))

;; Generate a list of new variables perfixed and numbered
(define (genvarlist-symbol prefix n start)
  (if (< n 1)
      '()
      (cons (string->symbol (string-append prefix
					   "_a"
					   (number->string start)))
	    (genvarlist-symbol prefix (- n 1) (+ start 1)))))

(define (genvarlist prefix n start)
  (map newast-var (genvarlist-symbol prefix n start)))

;; Some usefull AST constants

(define (truen) (newast-const 'true))
(define (falsen) (newast-const 'false))
(define (univn) (newast-universal))
