; file: "ast-analyze.scm"

; Copyright (C) 1998-1999 Universite de Montreal, All Rights Reserved.

; Perform various analysis on AST

(define (reduce-ast ast)
  (case (node-type ast)
    ((const var universal error) ast)
    ((funref) (make-node
	       'apply
	       (node-location ast)
	       (newast-const 'erl-function)
	       (let ((sa (node-val1 ast)))
		 (list (cdr sa)
		       (make-node 'var
				  (generic-location-join (car sa) (cdr sa))
				  (node-val1 (make-symbolarity-atom sa)))))))
    ((tuple) (make-node 'apply
			(node-location ast)
			(newast-const 'erl-tuple)
			(map reduce-ast (node-val1 ast))))
    ((cons) (make-node 'apply
		       (node-location ast)
		       (newast-const 'erl-cons)
		       (list (reduce-ast (node-val1 ast))
			     (reduce-ast (node-val2 ast)))))
    ((binop compare)
     (make-node
      'apply
      (node-location ast)
      (newast-const
       (string->symbol (string-append (symbol->string (node-val1 ast)) "/2")))
      (list (reduce-ast (node-val2 ast))
	    (reduce-ast (node-val3 ast)))))
    ((unop)
     (make-node
      'apply
      (node-location ast)
      (newast-const
       (string->symbol (string-append (symbol->string (node-val1 ast)) "/1")))
      (list (reduce-ast (node-val2 ast)))))
    ((apply)
     (make-node 'apply
		(node-location ast)
		(reduce-ast (node-val1 ast))
		(map reduce-ast (node-val2 ast))))
    ((match) (reduce-ast
	      (make-node 'fcase
			 (node-location ast)
			 (node-val2 ast)
			 (list (let ((var (ast-genvar)))
				 (make-node 'crtclause
					    (node-location ast)
					    (newast-match (node-copy var)
							  (node-val1 ast))
					    (list (truen))
					    (node-copy var)))
			       (make-catchall 'badmatch)))))
    ((fcase)
     (make-node 'fcase
		(node-location ast)
		(reduce-ast (node-val1 ast))
		(map reduce-ast (node-val2 ast))))
    ((case)
     (make-node 'fcase
		(node-location ast)
		(reduce-ast (node-val1 ast))
		(append (map reduce-ast (node-val2 ast))
			(list (make-catchall 'case_clause)))))
    ((catch2)
     (make-node 'catch2
		(node-location ast)
		(reduce-ast (node-val1 ast))))
    ((try catch)
     (reduce-ast
      (make-node
       'fcase
       (node-location ast)
       (let ((expr (node-val1 ast)))
	 (make-node 'catch2 (node-location expr) expr))
       (let ((var1 (ast-genvar))
	     (var2 (ast-genvar))
	     (var3 (ast-genvar)))
	 (cons
	  (newast-crtclause (newast-tuple (list (newast-const 'NORMAL)
						(node-copy var1)))
			    (list (truen))
			    (node-copy var1))
	  (let ((try? (eq? (node-type ast) 'try)))
	    (append
	     (if try? (node-val2 ast) '())
	     (list
	      (newast-crtclause
	       (newast-tuple (list (newast-const 'THROW) (node-copy var2)))
	       (list (truen))
	       (if try?
		   (newast-apply (newast-const 'erl-throw/1)
				 (list (node-copy var2)))
		   (node-copy var2)))
	      (if try?
		  (newast-crtclause
		   (newast-tuple (list (newast-const 'EXIT) (node-copy var3)))
		   (list (truen))
		   (newast-apply (newast-const 'erl-exit/1)
				 (list (node-copy var3))))
		  (newast-crtclause (node-copy var3)
				    (list (truen))
				    (node-copy var3)))))))))))
    ((prog) (make-prog (map reduce-ast (node-val1 ast))))
    ((topfundef)
     (reduce-ast (nfclauses->nfundef (node-val1 ast))))
    ((namedfundef) (make-node 'namedfundef
			      (node-location ast)
			      (node-val1 ast)
			      (reduce-ast (node-val2 ast))))
    ((fundef)
     (reduce-ast (fundef->expr (node-val1 ast) #t)))
    ((fundefarit) (make-node 'fundefarit
			     (node-location ast)
			     (node-val1 ast)
			     (node-val2 ast)
			     (reduce-ast (node-val3 ast))))
    ((crtclause) (check-crtclause
		  (make-node 'crtclause
			     (node-location ast)
			     (node-val1 ast)
			     (node-val2 ast) ; guards aren't reduced
			     (reduce-ast (node-val3 ast)))))
    ((lc) (reduce-ast (lc->fcase ast)))
    ((receive)
     (reduce-ast
      (let ((clauses (node-val1 ast))
	    (aexpr (node-val2 ast))
	    (abody (node-val3 ast)))
	(make-node
	 'fcase
	 (node-location ast)
	 (newast-apply (newast-const 'erl-receive_first)
		       (list aexpr))
	 (let ((cls_tail
		(append (map (lambda (cl)
			       (make-node
				'crtclause
				(node-location cl)
				(node-val1 cl)
				(node-val2 cl)
				(newast-block
				 'begin
				 (list (make-kcall 'erl-receive_accept)
				       (node-val3 cl)))))
			     clauses)
			(list (newast-crtclause
			       (univn)
			       (list (truen))
			       (newast-apply
				(newast-const 'erl-receive_next)
				'()))))))
	   (if (and (eq? (node-type aexpr) 'const)
		    (eq? (node-val1 aexpr) 'infinity))
	       cls_tail
	       (cons
		(newast-crtclause (newast-const '$timeout)
				  (list (truen))
				  abody)
		cls_tail)))))))
    ((block)
     (reduce-ast
      (case (node-val1 ast)
	((begin)
	 (let ((astlist (node-val2 ast)))
	   (if (= 1 (length astlist))
	       (car astlist)
	       (make-node
		'fcase
		(node-location ast)
		(car astlist)
		(list (newast-crtclause (univn)
					(list (truen))
					(make-node 'block
						   (generic-location-join-multi (cdr astlist))
						   'begin
						   (cdr astlist))))))))
	((if)
	 (make-node 'fcase
		    (node-location ast)
		    (newast-const 0)
		    (append (map (lambda (ifclause)
				   (make-node 'crtclause
					      (node-location ifclause)
					      (univn)
					      (node-val1 ifclause)
					      (node-val2 ifclause)))
				 (node-val2 ast))
			    (list (make-catchall 'if_clause)))))
       ((cond)
	(let* ((astlist (node-val2 ast))
	       (cdclause (car astlist))
	       (cdclauses (cdr astlist)))
	  (make-node
	   'fcase
	   (node-location ast)
	   (node-val1 cdclause)
	   (list (newast-crtclause (truen) (list (truen)) (node-val2 cdclause))
		 (newast-crtclause (falsen)
				   (list (truen))
				   (if (null? cdclauses)
				       (make-kcall 'erl-exit/1 'cond_clause)
				       (make-node 'block
						  (generic-location-join-multi cdclauses)
						  'cond
						  cdclauses)))
		 (make-catchall 'badbool)))))
       ((all_true some_true)
	(let ((astlist (node-val2 ast))
	      (truen (if (eq? (node-val1 ast) 'all_true) (truen) (falsen)))
	      (falsen (if (eq? (node-val1 ast) 'all_true) (falsen) (truen))))
	  (if (= 1 (length astlist))
	      (make-node 'fcase
			 (node-location ast)
			 (car astlist)
			 (list (newast-crtclause truen (list truen) truen)
			       (newast-crtclause falsen (list truen) falsen)
			       (make-catchall 'badbool)))
	      (make-node 
	       'fcase
 	       (node-location ast)
	       (car astlist)
	       (list (newast-crtclause truen
				       (list truen)
				       (make-node 'block
						  (generic-location-join-multi (cdr astlist))
						  (node-val1 ast)
						  (cdr astlist)))
		     (newast-crtclause falsen (list truen) falsen)
		     (make-catchall 'badbool))))))
       (else (internal-error!!! "Unknown block expression: "
				(node-type ast))))))
    (else (internal-error!!! "Unknown ast: " (node-type ast)))))

(define (lc->fcase lc)
  (let ((template (node-val1 lc))
	(quals (node-val2 lc)))
    (if (null? quals)
	(newast-cons template (newast-const '()))
	(let ((qual (car quals))
	      (rquals (cdr quals)))
	  (if (eq? (node-type qual) 'generator)
	      (let* ((lvar (ast-genvar))
		     (vprefix (symbol->string (gensymbol 'v)))
		     (vvar (car (genvarlist vprefix 1 1))))
		(newast-fcase
		 (node-val2 qual)
		 (list (newast-crtclause
			(node-copy lvar)
			(list (truen))
			(newast-apply
			 (newast-const (if (null? rquals)
					   'erl-map
					   'erl-flatmap))
			 (list
			  (newast-apply
			   (newast-const 'erl-function-lambda)
			   (list
			    (newast-fundefarit
			     1
			     vprefix
			     (newast-fcase
			      vvar
			      (list (newast-crtclause
				     (node-val1 qual)
				     (list (truen))
				     (if (null? rquals)
					 template
					 (lc->fcase (newast-lc template
							       rquals))))
				    (newast-crtclause (univn)
						      (list (truen))
						      (newast-const '())))))))
			  (node-copy lvar)))))))
	      (if (guard-and-terminates? qual)
		  (newast-fcase (newast-const 0)
				(list (newast-crtclause
				       (univn)
				       (list qual)
				       (lc->fcase (newast-lc template rquals)))
				      (newast-crtclause
				       (univn)
				       (list (truen))
				       (newast-const '()))))
		  (newast-fcase
		   qual
		   (list (newast-crtclause
			  (truen)
			  (list (truen))
			  (lc->fcase (newast-lc template rquals)))
			 (newast-crtclause (falsen)
					   (list (truen))
					   (newast-const '()))
			 (make-catchall 'badbool)))))))))


(define (fclause->crtclause fclause)
  (make-node 'crtclause
	     (node-location fclause)
	     (let ((args (node-val1 fclause)))
	       (if (= 1 (length args))
		   (car args)
		   (newast-tuple args)))
	     (node-val2 fclause)
	     (node-val3 fclause)))

(define (arity->argtuple prefix arity)
  (let ((varlist (genvarlist prefix arity 1)))
    (if (= arity 1)
	(car varlist)
	(newast-tuple varlist))))


;; Transform unnamed function definition to expression
(define (fundef->expr fclauses lambda?)
  (let* ((fclause1 (car fclauses))
	 (arity (length (node-val1 fclause1))))
    (let loop ((fclauses (cdr fclauses))
	       (rev-crtclauses (list (fclause->crtclause fclause1))))
      (if (null? fclauses)
	  (let ((loc (generic-location-join-multi fclauses))
		(prefix (symbol->string (gensymbol 'fun))))
	    (make-node
	     'fundefarit
	     loc
	     arity
	     prefix
	     (make-node 'fcase
			loc
			(arity->argtuple prefix arity)
			(reverse (cons (make-catchall (if lambda?
							  'lambda_clause
							  'function_clause))
				       rev-crtclauses)))))
	  (let ((fclause (car fclauses)))
	    (if (= arity (length (node-val1 fclause)))
		(loop (cdr fclauses)
		      (cons (fclause->crtclause fclause)
			    rev-crtclauses))
		(analyzer-error-mismatching-function-arity fclause)))))))

;; Transform clause-oriented function definitions
(define (nfclauses->nfundef nfclauses)
  (let* ((nfclause1 (car nfclauses))
	 (fclause1 (node-val2 nfclause1))
	 (name (node-val1 nfclause1))
	 (arity (length (node-val1 fclause1))))
    (let loop ((nfclauses (cdr nfclauses))
	       (rev-res (list fclause1)))
      (if (null? nfclauses)
	  (make-node 'namedfundef
		     (generic-location-join-multi nfclauses)
		     (symbol-arity->symbol name arity)
		     (fundef->expr (reverse rev-res) #f))
	  (let* ((nfclause (car nfclauses))
		 (fclause (node-val2 nfclause)))
	    (if (eq? name (node-val1 nfclause))
		(if (= arity (length (node-val1 fclause)))
		    (loop (cdr nfclauses)
			  (cons (node-val2 nfclause) rev-res))
		    (analyzer-error-mismatching-function-arity nfclause))
		(analyzer-error-mismatching-function-name nfclause)))))))

;; returns the first variable that appears at least two times
;; in list, return second occurence.
;; will return false if no dups.
(define (check-dupvars vars)
  (let loop ((lst vars) (once '()))
    (if (null? lst)
	#f
	(let* ((v (car lst))
	       (s (node-val1 v)))
	  (if (memq s once)
	      v
	      (loop (cdr lst) (cons s once)))))))

;; Variables in all vlists, removing dups
(define (union-vars vlsts)
  (if (null? vlsts)
      '()
      (if (null? (cdr vlsts))
	  (car vlsts)
	  (let ((vlst1 (car vlsts))
		(vlst2 (cadr vlsts)))
	    (union-vars (cons (append vlst1 (diff-vars vlst2 vlst1))
			      (cddr vlsts)))))))

;; Variables in vlst1 that are not in vlst2
(define (diff-vars vlst1 vlst2)
  (let ((vlst2-s (map node-val1 vlst2))) 
    (filter (lambda (v) (not (memq (node-val1 v) vlst2-s)))
	    vlst1)))

;; Variables that are in all vlists
(define (inter-vars vlsts)
  (if (null? vlsts)
      '()
      (let ((vlst1 (car vlsts))
	    (vlsts-s (map (lambda (vlst) (map node-val1 vlst)) (cdr vlsts))))
	(filter (lambda (v)
		  (null? (filter (lambda (vlst-s)
				   (not (memq (node-val1 v) vlst-s)))
				 vlsts-s)))
		vlst1))))


;; Gets all functions names defined in a program
(define (get-fundef-names progast)
  (map node-val1 (node-val1 progast)))

(define prog-fundef #f)
(define (prog-resolve-calls! prog)
  (set! prog-fundef (get-fundef-names prog))
  (for-each expr-resolve-calls! (map node-val2 (node-val1 prog)))
  (set! prog-fundef #f))

(define (fsymb->fa fsymb)
  ;********bug: what if fsymb is foo/bar/1 (in Erlang: 'foo/bar'(X)).
  (let loop ((lst (string->list (erl-atom->string fsymb)))
	     (rev-f '()))
    (and (pair? lst)
	 (let ((hd (car lst)))
	   (if (char=? hd #\/)
	       (erl-tuple (erl-atom<-string (list->string (reverse rev-f)))
			  (string->number (list->string (cdr lst))))
	       (loop (cdr lst) (cons hd rev-f)))))))

(define (fa->fsymb fa)
  (erl-atom<-string
   (string-append (erl-atom->string (erl-tuple-ref fa 1))
		  "/"
		  (number->string (erl-tuple-ref fa 2)))))

;; Resolve imported calls
;; Also computes fully named remote-calls
(define (expr-resolve-calls! expr)
  (case (node-type expr)
    ((const error var) #f)
    ((apply)
     (let ((func (node-val1 expr)) (args (node-val2 expr)))
       (expr-resolve-calls! func)
       (for-each expr-resolve-calls! args)
       (and
	(eq? (node-type func) 'const)
	(let ((fsymb (node-val1 func)))
	  (and
	   (symbol? fsymb)
	   (or (and (eq? fsymb 'erl-function)
		    (let ((funref (cadr args)))
		      (and (eq? (node-type funref) 'var)
			   (or (memq (node-val1 funref) prog-fundef)
			       (analyzer-error-undefined-function
				funref
				(node-val1 funref))))))
	       (and
		(eq? fsymb 'erl-remote-apply)
		(let ((fullname (car args)))
		  (and
		   (eq? (node-type fullname) 'apply)
		   (let ((mfafun (node-val1 fullname))
			 (mfaargs (node-val2 fullname)))
		     (and
		      (eq? (node-type mfafun) 'const)
		      (eq? (node-val1 mfafun) 'erl-mfa->fname)
		      (let ((amod (car mfaargs))
			    (afun (cadr mfaargs))
			    (aari (caddr mfaargs)))
			(and
			 (eq? (node-type amod) 'const)
			 (eq? (node-type afun) 'const)
			 (eq? (node-type aari) 'const)
			 (let ((mod (node-val1 amod))
			       (fun (node-val1 afun))
			       (ari (node-val1 aari)))
			   (and
			    (symbol? mod)
			    (symbol? fun)
			    (integer? ari)
			    (begin
			      (remote-calls-list-add! (list mod fun ari))
			      (node-val1-set! expr
					      (newast-const
					       (erl-mfa->fname mod fun ari)))
			      (node-val2-set! expr (cdr args))
			      #t))))))))))
	       (let ((str (erl-atom->string fsymb)))
		 (and (> (string-length str) 4)
		      (string=? (substring str 0 4) "erl-")))
	       (memq fsymb prog-fundef)
	       (let ((imp (get-import-decl (fsymb->fa fsymb))))
		 (and (erl-tuple? imp)
                      (let* ((mod (erl-tuple-ref imp 2))
                             (f/a (erl-tuple-ref imp 1))
                             (fun (erl-tuple-ref f/a 1))
                             (ari (erl-tuple-ref f/a 2)))
                        (begin
			  (remote-calls-list-add! (list mod fun ari))
			  (node-val1-set!
			   expr
			   (newast-const (erl-mfa->fname mod fun ari)))
			  #t))))
	       (and (memq fsymb bif-list)
		    (begin
		      (node-val1-set!
		       expr
		       (newast-const (symbol-append 'erl- fsymb)))
		      #t))
	       (analyzer-error-undefined-function func fsymb)))))))
    ((catch2) (expr-resolve-calls! (node-val1 expr)))
    ((fcase)
     (expr-resolve-calls! (node-val1 expr))
     (for-each expr-resolve-calls! (node-val2 expr)))
    ((crtclause) (expr-resolve-calls! (node-val3 expr)))
    ((fundefarit) (expr-resolve-calls! (node-val3 expr)))
    (else (internal-error!!! "expr-resolve-calls!"))))

;; Tells if a crtclause is a trap (always complete abruptly)
(define (crtclause-notrap? crtclause)
  (let ((bexpr (node-val3 crtclause)))
    (not (and (eq? (node-type bexpr) 'apply)
	      (let ((func (node-val1 bexpr)))
		(and (eq? (node-type func) 'const)
		     (let ((kfname (node-val1 func)))
		       (memq kfname '(erl-exit/1
				      erl-exit/2
				      erl-throw/1
				      erl-receive_next)))))))))


(define (prog-update-envs! prog)
  (for-each (lambda (e) (expr-update-envs! e '()))
	    (map node-val2 (node-val1 prog))))

;; Augment exprs with input/output environments
(define (expr-update-envs! expr input)
  (node-input-set! expr input)
  (case (node-type expr)
    ((const error) (node-output-set! expr input))
    ((var) (let ((varname (node-val1 expr)))
	     (if (and (not (memq #\/ (string->list (symbol->string varname))))
		      (not (memq varname (map node-val1 input))))
		 (analyzer-error-unbound-variable expr (node-val1 expr)))
	     (node-output-set! expr input)))
    ((cons) ;; only in guards, so input==output
     (expr-update-envs! (node-val1 expr) input)
     (expr-update-envs! (node-val2 expr) input)
     (node-output-set! expr input))
    ((tuple) ;; only in guards, so input==output
     (for-each (lambda (e) (expr-update-envs! e input)) (node-val1 expr))
     (node-output-set! expr input))
    ((compare) ;; only in guards, so input==output
     (expr-update-envs! (node-val2 expr) input)
     (expr-update-envs! (node-val3 expr) input)
     (node-output-set! expr input))
    ((apply)
     (let ((func (node-val1 expr)) (args (node-val2 expr)))
       (expr-update-envs! func input)
       (for-each (lambda (e) (expr-update-envs! e input)) args)
       (let* ((newbnds (append (node-newbnd func)
			       (apply append (map node-newbnd args))))
	      (dupbnd (check-dupvars newbnds)))
	 (if dupbnd
	     (analyzer-error-already-bound dupbnd))
	 (node-output-set! expr (append input newbnds)))))
    ((catch2) (let ((cexpr (node-val1 expr)))
		(expr-update-envs! cexpr input)
		(node-output-set! expr input))) ; same as input
    ((fcase)
     (let ((sexpr (node-val1 expr))
	   (crtclauses (node-val2 expr)))
       (expr-update-envs! sexpr input)
       (let ((sout (node-output sexpr)))
	 (for-each (lambda (e) (expr-update-envs! e sout)) crtclauses))
       (node-output-set!
	expr
	(inter-vars (map node-output (filter crtclause-notrap? crtclauses))))))
    ((crtclause) ;guards play no role here.
     (let ((pat (node-val1 expr))
	   (guards (node-val2 expr))
	   (body (node-val3 expr)))
       (node-input-set! pat input) ; don't go down in patterns
       (node-output-set! pat (union-vars (list input (pattern-vars pat))))
       (for-each (lambda (g) (expr-update-envs! g (node-output pat)))
		 guards)
       (expr-update-envs! body (node-output pat))
       (node-output-set! expr (node-output body))))
    ((fundefarit)
     (expr-update-envs! (node-val3 expr)
			(append input (genvarlist (node-val2 expr)
						  (node-val1 expr)
						  1)))
     (node-output-set! expr input))
    (else (internal-error!!! "expr-update-envs!: type=" (node-type expr)))))


(define (prog-update-fv! prog)
  (for-each (lambda (e) (expr-update-fv! e '()))
	    (map node-val2 (node-val1 prog))))

;; Calculates free variables, requires environments to be calculated first.
;; *** Patterns and guards aren't updated
(define (expr-update-fv! expr fva) ;;fva=free vars after evaluation
  (node-fva-set! expr fva)
  (case (node-type expr)
    ((const error) (node-fvb-set! expr fva))
    ((var) (node-fvb-set! expr (union-vars (list (list expr) fva))))
    ((apply)
     (let ((func (node-val1 expr)) (args (node-val2 expr)))
       (expr-update-fv! func fva)
       (for-each (lambda (e) (expr-update-fv! e fva)) args)
       (node-fvb-set! expr (union-vars (cons (node-fvb func)
					     (map node-fvb args))))))
    ((catch2)
     (let ((cexpr (node-val1 expr)))
       (expr-update-fv! cexpr fva)
       (node-fvb-set! expr (node-fvb cexpr))))
    ((fcase)
     (let* ((sexpr (node-val1 expr))
	    (crtclauses (node-val2 expr)))
       (for-each (lambda (e) (expr-update-fv! e fva)) crtclauses)
       (expr-update-fv!
	sexpr
	(union-vars
	; (map node-fvb crtclauses)))
	 (map node-fvb (filter crtclause-notrap? crtclauses))))
       (node-fvb-set! expr (node-fvb sexpr))
;       (display "fcase    :") (pp-ast expr)
;       (display "fva      :") (pp (map node-val1 (node-fva expr)))
;       (display "fvb      :") (pp (map node-val1 (node-fvb expr)))
;       (display "fva sexpr:") (pp (map node-val1 (node-fva sexpr)))
;       (newline)
       ))
    ((crtclause)
     (let ((pat (node-val1 expr))
	   (guards (node-val2 expr))
	   (body (node-val3 expr)))
       (expr-update-fv! body fva)
       (let* ((gvars (guards-vars guards))
	      (fvapat (union-vars (list gvars (node-fvb body)))))
	 (for-each
	  (lambda (varast)
	    (and (node-location varast)
		 (not (char=?
		       (string-ref (symbol->string (node-val1 varast)) 0)
		       #\_))
		 (analyzer-warning-unused-variable varast (node-val1 varast))))
	  (diff-vars (node-newbnd pat) fvapat))
	 (node-fvb-set! expr
			(diff-vars (union-vars (list (pattern-vars pat)
						     fvapat))
				   (node-newbnd pat)))
;	 (display "crtclause:") (pp-ast expr)
;	 (display "fvb:") (pp (map node-val1 (node-fvb expr)))
	 )))
    ((fundefarit)
     (let ((fexpr (node-val3 expr)))
       (expr-update-fv! fexpr '())
       (node-fvb-set! expr (node-fvb fexpr))))
    (else (internal-error!!! "expr-update-fv! type=" (node-type expr)))))

;; Copy of a pattern, removing location information to
;; prevent warnings about unused variables
(define (pat-copy pat)
  (case (node-type pat)
    ((var) (newast-var (node-val1 pat)))
    ((const universal error) (node-copy pat))
    ((tuple) (newast-tuple (map pat-copy (node-val1 pat))))
    ((cons) (newast-cons (pat-copy (node-val1 pat))
			 (pat-copy (node-val2 pat))))
    ((match) (newast-match (pat-copy (node-val1 pat))
			   (pat-copy (node-val2 pat))))
    (else (internal-error!!! "pat-copy type=" (node-type pat)))))

;; Returns a list of variables present in a pattern
;; may return dups!!!
(define (pattern-vars pat)
  (case (node-type pat)
    ((var) (list (node-copy pat)))
    ((const universal error) '())
    ((tuple) (apply append (map pattern-vars (node-val1 pat))))
    ((cons match) (append (pattern-vars (node-val1 pat))
			  (pattern-vars (node-val2 pat))))
    (else (internal-error!!! "pattern-vars type=" (node-type pat)))))

(define (guards-vars guards)
  (union-vars (map guard-vars guards)))

(define (guard-vars guard)
  (case (node-type guard)
    ((var) (list (node-copy guard)))
    ((cons) (union-vars (list (guard-vars (node-val1 guard))
			      (guard-vars (node-val2 guard)))))
    ((tuple) (union-vars (map guard-vars (node-val1 guard))))
    ((const error) '())
    ((compare) (union-vars (list (guard-vars (node-val2 guard))
				 (guard-vars (node-val3 guard)))))
    ((apply) (union-vars (cons (guard-vars (node-val1 guard))
			       (map guard-vars
				    (node-val2 guard)))))
    (else (internal-error!!! "guard-vars type=" (node-type guard)))))

;; Returns a list of all symbols occuring more than
;; once in a list
;; ex: (get-dups (a b a b c a) => (a b a)
(define (get-dups lst)
  (let loop ((lst lst) (once '()))
    (if (null? lst)
	'()
	(let ((s (car lst)))
	  (if (memq s once)
	      (cons s (loop (cdr lst) once))
	      (loop (cdr lst) (cons s once)))))))

;; replace a var varname with newvar in a pattern
;; returns #f if varname not present
;; sv=(varname . newvar)
(define (replace-var pat sv)
  (let ((varname (car sv))
	(newvar (cdr sv)))
    (case (node-type pat)
      ((const universal error) #f)
      ((var) (and (eq? (node-val1 pat) varname)
		  newvar))
      ((tuple)
       (let loop ((patlist (node-val1 pat)) (rev-res '()) (found? #f))
	 (if (null? patlist)
	     (if found?
		 (make-node 'tuple
			    (node-location pat)
			    (reverse rev-res))
		 #f)
	     (let ((pat (car patlist)))
	       (if found?
		   (loop (cdr patlist) (cons pat rev-res) #t)
		   (let ((replaced (replace-var pat sv)))
		     (if replaced
			 (loop (cdr patlist) (cons replaced rev-res) #t)
			 (loop (cdr patlist) (cons pat rev-res) #f))))))))
      ((cons match)
       (let* ((pat1 (node-val1 pat))
	      (replaced (replace-var pat1 sv)))
	 (if replaced
	     (make-node (node-type pat)
			(node-location pat)
			replaced
			(node-val2 pat))
	     (let* ((pat2 (node-val2 pat))
		    (replaced (replace-var pat2 sv)))
	       (if replaced
		   (make-node (node-type pat)
			      (node-location pat)
			      pat1
			      replaced)
		   #f)))))
      (else (internal-error!!!)))))

;; Check a crtclause to linearize patterns
;; also check guards(described below)
(define (check-crtclause clause)
  (let ((pat (node-val1 clause))
	(guards (map check-guard (node-val2 clause))))
    (let ((assqlist (map (lambda (var)
			   (cons var (ast-genvar)))
			 (get-dups (map node-val1 (pattern-vars pat))))))
      (make-node 'crtclause
		 (node-location clause)
		 (fold replace-var (cons pat assqlist))
		 (append (map (lambda (sv)
				(newast-compare '=:=
						(newast-var (car sv))
						(node-copy (cdr sv))))
			      assqlist)
			 guards)
		 (node-val3 clause)))))

;; Check if an expression is a guard and always terminates
;; Used for List Comprehension
(define (guard-and-terminates? expr)
  (let ((type (node-type expr)))
    (case type
      ((apply)
       (let ((func (node-val1 expr)) (args (node-val2 expr)))
	 (and (eq? (node-type func) 'const)
	      (memq (node-val1 func) recognizer-bif-list)
	      (pair? args)
	      (null? (cdr args))
	      (memq (node-type (car args)) '(var const)))))
      ((compare)
       (and (memq (node-type (node-val2 expr)) '(var const))
	    (memq (node-type (node-val3 expr)) '(var const))))
      ((const) (eq? (node-val1 expr) 'true))
      (else #f))))

;; Check a guard, returns the guard, updated as needed
;; Also transform record(E,R) expressions.
(define (check-guard guard)
  (let ((type (node-type guard)))
    (cond
     ((and (eq? type 'apply)
	   (let ((func (node-val1 guard)))
	     (and (eq? (node-type func) 'const)
		  (memq (node-val1 func) (cons 'record/2
					       recognizer-bif-list)))))
      (if (eq? (node-val1 (node-val1 guard)) 'record/2)
	  (let* ((args (node-val2 guard))
		 (arg2 (cadr args)))
	    (if (and (eq? (node-type arg2) 'const)
		     (symbol? (node-val1 arg2)))
                (if (get-record-definition arg2)
                    (check-guard
                     (make-node 'compare
                                (node-location guard)
                                '=:=
                                (make-node 'apply
                                           (node-location guard)
                                           (newast-const 'element/2)
                                           (list (newast-const 1)
                                                 (car args)))
                                arg2))
                    (analyzer-error-undefined-record-type arg2))
                (analyzer-error-invalid-record-type arg2)))
	  (make-node 'apply
		     (node-location guard)
		     (node-val1 guard)
		     (map check-guard-expr (node-val2 guard)))))
     ((eq? type 'compare)
      (make-node 'compare
		 (node-location guard)
		 (node-val1 guard)
		 (check-guard-expr (node-val2 guard))
		 (check-guard-expr (node-val3 guard))))
     ((and (eq? type 'const)
	   (eq? (node-val1 guard) 'true))
      guard)
     (else (analyzer-error-invalid-guard guard)))))

;; Check a guard expression 
(define (check-guard-expr expr)
  (let ((type (node-type expr)))
    (case type
      ((var const tuple cons) expr)
      ((binop)
       (make-node 'apply
		  (node-location expr)
		  (newast-const
		   (string->symbol
		    (string-append (symbol->string (node-val1 expr))
				   "/2")))
		  (list (check-guard-expr (node-val2 expr))
			(check-guard-expr (node-val3 expr)))))
      ((unop)
       (make-node 'apply
		  (node-location expr)
		  (newast-const
		   (string->symbol 
		    (string-append (symbol->string (node-val1 expr))
				   "/1")))
		  (list (check-guard-expr (node-val2 expr)))))
      ((apply)
       (let ((func (node-val1 expr)))
	 (if (and (eq? (node-type func) 'const)
		  (memq (node-val1 func) guard-bif-list))
	     (make-node 'apply
			(node-location expr)
			func
			(map check-guard-expr (node-val2 expr)))
	     (analyzer-error-invalid-guard-expression expr))))
      (else (analyzer-error-invalid-guard-expression expr)))))
