; file: "ast-comp.scm"

; Copyright (C) 1998-1999 Universite de Montreal, All Rights Reserved.

; AST compilation

(include "ast-build.scm")
(include "ast-analyze.scm")
(include "pm-comp.scm")
(include "scm-build.scm")

;; Compiles a side-effect free expression
;; returns #f if AST is not detected as a side-effect free expression
(define (compile-sexpr ast)
  (case (node-type ast)
    ((const) (let ((k (node-val1 ast)))
	       (if (or (symbol? k) (null? k) (pair? k) (vector? k))
		   (list 'quote k)
		   k)))
    ((var) (node-val1 ast))
    ((apply)
     (let ((func (node-val1 ast)) (args (node-val2 ast)))
       (and (eq? (node-type func) 'const)
	    (or (and (eq? (node-val1 func) 'erl-cons)
		     (let ((hd (compile-sexpr (car args)))
			   (tl (compile-sexpr (cadr args))))
		       (and hd tl (list 'erl-cons hd tl))))
		(and (eq? (node-val1 func) 'erl-tuple)
		     (let ((elts (map compile-sexpr args)))
		       (and (null? (filter (lambda (x) (not x)) elts))
			    (cons 'erl-tuple elts))))))))
    (else #f)))

;; Compiles a new bindings free expression
;; returns #f if AST is not detected as a new bindings free expression
;; Only fcases may introduce new bindings
(define (compile-bexpr ast)
  (case (node-type ast)
    ((const) (let ((k (node-val1 ast)))
	       (if (or (symbol? k) (null? k))
		   (list 'quote k)
		   k)))
    ((var) (node-val1 ast))
    ((apply)
     (let ((func (node-val1 ast)) (args (node-val2 ast)))
       (and (eq? (node-type func) 'const)
	    (let ((cargs (map compile-bexpr args)))
	      (and (null? (filter (lambda (x) (not x)) cargs))
		   (cons (node-val1 func) cargs))))))
    (else #f)))

;; List of fully named remote calls
(define remote-calls-list #f)
(define (remote-calls-list-init)
  (set! remote-calls-list '()))
(define (remote-calls-list-add! fname)
  (or (member fname remote-calls-list)
      (set! remote-calls-list (cons fname remote-calls-list))))

(define (compile-ast-to-scheme ast)
  (remote-calls-list-init)
  (add-topfundef! ast (gen-module_info/0))
  (add-topfundef! ast (gen-module_info/1))
  (let ((reduced (reduce-ast ast)))
    (prog-resolve-calls! reduced)
    (prog-update-envs! reduced)
    (prog-update-fv! reduced)
    (let ((scm (compile-ast reduced)))
      (cons reduced scm))))

(define (add-topfundef! ast topfundef)
  (node-val1-set! ast (cons topfundef (node-val1 ast))))

(define (gen-mi-compile)
  (list 
   (erl-tuple 'time (get-start-time))
   (erl-tuple 'options (get-compile-options))
   ))
  
(define (gen-module_info/0)
  (newast-topfundef
   (list
    (newast-namedfclause
     'module_info
     (newast-funclause
      '()
      (list (truen))
      (newast-const (erl-list (erl-tuple 'module (erl-atom<-string (get-module-name)))
			      (erl-tuple 'exports (get-export-list))
			      (erl-tuple 'imports (get-import-list))
			      (erl-tuple 'attributes (get-wild-list))
			      (erl-tuple 'compile (gen-mi-compile))
			      )))))))

(define (gen-module_info/1)
  (newast-topfundef
   (list
    (newast-namedfclause 'module_info
			 (newast-funclause (list (newast-const 'module))
					   (list (truen))
					   (newast-const (erl-atom<-string (get-module-name)))))
    (newast-namedfclause 'module_info
			 (newast-funclause (list (newast-const 'exports))
					   (list (truen))
					   (newast-const (get-export-list))))
    (newast-namedfclause 'module_info
			 (newast-funclause (list (newast-const 'imports))
					   (list (truen))
					   (newast-const (get-import-list))))
    (newast-namedfclause 'module_info
			 (newast-funclause (list (newast-const 'attributes))
					   (list (truen))
					   (newast-const (get-wild-list))))
    (newast-namedfclause 'module_info
			 (newast-funclause (list (newast-const 'compile))
					   (list (truen))
					   (newast-const (gen-mi-compile)))))))

(define (compile-ast ast)
  (compile-aux ast identity #f))


(##define-macro (report x)
;		`(time ,x))
		x)

(define currentfun #f)

;; Compiles an AST to scheme code,
;; given: a continuation kont(scheme code)
(define (compile-aux ast kont ignore-result?)
  (case (node-type ast)
    ((const)
     (if ignore-result?
	 (gen-apply kont)
	 (let ((k (node-val1 ast)))
	   (if (or (symbol? k) (null? k) (pair? k) (vector? k))
	       (gen-apply kont (list 'quote k))
	       (gen-apply kont k)))))
    ((var) (if ignore-result? 
	       (gen-apply kont)
	       (gen-apply kont (node-val1 ast))))
    ((apply)
     (let ((func (node-val1 ast))
	   (args (node-val2 ast)))
       (if (eq? (node-type func) 'error)
	   (generator-error-abort)
	   (case (node-val1 func)
	     ((erl-exit/1) (compile-aux (car args) 'erl-exit/1 #f))
	     ((erl-throw/1) (compile-aux (car args) 'erl-throw/1 #f))
	     ((erl-receive_next) (list 'erl-receive_next))
	     ((erl-receive_first)
	      (let ((t (gensymbol 't)))
		(compile-aux
		 (car args)
		 (gen-lambda (list t)
			     (list 'erl-receive_first
				   t
				   (if ignore-result?
				       (gen-lambda (list 'dummy)
						   (gen-apply kont))
				       kont)))
		 #f)))
	     (else
	      (let loop ((fargs (genvarlist-symbol 
				 (symbol->string (gensymbol 'ap))
				 (length args)
				 1))
			 (aargs args)
			 (rev-args '()))
		(if (null? fargs)
		    (let ((scm (apply gen-apply
				      (cons (node-val1 func)
					    (reverse rev-args)))))
		      (if ignore-result?
			  (gen-begin scm (gen-apply kont))
			  (gen-apply kont scm)))
		    (let* ((aarg (car aargs))
			   (bexpr (compile-bexpr aarg)))
		      (if bexpr
			  (loop (cdr fargs)
				(cdr aargs)
				(cons bexpr rev-args))
			  (compile-aux aarg
				       (gen-lambda
					(list (car fargs))
					(loop (cdr fargs)
					      (cdr aargs)
					      (cons (car fargs)
						    rev-args)))
				       #f))))))))))
    ((catch2)
     (compile-catch2 ast kont ignore-result?))
    ((fcase)
     (compile-fcase ast kont ignore-result?))
    ((namedfundef)
     (set! currentfun (node-val1 ast))
;     (display "** ") (write (node-val1 ast)) (display " :") (newline)
;     (pp-val
      (list ; gen-set!
       (node-val1 ast)
       ;; Fetch lambda expr from function definition
       (let* ((primcode (report (compile-aux (node-val2 ast)
					     '(lambda (x) x)
					     #f)))
	      (optcode (report (optimize primcode)))
	      (factcode (report (factorize-ifs optcode))))
	 ;; factcode is: (make_function arity (lambda (a1 .. an) SCMEXPR))
	 ;; or         : (make_function arity name)
	 (let ((lam (caddr factcode)))
	   (if (lambda? lam)
	       lam
	       (let loop ((n (cadr factcode))
			  (args '()))
		 (if (= n 0)
		     (list 'lambda args (cons lam args))
		     (loop (- n 1) (cons (gensymbol 'a) args)))))))));)
    ((fundefarit)
     (if ignore-result?
	 (gen-apply kont)
	 (gen-apply
	  kont
	  (gen-apply 'erl-function
		     (node-val1 ast)
		     (gen-lambda
		      (genvarlist-symbol (node-val2 ast) (node-val1 ast) 1)
		      (compile-ast (node-val3 ast)))))))
    ((prog) (compile-prog ast))
    ((error) (generator-error-abort))
    (else (internal-error!!! "compile-todo: " (node-type ast)))))


(define (compile-prog prog)
  (gen-let
   '()
   (gen-begin
    '(define-macro (erl-only-include macros defines) macros)
    (list 'include (filename-build (get-etos-root) "rt-gambit.scm"))
    (gen-letrec
     (map compile-ast (node-val1 prog))
     (let ((module-name (get-module-name))
           (exports (get-export-list))
           (imports remote-calls-list))

       (define (mfa->symbol m f a)
         (string->symbol
          (string-append (if m (string-append m ":") "")
                         f
                         "/"
                         (number->string a))))

       (define export-table
         (map (lambda (fa)
                (let* ((f (erl-atom->string (erl-tuple-ref fa 1)))
                       (a (erl-int->exact-integer (erl-tuple-ref fa 2)))
                       (local-var (mfa->symbol #f f a)))
                  (list 'vector f a local-var)))
              exports))

       (define install
         (append
          (map (lambda (fa)
                 (let* ((f (erl-atom->string (erl-tuple-ref fa 1)))
                        (a (erl-int->exact-integer (erl-tuple-ref fa 2)))
                        (local-var (mfa->symbol #f f a))
                        (global-var (mfa->symbol module-name f a)))
                   (list 'erl-function-set!
                         global-var
                         local-var)))
               exports)
          (map (lambda (mfa)
                 (let* ((m (erl-atom->string (car mfa)))
                        (f (erl-atom->string (cadr mfa)))
                        (a (erl-int->exact-integer (caddr mfa)))
                        (global-var (mfa->symbol m f a)))
                   (list 'if
                         (list 'erl-function-unbound? m f a)
                         (list 'erl-function-set!
                               global-var
                               (list 'erl-undefined-function m f)))))
               imports)))

       (define uninstall
         (map (lambda (fa)
                (let* ((f (erl-atom->string (erl-tuple-ref fa 1)))
                       (a (erl-int->exact-integer (erl-tuple-ref fa 2)))
                       (global-var (mfa->symbol module-name f a)))
                  (list 'set!
                        global-var
                        (list 'erl-undefined-function module-name f))))
              exports))

       (list
        'erl-install-module
        module-name
        (cons 'vector export-table)
        (cons 'lambda
              (cons '()
                    (if (null? install) '(#f) install)))
        (cons 'lambda
              (cons '()
                    (if (null? uninstall) '(#f) uninstall)))))))))

(define (compile-catch2 catch2 kont ignore-result?)
  (let ((ename (gensymbol 'catch_expr))
	(rname (gensymbol 'catch_result)))
    (gen-apply
     (gen-lambda (list rname)
		 (gen-begin (gen-apply 'erl-pop-abrupt!)
			    (gen-apply kont rname)))
     (gen-apply
      'call-with-current-continuation
      (gen-lambda
       (list 'abrupt)
       (gen-begin
	(gen-apply 'erl-push-abrupt! 'abrupt)
	(compile-aux (node-val1 catch2)
		     (gen-lambda (list ename)
				 (gen-apply 'erl-tuple ''NORMAL ename))
		     #f))))))) ; never ignored.

(define (compile-fcase fcase kont ignore-result?)
  (let* ((clauses (node-val2 fcase))
	 (clause (car clauses))
	 (pat (node-val1 clause))
	 (guards (node-val2 clause)))
    (if (and (eq? (node-type pat) 'universal)
	     (null? (filter (lambda (g) (not (eq? (node-type g) 'const)))
			    guards)))
	(compile-aux (node-val1 fcase)
		     (gen-lambda '()
				 (compile-aux (node-val3 clause)
					      kont
					      ignore-result?))
		     #t)
	(let ((kontneed (map node-val1
			     (diff-vars (node-fva fcase)
					(node-fva (node-val1 fcase)))))
	      (kontname (gensymbol 'casek)))
	  (let* ((cexpr (compile-sexpr (node-val1 fcase)))
		 (cexprname (gensymbol 'sel))
		 (choose_clause
		  (gen-apply
		   (gen-lambda (list kontname)
			       (compile-cinfos
				(clauses->cinfos clauses
						 (if cexpr cexpr cexprname)
						 kontname
						 kontneed
						 ignore-result?)))
		   (if (null? kontneed)
		       kont
		       (if ignore-result?
			   (gen-lambda kontneed (gen-apply kont))
			   (let ((rname (gensymbol 'bodyr)))
			     (gen-lambda (cons rname kontneed)
					 (gen-apply kont rname))))))))
	    (if cexpr
		choose_clause
		(compile-aux (node-val1 fcase)
			     (gen-lambda (list cexprname) ;selector needed
					 choose_clause)
			     #f)))))))

