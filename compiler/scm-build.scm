; file: "scm-build.scm"

; Copyright (C) 1998-1999 Universite de Montreal, All Rights Reserved.

; Scheme code construction set


(define identity '(lambda (x) x))

(define (quote? scm)
  (and (pair? scm) (eq? (car scm) 'quote)))

(define (quoted-atom? scm)
  (and (pair? scm) (eq? (car scm) 'quote) (symbol? (cadr scm))))

(define (lambda? scm)
  (and (pair? scm) (eq? (car scm) 'lambda)))
(define lambda-args cadr)
(define lambda-body caddr)

(define (let? scm)
  (and (pair? scm) (eq? (car scm) 'let)))
(define let-bindings cadr)
(define let-body caddr)

(define (letrec? scm)
  (and (pair? scm) (eq? (car scm) 'letrec)))
(define letrec-bindings cadr)
(define letrec-body caddr)

(define (if? scm)
  (and (pair? scm) (eq? (car scm) 'if)))
(define if-test cadr)
(define if-then caddr)
(define if-else cadddr)

(define (or? scm)
  (and (pair? scm) (eq? (car scm) 'or)))
(define (and? scm)
  (and (pair? scm) (eq? (car scm) 'and)))

(define (begin? scm)
  (and (pair? scm) (eq? (car scm) 'begin)))

(define (const? scm)
  (if (pair? scm)
      (eq? (car scm) 'quote)
      (not (symbol? scm))))

(define (count-ref scm var)
  (cond
   ((eq? scm var) 1)
   ((lambda? scm) (if (memq var (lambda-args scm))
		      0
		      (count-ref (lambda-body scm) var)))
   ((let? scm) (let ((bindings (let-bindings scm)))
		 (+ (fold +
			  (map (lambda (bnd) (count-ref (cadr bnd) var))
			       bindings))
		    (if (memq var (map car bindings))
			0
			(count-ref (let-body scm) var)))))
   ((const? scm) 0)
   ((pair? scm) (fold + (map (lambda (scm) (count-ref scm var)) scm)))
   (else 0)))

(define (scm-replace-vars scm env)
  (cond
   ((and (symbol? scm)
	 (let ((found (assq scm env)))
	   (if found
	       (cdr found)
	       scm))))
   ((lambda? scm)
    (let* ((fargs (lambda-args scm))
	   (newenv (filter (lambda (ve)
			     (not (memq (car ve) fargs)))
			   env)))
      (if (null? newenv)
	  scm
	  (gen-lambda (lambda-args scm)
		      (scm-replace-vars (lambda-body scm) newenv)))))
   ((let? scm)
    (let* ((bnds (let-bindings scm))
	   (fargs (map car bnds)))
      (gen-let (map (lambda (bnd)
		      (list (car bnd) (scm-replace-vars (cadr bnd) env)))
		    bnds)
	       (let ((newenv (filter (lambda (ve)
				       (not (memq (car ve) fargs)))
				     env)))
		 (if (null? newenv)
		     (let-body scm)
		     (scm-replace-vars (let-body scm) newenv))))))
   ((const? scm) scm)
   ((pair? scm) (map (lambda (scm) (scm-replace-vars scm env)) scm))
   (else scm)))

(define (simple? scm)
  (if (pair? scm)
      (let ((kind (car scm)))
	(or (eq? kind 'quote) ; a constant
	    (and (eq? kind 'lambda)
		 (let ((body (lambda-body scm)))
		   (or (simple? body) ; a simple lambda
		       (and (pair? body)
			    (null? (cdr body)))))))) ; a thunk-call lambda body
      #t))

;; Simplifie a scheme expression
;;  given we don't care about its result, just it's (side)-effect
(define (scm-effect scm)
  (cond
   ((const? scm) #f)
   ((symbol? scm) #f)
   ((lambda? scm) #f)
   ((if? scm) (let ((test-effect (scm-effect (if-test scm)))
		    (then-effect (scm-effect (if-then scm)))
		    (else-effect (scm-effect (if-else scm))))
		(cond
		 ((and (not then-effect) (not else-effect))
		  test-effect)
		 ((or then-effect else-effect)
		  (gen-if (if-test scm)
			  then-effect
			  else-effect))
		 (else #f))))
;		(if (or test-effect then-effect else-effect)
;		    (list 'if
;			  (if-test scm)
;			  then-effect
;			  else-effect)
;		    #f)))
   ((let? scm)
;    (gen-let (let-bindings scm) (scm-effect (let-body scm))))
    (let ((body-effect (scm-effect (let-body scm)))
	  (bindings (let-bindings scm)))
      (apply gen-apply
	     (cons (gen-lambda (map car bindings)
			       body-effect)
		   (map cadr bindings)))))
   ((or (and (pair? scm)
	     (memq (car scm) no-effect-fun-list))
	(begin? scm))
    (apply gen-begin (cdr scm)))
   (else scm)))

(define (gen-set! var expr)
  (if (lambda? expr)
      (list 'define
	    (cons var (lambda-args expr))
	    (lambda-body expr))
      (list 'set! var expr)))


(define (gen-begin . exprs)
  (if (null? exprs)
      #f
      (let loop ((exprs exprs) (rev-res '()))
	(let ((hd (car exprs)) (tl (cdr exprs)))
	  (if (begin? hd)
	      (loop (append (cdr hd) (cdr exprs)) rev-res)
	      (if (null? tl)
		  (if (null? rev-res)
		      (scm-effect hd)
		      (cons 'begin (reverse (cons hd rev-res))))
		  (let ((se (scm-effect hd)))
		    (if se
			(loop tl (cons se rev-res))
			(loop tl rev-res)))))))))

(define (gen-and . exprs)
  (cons 'and
	(let loop ((exprs exprs))
	  (if (null? exprs)
	      '()
	      (let ((expr (car exprs)))
		(if (and? expr)
		    (loop (append (cdr expr) (cdr exprs)))
		    (cons expr (loop (cdr exprs)))))))))

(define (gen-or . exprs)
  (cons 'or
	(let loop ((exprs exprs))
	  (if (null? exprs)
	      '()
	      (let ((expr (car exprs)))
		(if (or? expr)
		    (loop (append (cdr expr) (cdr exprs)))
		    (cons expr (loop (cdr exprs)))))))))

; Here, we make common subexpression elimination
(define (count-sub scm sub)
  (cond
   ((if? scm) (+ (count-sub (if-test scm) sub)
		 (count-sub (if-then scm) sub)
		 (count-sub (if-else scm) sub)))
   ((let? scm) (+ (count-sub (let-body scm) sub)
		  (fold + (map (lambda (bnd)
				 (count-sub (cadr bnd) sub))
			       (let-bindings scm)))))
   ((lambda? scm) (count-sub (lambda-body scm) sub))
   ((equal? scm sub) 1)
   ((pair? scm)
    (let ((fun (car scm)))
      (let ((equiv (assq fun fun-super)))
	(if equiv
	    (count-sub (cons (cdr equiv) (cdr scm)) sub)
	    (fold + (map (lambda (scm) (count-sub scm sub)) scm))))))
   (else 0)))

(define (scm-replace-expr scm expr val)
  (cond
   ((equal? scm expr) val)
   ((lambda? scm) (gen-lambda (lambda-args scm)
			      (scm-replace-expr (lambda-body scm) expr val)))
   ((let? scm)
    (let* ((lb (let-bindings scm))
	   (formals (map car lb)))
      (apply gen-apply
	     (cons (gen-lambda formals
			       (scm-replace-expr (let-body scm) expr val))
		   (map (lambda (fa)
			  (scm-replace-expr (cadr fa) expr val))
			lb)))))
   ((pair? scm)
    (let ((fun (car scm)) (args (cdr scm)))
      (let ((equiv (assq fun fun-super)))
	(if (and equiv
		 (equal? (cons (cdr equiv) args) expr))
	    val
	    (apply gen-apply
		   (map (lambda (scm)
			  (scm-replace-expr scm expr val))
			scm))))))
   (else scm)))

(define (test->subs test)
  (cond
   ((and (pair? test)
	 (not (eq? (car test) 'quote)))
    (cons test (apply append (map test->subs test))))
   (else '())))

(define (gen-if test then elsa)
  (list 'if test then elsa))

(define (gen-if-opt test then elsa)
  (let loop ((subs (test->subs test)))
    (if (null? subs)
	(list 'if test then elsa)
	(let ((sub (car subs)))
	  (let ((subcount (+ (count-sub then sub)
			     (count-sub elsa sub))))
	    (if (> subcount 0)
		(let ((subname (gensymbol 'sub)))
; 		  (gen-apply-opt
; 		   (gen-lambda
; 		    (list subname)
; 		    (gen-if-opt
; ;		     (optimize (scm-replace-expr test sub subname))
; ;		     (optimize (scm-replace-expr then sub subname))
; ;		     (optimize (scm-replace-expr elsa sub subname))))
; 		     (scm-replace-expr test sub subname)
; 		     (scm-replace-expr then sub subname)
; 		     (scm-replace-expr elsa sub subname)))
; 		   sub))
		  (gen-let
		   (list (list subname sub))
		   (gen-if-opt
		    (scm-replace-expr test sub subname)
		    (scm-replace-expr then sub subname)
		    (scm-replace-expr elsa sub subname))))
		(loop (cdr subs))))))))

;; Factorize ifs in code with 'and's and 'or's
(define (factorize-ifs scm)
  (cond
   ((if? scm)
    (let ((test (if-test scm))
	  (then (factorize-ifs (if-then scm)))
	  (elsa (factorize-ifs (if-else scm))))
      (cond
       ((equal? then elsa) then)
       ((and (if? elsa) (equal? then (if-then elsa)))
	(factorize-ifs (list 'if
			     (gen-or test (if-test elsa))
			     then
			     (if-else elsa))))
       ((and (if? elsa) (equal? then (if-else elsa)))
	(factorize-ifs (list 'if
			     (gen-or test (list 'not (if-test elsa)))
			     then
			     (if-then elsa))))
       ((and (if? then) (equal? elsa (if-else then)))
	(factorize-ifs (list 'if
			     (gen-and test (if-test then))
			     (if-then then)
			     elsa)))
       ((and (if? then) (equal? elsa (if-then then)))
	(factorize-ifs (list 'if
			     (gen-and test (list 'not (if-test then)))
			     (if-else then)
			     elsa)))
       (else
	(list 'if test then elsa)))))
   ((let? scm)
    (gen-let (map (lambda (bnd) (list (car bnd) (factorize-ifs (cadr bnd))))
		  (let-bindings scm))
	     (factorize-ifs (let-body scm))))
;    (let ((bindings (let-bindings scm)))
;      (apply gen-apply-opt
;	     (cons (gen-lambda (map car bindings)
;			       (factorize-ifs (let-body scm)))
;		   (map (lambda (bnd) (factorize-ifs (cadr bnd)))
;			bindings)))))
   ((lambda? scm)
    (gen-lambda (lambda-args scm) (factorize-ifs (lambda-body scm))))
   ((pair? scm)
    (map factorize-ifs scm))
   (else scm)))

(define (unquote-const scm)
  (if (pair? scm)
      (cadr scm)
      scm))

(define (gen-lambda formals body)
  (if (and (pair? body)
	   (equal? formals (cdr body))) ;; renaming
      (car body)
      (list 'lambda formals body)))

(define (gen-let bindings body)
  (list 'let bindings body))

(define (gen-letrec bindings body)
  (list 'letrec bindings body))

(define (gen-apply app . args)
  (cons app args))

;; app isn't optimized here, args are
(define (gen-apply-opt app . args)
  (cond
   ((lambda? app)
    (let loop ((body (lambda-body app))
	       (fargs (lambda-args app))
	       (aargs args)
	       (rev-fargs '())
	       (rev-aargs '())
	       (newenv '()))
      (if (null? fargs)
	  (let ((newbody (if (null? newenv)
			     body
			     (scm-replace-vars body newenv))))
	    (if (null? rev-fargs)
		(optimize newbody)
		(gen-let (reverse (map (lambda (f a) (list f a))
				       rev-fargs
				       rev-aargs))
			 (optimize newbody))))
	  (let ((farg (car fargs)) (aarg (car aargs)))
	    (cond
	     ((simple? aarg) ; copy/constant propagation
	      (loop body
		    (cdr fargs)
		    (cdr aargs)
		    rev-fargs
		    rev-aargs
		    (cons (cons farg aarg) newenv)))
	     (else
	      (let ((refcount (count-ref body farg)))
;	       (display "app : ") (pp app)
;	       (display "args: ") (pp args)
;	       (display "farg: ") (pp farg)
;	       (display "body: ") (pp body)
;	       (display "refc: ") (pp refcount)
;	       (newline)
		(cond
		 ((= refcount 0) ; useless code elim.
		  (let ((aarg-effect (scm-effect aarg)))
		    (if aarg-effect
			(loop body
			      (cdr fargs)
			      (cdr aargs)
			      (cons farg rev-fargs)
			      (cons aarg-effect rev-aargs)
			      newenv)
			(loop body
			      (cdr fargs)
			      (cdr aargs)
			      rev-fargs
			      rev-aargs
			      newenv))))
		 ((= refcount 1) ; safe inlining if no side-effect
		  (if (and (scm-effect aarg) (scm-effect body))
		      (loop body
			    (cdr fargs)
			    (cdr aargs)
			    (cons farg rev-fargs)
			    (cons aarg rev-aargs)
			    newenv)
		      (loop body
			    (cdr fargs)
			    (cdr aargs)
			    rev-fargs
			    rev-aargs
			    (cons (cons farg aarg) newenv))))
		 (else ; check to factorize aarg in body
		  (let ((aarg-effect (scm-effect aarg)))
		    (if aarg-effect
			(loop body
			      (cdr fargs)
			      (cdr aargs)
			      (cons farg rev-fargs)
			      (cons aarg rev-aargs)
			      newenv)
			(loop (scm-replace-expr body aarg farg)
			      (cdr fargs)
			      (cdr aargs)
			      (cons farg rev-fargs)
			      (cons aarg rev-aargs)
			      newenv))))))))))))
   ;; some partial evaluation
   ((and (eq? app 'erl-function-lambda)
	 (not (null? args))
	 (null? (cdr args))
	 (let ((arg (car args)))
	   (and (pair? arg)
		(eq? (car arg) 'erl-function))))
    (optimize (caddar args)))
   ((and (symbol? app) ; constant folding
	 (null? (filter (lambda (scm) (not (const? scm))) args)))
    (cond
     ((eq? app 'erl-cons)
      (list 'quote (apply cons (map unquote-const args))))
     ((eq? app 'erl-tuple)
      (list 'quote (apply vector (cons 'tuple (map unquote-const args)))))
     ((and (eq? app '+/2) (erl-num? (car args)) (erl-num? (cadr args)))
      (apply + args))
     ((and (eq? app '-/2) (erl-num? (car args)) (erl-num? (cadr args)))
      (apply - args))
     ((and (eq? app '*/2) (erl-num? (car args)) (erl-num? (cadr args)))
      (apply * args))
     ((and (eq? app 'erl-mfa->fname)
	   (quoted-atom? (car args))
	   (quoted-atom? (cadr args))
	   (erl-fix? (caddr args)))
      (list 'quote (apply erl-mfa->fname (map unquote-const args))))
;; To transform (erl-exit/1 'reason) to (erl-exit-reason)
;     ((and (eq? app 'erl-exit/1)
;	   (quoted-atom? (car args)))
;      (case (unquote-const (car args))
;	((badarg) (cons 'erl-exit-badarg '()))
;	((case_clause) (cons 'erl-exit-case_clause '()))
;	((function_clause) (cons 'erl-exit-function_clause '()))
;	((lambda_clause) (cons 'erl-exit-lambda_clause '()))
;	(else (cons app args))))
     (else (cons app args))))
   (else (cons (optimize app) args))))

;(define (optimize scm) (list 1 2 scm))
(define (optimize scm)
  (cond
   ((const? scm) scm)
   ((symbol? scm) scm)
   ((if? scm) (apply gen-if-opt (map optimize (cdr scm))))
   ((lambda? scm) (gen-lambda (lambda-args scm)
 			      (optimize (lambda-body scm))))
   ((let? scm)
    (gen-let (map (lambda (bnd)
		    (list (car bnd) (optimize (cadr bnd))))
		  (let-bindings scm))
	     (optimize (let-body scm))))
   ((begin? scm) (apply gen-begin (map optimize (cdr scm))))
   ((pair? scm)
    (apply gen-apply-opt (cons (car scm) (map optimize (cdr scm)))))
   (else (internal-error!!! "optimize"))))









