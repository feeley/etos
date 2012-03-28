; file: "pm-build.scm"

; Copyright (C) 1998-1999 Universite de Montreal, All Rights Reserved.

; Pattern matcher's construction set

(define (make-all texpr1 texpr2)
  (case texpr1
    ((#t) texpr2)
    ((#f) #f)
    (else (case texpr2
	    ((#t) texpr1)
	    ((#f) #f)
	    (else (list 'all texpr1 texpr2))))))

(define (make-and texpr1 texpr2)
  (case texpr1
    ((#t) texpr2)
    ((#f) #f)
    (else (case texpr2
	    ((#t) texpr1)
	    ((#f) #f)
	    (else (list 'and texpr1 texpr2))))))

(define (make-one texpr1 texpr2)
  (case texpr1
    ((#t) #t)
    ((#f) texpr2)
    (else (case texpr2
	    ((#t) #t)
	    ((#f) texpr1)
	    (else (list 'one texpr1 texpr2))))))

(define (make-or texpr1 texpr2)
  (case texpr1
    ((#t) #t)
    ((#f) texpr2)
    (else (case texpr2
	    ((#t) #t)
	    ((#f) texpr1)
	    (else (list 'or texpr1 texpr2))))))

(define (make-not texpr)
  (case texpr
    ((#t) #f)
    ((#f) #t)
    (else (list 'not texpr))))

(define (make-kt texpr) (list 'kt texpr))
(define (make-kf texpr) (list 'kf texpr))
(define (make-exec . app) (cons 'exec app))
(define (make-if3 texpr true false unknown)
  (list 'if3 texpr true false unknown))
(define (make-anyk tst o texpr) (list 'anyk tst o texpr))

(define (make-cinfo bd-texpr pg-texpr bodycode bodyneed bindings)
  (vector 'cinfo bd-texpr pg-texpr bodycode bodyneed bindings))

(define (cinfo-bd-texpr cinfo) (vector-ref cinfo 1))
(define (cinfo-pg-texpr cinfo) (vector-ref cinfo 2))
(define (cinfo-bodycode cinfo) (vector-ref cinfo 3))
(define (cinfo-bodyneed cinfo) (vector-ref cinfo 4))
(define (cinfo-bindings cinfo) (vector-ref cinfo 5))

(define (cinfo-update-bd-texpr cinfo bd-texpr)
  (make-cinfo bd-texpr
	      (cinfo-pg-texpr cinfo)
	      (cinfo-bodycode cinfo)
	      (cinfo-bodyneed cinfo)
	      (cinfo-bindings cinfo)))
(define (cinfo-update-pg-texpr cinfo pg-texpr)
  (make-cinfo (cinfo-bd-texpr cinfo)
	      pg-texpr
	      (cinfo-bodycode cinfo)
	      (cinfo-bodyneed cinfo)
	      (cinfo-bindings cinfo)))
(define (cinfo-update-bodycode cinfo bodycode)
  (make-cinfo (cinfo-bd-texpr cinfo)
	      (cinfo-pg-texpr cinfo)
	      bodycode
	      (cinfo-bodyneed cinfo)
	      (cinfo-bindings cinfo)))
(define (cinfo-add-bindings cinfo bindings)
  (make-cinfo (cinfo-bd-texpr cinfo)
	      (cinfo-pg-texpr cinfo)
	      (cinfo-bodycode cinfo)
	      (cinfo-bodyneed cinfo)
	      (append bindings (cinfo-bindings cinfo))))

;; Build clauses info from clause ASTs
;; list of: #(bd-texpr pg-texpr body_code bodyneed bindings)
(define (clauses->cinfos clauses cexpr kont kontneed ignore-result?)
  (let loop ((clauses clauses))
    (if (null? clauses)
	'()
	(cons (let ((clause (car clauses)))
		(let* ((pat (node-val1 clause))
		       (guards (node-val2 clause))
		       (body (node-val3 clause))
		       (inputs (map node-val1 (node-input pat))))
		  (make-cinfo
		   (pat->texpr-bindings pat cexpr inputs)
		   (pg->texpr cexpr pat guards inputs)
		   (let ((bodyres (gensymbol 'be)))
		     (compile-aux
		      body
		      (if ignore-result?
			  (gen-lambda '()
				      (apply gen-apply (cons kont kontneed)))
			  (gen-lambda
			   (list bodyres)
			   (apply gen-apply
				  (cons kont (cons bodyres kontneed)))))
		      ignore-result?))
		   (map node-val1 (diff-vars (node-fvb body)
					     (node-fvb clause)))
		   (map (lambda (v) (cons v v)) inputs))))
	      (loop (cdr clauses))))))

;; Build texpr from:
;; cexpr : simple expression to access term to be matched
;; pat   : pattern to match against
;; guards: guards to be fulfilled
;; bv    : bound variables after evaluating cexpr
(define (pg->texpr cexpr pat guards bv)
  (let ((pat-texpr (pat->texpr pat cexpr bv))
	(guards-texpr (guards->texpr guards)))
    (make-all pat-texpr guards-texpr)))

(define (pat->texpr pat term bv)
;  (display "pattern:")(pp-ast pat)
;  (display "texpr  :")(pp (node-type pat))
;  (newline)
  (case (node-type pat)
    ((const)
     (let ((k (node-val1 pat)))
       (cond
	((erl-fixnum? k) (list 'erl-fix=k term k))
	((erl-bignum? k) (make-and (list 'erl-sub? term)
				   (list 'erl-big? term)
				   (list 'erl-big=k term k)))
	((erl-flonum? k) (make-and (list 'erl-sub? term)
				   (list 'erl-flo? term)
				   (list 'erl-flo=k term k)))
	((erl-atom? k) (list 'erl-ato=k term (list 'quote k)))
	((erl-char? k) (make-and (list 'erl-spc? term)
				 (list 'erl-chr=k term k)))
	((erl-nil? k) (list 'erl-nil? term))
	(else (internal-error!!! "pat->texpr")))))
    ((var)
     (let ((varname (node-val1 pat)))
       (if (memq varname bv)
	   (list 'erl-=:= varname term)
	   #t)))
    ((cons)
     (cond
      ((and (pair? term) (eq? (car term) 'erl-cons))
       (make-all (pat->texpr (node-val1 pat) (cadr term) bv)
		 (pat->texpr (node-val2 pat) (caddr term) bv)))
      (#t;(symbol? term) ;; variable
       (make-and 
	(list 'erl-con? term)
	(make-all (pat->texpr (node-val1 pat) (list 'erl-hd term) bv)
		  (pat->texpr (node-val2 pat) (list 'erl-tl term) bv))))
      (else #f)))
    ((tuple)
     (cond
      ((and (pair? term) (eq? (car term) 'erl-tuple))
       (let loop ((pelts (node-val1 pat)) (telts (cdr term)))
	 (and (eq? (null? pelts) (null? telts)) ; different sizes?
	      (or (null? pelts)
		  (make-all (pat->texpr (car pelts) (car telts) bv)
			    (loop (cdr pelts) (cdr telts)))))))
      (#t;(symbol? term) ;; variable
       (let ((elts (node-val1 pat)))
	 (make-and
	  (list 'erl-sub? term)
	  (make-and
	   (list 'erl-vec? term)
	   (make-and (list 'erl-fix=k
			   (list 'erl-vector-length term)
			   (+ 1 (length elts)))
		     (make-all (list 'erl-ato=k
				     (list 'erl-vector-ref term 0)
				     ''tuple)
			       (let loop ((elts elts) (idx 1))
				 (if (null? elts)
				     #t
				     (make-all 
				      (pat->texpr (car elts)
						  (list 'erl-vector-ref
							term
							idx)
						  bv)
				      (loop (cdr elts) (+ idx 1)))))))))))
      (else #f)))
    ((match)
     (make-all (pat->texpr (node-val1 pat) term bv)
	       (pat->texpr (node-val2 pat) term bv)))
    ((universal) #t)
    (else (internal-error!!! "pat->texpr"))))

;; build texpr containing only bindings (and required tests)
;; Only these constructs: bind, and, all, #t, #f
;; Only these tests: con? sub? vec? fix=k
(define (pat->texpr-bindings pat term bv)
  (let ((res
  (case (node-type pat)
    ((const universal) #t)
    ((var) (let ((varname (node-val1 pat)))
	     (if (memq varname bv)
		 #t
		 (list 'bind varname term))))
    ((cons)
     (cond
      ((and (pair? term) (eq? (car term) 'erl-cons))
       (make-all (pat->texpr-bindings (node-val1 pat) (cadr term) bv)
		 (pat->texpr-bindings (node-val2 pat) (caddr term) bv)))
      (#t ;(symbol? term) ;; variable
       (make-and
	(list 'erl-con? term)
	(make-all
	 (pat->texpr-bindings (node-val1 pat) (list 'erl-hd term) bv)
	 (pat->texpr-bindings (node-val2 pat) (list 'erl-tl term) bv))))
      (else #f)))
    ((tuple)
     (cond
      ((and (pair? term) (eq? (car term) 'erl-tuple))
       (let loop ((pelts (node-val1 pat)) (telts (cdr term)))
	 (and (eq? (null? pelts) (null? telts)) ; different sizes
	      (or (null? pelts)
		  (make-all (pat->texpr-bindings (car pelts) (car telts) bv)
			    (loop (cdr pelts) (cdr telts)))))))
      (#t;(symbol? term) ;; variable
       (let ((elts (node-val1 pat)))
	 (make-and
	  (list 'erl-sub? term)
	  (make-and
	   (list 'erl-vec? term)
	   (make-and (list 'erl-fix=k
			   (list 'erl-vector-length term)
			   (+ 1 (length elts)))
		     (let loop ((elts elts) (idx 1))
		       (if (null? elts)
			   #t
			   (make-all
			    (pat->texpr-bindings (car elts)
						 (list 'erl-vector-ref
						       term
						       idx)
						 bv)
			    (loop (cdr elts) (+ idx 1))))))))))
      (else #f)))
    ((match)
     (make-all (pat->texpr-bindings (node-val1 pat) term bv)
	       (pat->texpr-bindings (node-val2 pat) term bv)))
    (else (internal-error!!! "pat->texpr-bindings"))))
)
;    (display "pat  :") (pp-ast pat)
;    (display "texpr:") (pp res)
;    (display "bv   :") (pp bv)
;    (newline)
    res))


;; Build texpr from a list of guards
(define (guards->texpr astlist)
  (if (null? astlist)
      #t
      (let ((hd (car astlist))
	    (tl (cdr astlist)))
	(if (null? tl)
	    (guard->texpr hd)
	    (make-all
		  (guard->texpr hd)
		  (guards->texpr tl))))))

;; Build texpr from a guard
;; The guard is guaranteed to be syntactically correct
(define (guard->texpr guard)
  (case (node-type guard)
    ((const) #t)
    ((compare)
     (let ((cop (node-val1 guard))
	   (arg1 (node-val2 guard))
	   (arg2 (node-val3 guard)))
       (let ((arg1c (gexpr->code arg1)) (arg2c (gexpr->code arg2)))
	 (make-and
	  (make-all (gexpr->test arg1) (gexpr->test arg2))
	  (case cop
	    ((<) (list 'erl-< arg1c arg2c))
	    ((>) (list 'erl-< arg2c arg1c))
	    ((<=) (make-not (list 'erl-< arg2c arg1c)))
	    ((>=) (make-not (list 'erl-< arg1c arg2c)))
	    ((=:=) (list 'erl-=:= arg1c arg2c))
	    ((=/=) (make-not (list 'erl-=:= arg1c arg2c)))
	    ((==) (list 'erl-== arg1c arg2c))
	    ((/=) (make-not (list 'erl-== arg1c arg2c))))))))
    ((apply) ; recognizer bifs
     (let ((arg (car (node-val2 guard))))
       (let ((argtest (gexpr->test arg))
	     (argcode (gexpr->code arg))
	     (vector-type-test
	      (lambda (type code)
		(make-and (list 'erl-sub? code)
			  (make-and
			   (list 'erl-vec? code)
			   (list 'erl-ato=k
				 (list 'erl-vector-ref code 0)
				 type))))))
	 (make-and
	  argtest
	  (case (node-val1 (node-val1 guard))
	    ((is_atom/1) (make-and (list 'erl-sub? argcode)
				   (list 'erl-ato? argcode)))
	    ((is_binary/1) (vector-type-test ''binary argcode))
	    ((is_char/1) (make-and (list 'erl-spc? argcode)
				   (list 'erl-chr? argcode)))
	    ((is_compound/1)
	     (make-one (list 'erl-nil? argcode)
		       (make-one (list 'erl-con? argcode)
				 (vector-type-test ''tuple argcode))))
	    ((is_cons/1) (list 'erl-con? argcode))
	    ((is_float/1) (make-and (list 'erl-sub? argcode)
				    (list 'erl-flo? argcode)))
	    ((is_function/1) (vector-type-test ''function argcode))
	    ((is_integer/1) (list 'erl-int? argcode))
	    ((is_list/1) (list 'erl-lst? argcode))
	    ((is_null/1) (list 'erl-nil? argcode))
	    ((is_number/1) (list 'erl-num? argcode))
	    ((is_pid/1) (vector-type-test ''pid argcode))
	    ((is_port/1) (vector-type-test ''port argcode))
	    ((is_ref/1) (vector-type-test ''ref argcode))
	    ((is_tuple/1) (vector-type-test ''tuple argcode))
	    ((is_string/1) (list 'erl-str? argcode))
	    (else (internal-error!!! "invalid recognizer: " guard)))))))
    ((error) (generator-error-abort))
    (else (internal-error!!! "invalid guard: " guard))))

;; Takes length(X) and returns length(tl(X))
(define (build-length-tl gexpr)
  (newast-apply (node-val1 gexpr)
		(list (newast-apply (newast-const 'hd/1)
				    (node-val2 gexpr)))))

;; Returns a texpr evaluating a guard expression
(define (gexpr->code gexpr)
  (case (node-type gexpr)
    ((const) (node-val1 gexpr))
;(let ((k (node-val1 gexpr)))
;	       (if (symbol? k)
;		   (list 'quote k)
;		   k)))
    ((var) (node-val1 gexpr))
    ((tuple) (cons 'erl-tuple (map gexpr->code (node-val1 gexpr))))
    ((cons) (list 'erl-cons
		  (gexpr->code (node-val1 gexpr))
		  (gexpr->code (node-val2 gexpr))))
    ((apply)
     (let ((fname (node-val1 (node-val1 gexpr)))
	   (args (node-val2 gexpr)))
       (cond
	((null? args) ; arity 0
	 (case fname
	   ((node/0) (list 'node/0))
	   ((nodes/0) (list 'nodes/0))
	   ((self/0) (list 'self/0))
	   (else (internal-error!!! "unknown guard bif: " fname))))
	((null? (cdr args)) ; arity 1
	 (let ((argcode (gexpr->code (car args))))
	   (case fname
	     ((abs/1) (list 'erl-safe-abs/1 argcode))
	     ((float/1) (make-or
			 (make-and (make-kt (list 'erl-flo? argcode))
				   argcode)
			 (make-or (make-and (list 'kt (list 'erl-int? argcode))
					    (list 'erl-safe-float/1 argcode))
				  (list 'erl-safe-float/1 argcode))))
	     ((hd/1) (list 'erl-hd argcode))
	     ((length/1) (make-or
			  (make-and (make-kt (list 'erl-nil? argcode))
				    0)
			  (make-or
			   (make-and (make-kt (list 'erl-con? argcode))
				     (list `fix+/2
					   (make-exec
					    (lambda ()
					      (gexpr->code
					       (build-length-tl gexpr))))
					   1))
			   (list 'erl-safe-length/1 argcode))))
	     ((node/1) (list 'erl-safe-node/1 argcode))
	     ((round/1) (make-or (make-and (make-kt (list 'erl-int? argcode))
					   argcode)
				 (list 'erl-safe-round/1 argcode)))
	     ((sign/1) (list 'erl-safe-sign/1 argcode))
	     ((size/1) (list 'erl-safe-size/1 argcode))
	     ((tl/1) (list 'erl-tl argcode))
	     ((trunc/1) (make-or (make-and (make-kt (list 'erl-int? argcode))
					   argcode)
				 (list 'erl-safe-trunc/1 argcode)))
	     ((+/1) argcode)
	     ((-/1) (list 'erl-safe-/1 argcode))
	     ((bnot/1) (list 'erl-safe-bnot/1 argcode))
	     ((not/1) (make-or
		       (make-and (make-kt (list 'erl-ato=k argcode ''true))
				 ''false)
		       (make-and (make-kt (list 'erl-ato=k argcode ''false))
				 ''true)
		       (list 'erl-safe-not/1 argcode)))
	     (else (internal-error!!! "unknown guard bif: " fname)))))
	(else ; arity 2
	 (let ((arg1code (gexpr->code (car args)))
	       (arg2code (gexpr->code (cadr args))))
	   (case fname
	     ((element/2) (list 'erl-vector-ref arg2code arg1code))
	     ((+/2) (list 'erl-safe-+/2 arg1code arg2code))
	     ((-/2) (list 'erl-safe--/2 arg1code arg2code))
	     ((bor/2) (list 'erl-safe-bor/2 arg1code arg2code))
	     ((bxor/2) (list 'erl-safe-bxor/2 arg1code arg2code))
	     ((bsl/2) (list 'erl-safe-bsl/2 arg1code arg2code))
	     ((bsr/2) (list 'erl-safe-bsr/2 arg1code arg2code))
	     ((*/2) (list 'erl-safe-*/2 arg1code arg2code))
	     ((//2) (list 'erl-safe-//2 arg1code arg2code))
	     ((///2) (list 'erl-safe-///2 arg1code arg2code))
	     ((div/2) (list 'erl-safe-div/2 arg1code arg2code))
	     ((mod/2) (list 'erl-safe-mod/2 arg1code arg2code))
	     ((rem/2) (list 'erl-safe-rem/2 arg1code arg2code))
	     ((band/2) (list 'erl-safe-band/2 arg1code arg2code))
	     (else (internal-error!!! "unknown guard bif: " fname))))))))
    (else (internal-error!!! "invalid guard expr:" gexpr))))


(define (binop->test typetest arg1code arg2code calc-test)
  (make-or (make-kt (make-all (list typetest arg1code)
			      (list typetest arg2code)))
	   (list calc-test arg1code arg2code)))

; Returns a texpr of required tests for normal evaluation of a guard expression
(define (gexpr->test gexpr)
  (case (node-type gexpr)
    ((const) #t)
    ((var) (list 'bnd? (node-val1 gexpr)))
    ((tuple) (fold (lambda (a b) (make-all a b))
		   (cons #t (map gexpr->test (node-val1 gexpr)))))
    ((cons) (make-all
		  (gexpr->test (node-val1 gexpr))
		  (gexpr->test (node-val2 gexpr))))
    ((apply)
     (let ((fname (node-val1 (node-val1 gexpr)))
	   (args (node-val2 gexpr)))
       (cond
	((null? args) #t) ; arity 0 always completes normally
	((null? (cdr args)) ; arity 1
	 (let* ((arg (car args))
		(argtest (gexpr->test arg)) 
		(argcode (gexpr->code arg)))
	   (make-and
	    argtest
	    (case fname
	      ((abs/1) (make-or (make-kt (list 'erl-num? argcode))
				(list 'erl-tst-abs/1 argcode)))
	      ((float/1) (make-or (make-kt (list 'erl-num? argcode))
				  (list 'erl-tst-float/1 argcode)))
	      ((hd/1) (list 'erl-con? argcode))
	      ((length/1) (make-or (make-kt (list 'erl-lst? argcode))
				   (list 'erl-tst-length/1 argcode)))
	      ((node/1) (list 'erl-tst-node/1 argcode))
	      ((round/1) (make-or (make-kt (list 'erl-num? argcode))
				  (list 'erl-tst-round/1 argcode)))
	      ((sign/1) (make-or (make-kt (list 'num? argcode))
				 (list 'erl-tst-sign/1 argcode)))
	      ((size/1) (list 'erl-tst-size/1 argcode))
	      ((tl/1) (list 'erl-con? argcode))
	      ((trunc/1) (make-or (make-kt (list 'erl-num? argcode))
				  (list 'erl-tst-trunc/1 argcode)))
	      ((+/1) (make-or (make-kt (list 'erl-num? argcode))
			      (list 'erl-tst-+/1 argcode)))
	      ((-/1) (make-or (make-kt (list 'erl-num? argcode))
			      (list 'erl-tst--/1 argcode)))
	      ((bnot/1) (make-or (make-kt (list 'erl-int? argcode))
				 (list 'erl-tst-bnot/1 argcode)))
	      ((not/1) (make-or
			(make-kt (make-or (list 'erl-ato=k argcode ''true)
					  (list 'erl-ato=k argcode ''false)))
			(list 'erl-tst-not/1 argcode)))
	      (else (internal-error!!! "unknown guard bif: " fname))))))
	(else ; arity 2
	 (let* ((arg1 (car args)) (arg2 (cadr args))
		(arg1t (gexpr->test arg1)) (arg2t (gexpr->test arg2)) 
		(arg1c (gexpr->code arg1)) (arg2c (gexpr->code arg2)))
	   (make-and
	    (make-all arg1t arg2t)
	    (case fname
	      ((element/2) (list 'erl-tst-element/2 arg1c arg2c))
	      ((+/2) (binop->test 'erl-num? arg1c arg2c 'erl-tst-+/2))
	      ((-/2) (binop->test 'erl-num? arg1c arg2c 'erl-tst--/2))
	      ((bor/2) (binop->test 'erl-int? arg1c arg2c 'erl-tst-bor/2))
	      ((bxor/2) (binop->test 'erl-int? arg1c arg2c 'erl-tst-bxor/2))
	      ((bsl/2) (binop->test 'erl-int? arg1c arg2c 'erl-tst-bsl/2))
	      ((bsr/2) (binop->test 'erl-int? arg1c arg2c 'erl-tst-bsr/2))
	      ((*/2) (binop->test 'erl-num? arg1c arg2c 'erl-tst-*/2))
	      ((//2) (binop->test 'erl-num? arg1c arg2c 'erl-tst-//2))
	      ((///2) (binop->test 'erl-int? arg1c arg2c 'erl-tst-///2))
	      ((div/2) (binop->test 'erl-int? arg1c arg2c 'erl-tst-div/2))
	      ((mod/2) (binop->test 'erl-int? arg1c arg2c 'erl-tst-mod/2))
	      ((rem/2) (binop->test 'erl-int? arg1c arg2c 'erl-tst-rem/2))
	      ((band/2) (binop->test 'erl-int? arg1c arg2c 'erl-tst-band/2))
	      (else (internal-error!!! "unknown guard bif: " fname)))))))))
    (else (internal-error!!! "invalid guard expr:" gexpr))))

								    
			

