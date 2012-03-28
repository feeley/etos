; file: "pm-predict.scm"

; Copyright (C) 1998-1999 Universite de Montreal, All Rights Reserved.

; Pattern matcher's predicates
(define-structure pinfo truths lies unknowns bindings)

(define (pinfo+binding pinfo binding)
  (make-pinfo (pinfo-truths pinfo)
	      (pinfo-lies pinfo)
	      (pinfo-unknowns pinfo)
	      (cons binding (pinfo-bindings pinfo))))

;; MUCH faster
(define (pinfo+unknown! pinfo unknown)
  (pinfo-unknowns-set! pinfo (cons unknown (pinfo-unknowns pinfo))))

(define (texpr-simple? texpr)
  (if (pair? texpr)
      (let ((app (car texpr)))
	(and (not (memq app
			'(bnd? kt kf one or all and not if3 quote exec anyk)))
	     (let loop ((args (cdr texpr)))
	       (or (null? args)
		   (and (texpr-simple? (car args))
			(loop (cdr args)))))))
      #t))

;; Predict the result of a texpr
;; May return #t #f or a "possibly" simpler texpr
;; kt, kf, if3 and anyk are fully evaluated
(define (texpr-predict! texpr pinfo)
  (let loop ((texpr texpr))
    ;(pp (vector 'pred texpr)); (pinfo-unknowns pinfo)))
    (cond
     ((pair? texpr)
      (let ((app (car texpr)) (args (cdr texpr)))
	(case app
	  ((one)
	   (let ((texpr1 (loop (car args))))
	     (if (eq? texpr1 #t)
		 texpr1
		 (make-one texpr1 (loop (cadr args))))))
	  ((or)
	   (let ((texpr1 (loop (car args))))
	     (if (eq? texpr1 #t)
		 texpr1
		 (if (eq? texpr1 #f)
		     (loop (cadr args))
		     (make-or texpr1 (cadr args))))))
	  ((all)
	   (let ((texpr1 (loop (car args))))
	     (if (eq? texpr1 #f)
		 texpr1
		 (make-all texpr1 (loop (cadr args))))))
	  ((and)
	   (let ((texpr1 (loop (car args))))
	     (if (eq? texpr1 #f)
		 texpr1
		 (if (eq? texpr1 #t)
		     (loop (cadr args))
		     (make-and texpr1 (cadr args))))))
	  ((not) (make-not (loop (car args))))
	  ((if3)
	   (let ((texpr (loop (car args))))
	     (case texpr
	       ((#t) (loop (cadr args)))
	       ((#f) (loop (caddr args)))
	       (else (loop (cadddr args))))))
	  ((kt) (eq? (loop (car args)) #t))
	  ((kf) (eq? (loop (car args)) #f))
	  ((bnd?) (if (assq (car args) (pinfo-bindings pinfo))
		      #t
		      texpr))
	  ((quote) texpr)
	  ((exec)
	   (loop (apply (car args) (cdr args))))
	  ((anyk) ; tst=k o texpr
	   (if (null?
		(filter
		 (lambda (x) (eq? x #t))
		 (mapfilter
		  (lambda (t)
		    (and (eq? (car t) (car args))
			 (equal? (cadr t) (loop (cadr args)))
			 (eq? (texpr-predict! (caddr args)
					      (pinfo+binding pinfo
							     (cons '$anyk
								   (caddr t))))
			      #t)))
		  (pinfo-truths pinfo))))
	       #f
	       #t))
	  (else
	   (if (member texpr (pinfo-unknowns pinfo))
	       texpr
	       (let ((newt (cons app (map loop args))))
		 (if (memq app tst-list)
		     (cond
		      ((member newt (pinfo-unknowns pinfo)) newt)
		      ((member newt (pinfo-truths pinfo)) #t)
		      ((member newt (pinfo-lies pinfo)) #f)
		      (else
		       (pinfo+unknown! pinfo newt)
		       (loop (test-expand newt))))
		     (begin
		       (pinfo+unknown! pinfo newt)
		       newt))))))))
     ((symbol? texpr)
      (let ((vv (assq texpr (pinfo-bindings pinfo))))
	(if vv (cdr vv) texpr)))
     (else texpr))))

;; Reduce a texpr according to pinfo
;; toplevel kt, kf, if3 and anyk are not reduced
;; if their results could be different with more information
(define (texpr-reduce! texpr pinfo)
  (let loop-reduce ((texpr texpr))
    ;(pp (vector 'redu texpr)); (pinfo-unknowns pinfo)))
    (cond
     ((pair? texpr)
      (let ((app (car texpr)) (args (cdr texpr)))
	(case app
	  ((one)
	   (let ((texpr1 (loop-reduce (car args))))
	     (if (eq? texpr1 #t)
		 texpr1
		 (make-one texpr1 (loop-reduce (cadr args))))))
	  ((or)
	   (let ((texpr1 (loop-reduce (car args))))
	     (if (eq? texpr1 #t)
		 texpr1
		 (make-or texpr1 (loop-reduce (cadr args))))))
	  ((all)
	   (let ((texpr1 (loop-reduce (car args))))
	     (and texpr1
		  (make-all texpr1 (loop-reduce (cadr args))))))
	  ((and)
	   (let ((texpr1 (loop-reduce (car args))))
	     (and texpr1
		  (make-and texpr1 (loop-reduce (cadr args))))))
	  ((not) (make-not (loop-reduce (car args))))
	  ((if3)
	   (let ((texpr (loop-reduce (car args))))
	     (case texpr
	       ((#t) (loop-reduce (cadr args)))
	       ((#f) (loop-reduce (caddr args)))
	       (else (cons 'if3 (cons texpr (cdr args)))))))
	  ((kt)
	   (let ((texpr (loop-reduce (car args))))
	     (if (boolean? texpr)
		 texpr
		 (list 'kt texpr))))
	  ((kf)
	   (let ((texpr (loop-reduce (car args))))
	     (if (boolean? texpr)
		 (not texpr)
		 (list 'kf texpr))))
	  ((exec) texpr)
	  ((anyk) ; tst=k o texpr
	   (if (null?
		(filter
		 (lambda (x) (eq? x #t))
		 (mapfilter
		  (lambda (t)
		    (and (eq? (car t) (car args))
			 (equal? (cadr t) (texpr-predict! (cadr args) pinfo))
			 (eq? (texpr-reduce! (caddr args)
					     (pinfo+binding pinfo
							    (cons '$anyk
								  (caddr t))))
			      #t)))
		  (pinfo-truths pinfo))))
	       texpr
	       #t))
	  ((bnd?)
	   (if (assq (car args) (pinfo-bindings pinfo))
	       #t
	       texpr))
	  ((quote) texpr)
	  (else
	   (if (memq app tst-list)
	       (let ((newt (cons app (map (lambda (t)
					    (texpr-predict! t pinfo))
					  args))))
		 (cond
		  ((member newt (pinfo-unknowns pinfo)) newt)
		  ((member newt (pinfo-truths pinfo)) #t)
		  ((member newt (pinfo-lies pinfo)) #f)
		  (else
		   (pinfo+unknown! pinfo newt)
		   (texpr-predict! (test-expand newt) pinfo))))
	       texpr)))))
     ((symbol? texpr)
      (let ((vv (assq texpr (pinfo-bindings pinfo))))
	(if vv (cdr vv) texpr)))
     (else texpr))))

;; fetch bindings from texpr
;; Bindings may only appear in patterns,
;; having only 'and' and 'all' texpr types...
(define (texpr->bindings texpr)
  (if (pair? texpr)
      (let ((app (car texpr)) (args (cdr texpr)))
	(case app
	  ((all) (append (texpr->bindings (car args))
			 (texpr->bindings (cadr args))))
	  ((and) (texpr->bindings (car args)))
	  ((bind) (list (cons (car args) (cadr args))))
	  (else '())))
      '()))

;; remove accessible bindings 
(define (texpr-remove-bindings texpr)
  (if (pair? texpr)
      (let ((app (car texpr)) (args (cdr texpr)))
	(case app
	  ((all) (make-all (texpr-remove-bindings (car args))
			   (texpr-remove-bindings (cadr args))))
	  ((and)
	   (let ((texpr (texpr-remove-bindings (car args))))
	     (if (eq? texpr #t)
		 (texpr-remove-bindings (cadr args))
		 (make-and texpr (cadr args)))))
	  ((bind) #t)
	  (else texpr)))
      texpr))

(define (texpr->tests! texpr pinfo)
  (texpr->tests-aux (texpr-predict! texpr pinfo)))

(define (texpr->tests-aux texpr)
  (if (pair? texpr)
      (let ((app (car texpr)) (args (cdr texpr)))
	(case app
	  ((all one) (append (texpr->tests-aux (car args))
			     (texpr->tests-aux (cadr args))))
	  ((and or not) (texpr->tests-aux (car args)))
	  ((bnd?) '())
	  (else (list texpr))))
      '()))


;; Constant static prediction
(define (static-predict-type-k tst k)
  (case tst
    ((erl-sub?) (and (erl-sub? k) (or (erl-big? k) (erl-flo? k))))
    ((erl-spc?) (erl-spc? k))
    ((erl-fix?) (erl-fix? k))
    ((erl-int?) (or (erl-fixnum? k) (erl-bignum? k)))
    ((erl-num?) (or (erl-fixnum? k) (erl-bignum? k) (erl-flonum? k)))
    ((erl-nil?) (erl-nil? k))
    ((erl-con?) #f)
    ((erl-big?) (erl-bignum? k))
    ((erl-flo?) (erl-flonum? k))
    ((erl-ato?) #f)
    ((erl-vec?) #f)
    ((erl-chr?) (erl-chr? k))
    ((erl-lst?) (erl-nil? k))
    ((erl-str?) (erl-nil? k))
    (else (internal-error!!! "static-predict-type-k, test: " tst))))

(define (static-predict-val-k tst k val)
  (case tst
    ((erl-fix=k erl-ato=k erl-chr=k) (eq? k val))
    ((erl-flo=k erl-big=k) (= k val))
    (else (internal-error!!! "static-predict-val-k, test: " tst))))


(define (constructor-type constructor)
  (case constructor
    ((erl-hd erl-tl erl-vector-ref) 'obj)
    ((erl-vector-length) 'fix)
    ((erl-cons) 'con)
    ((erl-tuple) 'vec)
    ((quote) 'obj)
    ((erl-safe-nodes/1) 'lst)
    ((self/0) 'vec)
    ((erl-safe-abs/1) 'num)
    ((erl-safe-float/1) 'flo)
    ((fix+/2) 'fix)
    ((erl-safe-length/1) 'fix)
    ((erl-safe-node/1) 'ato)
    ((erl-safe-sign/1) 'fix)
    ((erl-safe-round/1) 'int)
    ((erl-safe-size/1) 'fix)
    ((erl-safe-trunc/1) 'int)
    ((erl-safe-+/2) 'num)
    ((erl-safe--/2) 'num)
    ((erl-safe-bor/2) 'int)
    ((erl-safe-bxor/2) 'int)
    ((erl-safe-bsl/2) 'int)
    ((erl-safe-bsr/2) 'int)
    ((erl-safe-*/2) 'num)
    ((erl-safe-//2) 'num)
    ((erl-safe-///2) 'int)
    ((erl-safe-div/2) 'int)
    ((erl-safe-mod/2) 'int)
    ((erl-safe-rem/2) 'int)
    ((erl-safe-band/2) 'int)
    ((erl-safe--/1) 'num)
    ((erl-safe-bnot/1) 'int)
    ((erl-safe-not/1) 'ato)
    (else (internal-error!!! "constructor-type, unknown: " constructor))))
    
(define (static-predict-type-app tst app args)
  (let ((ctype (constructor-type app)))
    (case tst
      ((erl-sub?) (or (memq ctype '(big flo ato vec))
		      (and (memq ctype '(obj num)) 'unknown)))
      ((erl-spc?) (or (memq ctype '(chr nil))
		      (and (memq ctype '(obj lst str)) 'unknown)))
      ((erl-fix?) (or (eq? ctype 'fix)
		      (and (memq ctype '(obj num int)) 'unknown)))
      ((erl-con?) (or (eq? ctype 'con)
		      (and (eq? ctype 'obj) 'unknown)))
      ((erl-big?) (or (eq? ctype 'big)
		      (and (memq ctype '(obj sub num int)) 'unknown)))
      ((erl-flo?) (or (eq? ctype 'flo)
		      (and (memq ctype '(obj sub num)) 'unknown)))
      ((erl-ato?) (or (eq? ctype 'ato)
		      (and (memq ctype '(obj sub)) 'unknown)))
      ((erl-vec?) (or (eq? ctype 'vec)
		      (and (memq ctype '(obj sub)) 'unknown)))
      ((erl-chr?) (or (eq? ctype 'chr)
		      (and (memq ctype '(obj spc)) 'unknown)))
      ((erl-nil?) (or (eq? ctype 'nil)
		      (and (memq ctype '(obj spc lst)) 'unknown)))
      ((erl-lst?) (or (memq ctype '(nil lst))
		      (and (eq? app 'cons)
			   (static-predict-type 'erl-lst? (cadr args)))
		      (and (memq ctype '(obj spc con)) 'unknown)))
      ((erl-str?) (or (memq ctype '(nil str))
		      (and (eq? app 'cons)
			   (eq? (static-predict-type 'erl-chr? (car args)) #t)
			   (static-predict-type 'erl-str? (cadr args)))
		      (and (memq ctype '(obj spc con lst)) 'unknown)))
      ((erl-int?) (or (memq ctype '(fix big int))
		      (and (memq ctype '(obj sub num)) 'unknown)))
      ((erl-num?) (or (memq ctype '(fix big int flo num))
		      (and (memq ctype '(obj sub)) 'unknown)))
      (else (internal-error!!! "static-predict-type-app")))))

(define (static-predict-val-app tst val app args)
  (let ((ctype (constructor-type app)))
    (case tst
      ((erl-fix=k) (and (memq ctype '(obj fix int num)) 'unknown))
      ((erl-big=k) (and (memq ctype '(obj sub big int num)) 'unknown))
      ((erl-flo=k) (and (memq ctype '(obj sub flo num)) 'unknown))
      ((erl-ato=k) (and (memq ctype '(obj sub ato)) 'unknown))
      ((erl-chr=k) (and (memq ctype '(obj spc chr)) 'unknown))
      (else (internal-error!!! "static-predict-val-app")))))

;; Static prediction, returns #t, #f or 'unknown
(define (static-predict-type tst o)
  (cond
   ((symbol? o) 'unknown) ; nothing can be said about a variable
   ((const? o) (static-predict-type-k tst o))
   (else (static-predict-type-app tst (car o) (cdr o)))))

(define (static-predict-val tst val o)
  (cond
   ((symbol? o) 'unknown)
   ((const? o) (static-predict-val-k tst val (unquote-const o)))
   (else (static-predict-val-app tst val (car o) (cdr o)))))



;; Expands an unary test given dependance-tree information
(define (class-expand o tst ktst parent children brothers)

  (define (one-of tsts)
    (if (null? (cdr tsts))
	(list (car tsts) o)
	(list 'one 
	      (list (car tsts) o)
	      (one-of (cdr tsts)))))
  
  (let ((sp (static-predict-type tst o)))
    (if (boolean? sp)
	sp
	;; brotest induce too much calculation for rare gain
	(let* ((brotest (list tst o));(make-if3 (one-of brothers)
				  ;#f
				  ;#t
				  ;(list tst o)))
	       (partest (if parent
			    (make-if3 (list parent o)
				      brotest
				      #f
				      (list tst o))
			    brotest))
	       (chitest (if children
			    (make-or (make-kt (one-of children))
				     partest)
			    partest)))
	  (if ktst
	      (make-or (list 'anyk ktst o (list ktst o '$anyk))
		       chitest)
	      chitest)))))

;; expand "unary" constant tests, given info
(define (tst=k-expand o k tst parent k=)
  (let ((sp (static-predict-val tst k o)))
    (if (boolean? sp)
	sp
	(make-and (make-not (make-kf (list parent o)))
		  (make-and
		   (make-not (make-anyk tst 
					o
					(make-exec (lambda (x) (not (k= x k)))
						   '$anyk)))
		   (list tst o k))))))

(define (test-expand test)
  (let ((type (car test)) (o (cadr test)))
    (case type
      ((erl-sub?)
       (class-expand o 'erl-sub? #f #f '(erl-big? erl-flo? erl-ato? erl-vec?)
		     '(erl-spc? erl-fix? erl-con?)))
      ((erl-spc?)
       (class-expand o 'erl-spc? #f #f '(erl-chr? erl-nil?)
		     '(erl-sub? erl-fix? erl-con?)))
      ((erl-fix?)
       (class-expand o 'erl-fix? 'erl-fix=k #f #f
		     '(erl-sub? erl-spc? erl-con?)))
      ((erl-con?)
       (class-expand o 'erl-con? #f #f #f
		     '(erl-sub? erl-spc? erl-fix?)))
      ((erl-big?)
       (class-expand o 'erl-big? 'erl-big=k 'erl-sub? #f
		     '(erl-flo? erl-ato? erl-vec?)))
      ((erl-flo?)
       (class-expand o 'erl-flo? 'erl-flo=k 'erl-sub? #f
		     '(erl-big? erl-ato? erl-vec?)))
      ((erl-ato?)
       (class-expand o 'erl-ato? 'erl-ato=k 'erl-sub? #f
		     '(erl-big? erl-flo? erl-vec?)))
      ((erl-vec?)
       (class-expand o 'erl-vec? #f 'erl-sub? #f
		     '(erl-big? erl-flo? erl-ato?)))
      ((erl-chr?)
       (class-expand o 'erl-chr? 'erl-chr=k 'erl-spc? #f
		     '(erl-nil?)))
      ((erl-nil?)
       (class-expand o 'erl-nil? #f 'erl-spc? #f
		     '(erl-chr?)))
      ((erl-big=k erl-flo=k erl-ato=k erl-chr=k erl-fix=k)
       (let ((k (caddr test)))
	 (case type
	   ((erl-big=k) (tst=k-expand o k 'erl-big=k 'erl-big? =))
	   ((erl-flo=k) (tst=k-expand o k 'erl-flo=k 'erl-flo? =))
	   ((erl-ato=k) (tst=k-expand o k 'erl-ato=k 'erl-ato? eq?))
	   ((erl-chr=k) (tst=k-expand o k 'erl-chr=k 'erl-chr? eq?))
	   ((erl-fix=k) (tst=k-expand o k 'erl-fix=k 'erl-fix? eq?)))))
      ((erl-lst?) (let ((sp (static-predict-type type o)))
		    (if (boolean? sp) 
			sp
			(make-if3 (list 'erl-con? o)
				  (list 'erl-lst? (list 'erl-tl o))
				  (list 'erl-nil? o)
				  (make-if3 (list 'erl-nil? o)
					    #t
					    test
					    test)))))
      ((erl-str?)
       (let ((sp (static-predict-type type o)))
	 (if (boolean? sp)
	     sp
	     (make-if3 (list 'erl-con? o)
		       (make-if3 (make-and (list 'erl-spc? (list 'erl-hd o))
					   (list 'erl-chr? (list 'erl-hd o)))
				 (list 'erl-str? (list 'erl-tl o))
				 #f
				 test)
		       (list 'erl-nil? o)
		       (make-if3 (list 'erl-nil? o) #t test test)))))
      ((erl-int?) (let ((sp (static-predict-type type o)))
		    (if (boolean? sp)
			sp
			(make-one (list 'erl-fix? o)
				  (make-and (list 'erl-sub? o)
					    (list 'erl-big? o))))))
      ((erl-num?) (let ((sp (static-predict-type type o)))
		    (if (boolean? sp)
			sp
			(make-one (list 'erl-fix? o)
				  (make-and (list 'erl-sub? o)
					    (make-one (list 'erl-big? o)
						      (list 'erl-flo? o)))))))
      ((erl-<)
       (let ((o2 (caddr test)))
	 (make-or
	  (make-anyk 'erl-< o (list 'erl-< '$anyk o2))
	  test)))
      ((erl-=:=)
       (let ((o2 (caddr test)))
	 (cond
	  ((const? o) (pat->texpr (newast-const o) o2 #f))
	  ((const? o2) (pat->texpr (newast-const o2) o #f))
	  (else
	   (make-and
	    (make-not (make-kt (make-one (list 'erl-< o o2)
					 (list 'erl-< o2 o))))
	    (make-or
	     (make-or (make-kt (list 'erl-=:= o2 o))
		      (make-anyk 'erl-=:= o (list 'erl-=:= '$anyk o2)))
	     test))))))
      ((erl-==)
       (let ((o2 (caddr test)))
	 (make-and
	  (make-not (make-kt (make-one (list 'erl-< o o2)
				       (list 'erl-< o2 o))))
	  (make-or
	   (make-or (make-kt (list 'erl-== o2 o))
		    (make-anyk 'erl-== o (list 'erl-== '$anyk o2)))
	   test))))
      (else test))))

