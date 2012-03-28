; file: "pm-build.scm"

; Copyright (C) 1998-1999 Universite de Montreal, All Rights Reserved.

; Pattern match compilation

(include "pm-build.scm")
(include "pm-predict.scm")

;; updates the bindings of a cinfo using bd-texpr and pinfo
;; also reduce the ast according to information
;; returns the fully updated cinfo
(define (cinfo-update cinfo truths lies)
  (let* ((pinfo (make-pinfo truths lies '() (cinfo-bindings cinfo)))
	 (bd-texpr (texpr-reduce! (cinfo-bd-texpr cinfo) pinfo))
	 (newbindings (texpr->bindings bd-texpr)))
    (if (null? newbindings)
	(cinfo-update-pg-texpr
	 cinfo
	 (texpr-reduce! (cinfo-pg-texpr cinfo) pinfo))
	(cinfo-update
	 (let ((newcinfo (cinfo-add-bindings cinfo newbindings)))
	   (cinfo-update-bd-texpr
	    newcinfo
	    (texpr-reduce! (texpr-remove-bindings bd-texpr)
			   (make-pinfo (pinfo-truths pinfo)
				       (pinfo-lies pinfo)
				       (pinfo-unknowns pinfo)
;				       '() ;unknowns may be resolved(?)
				       (cinfo-bindings newcinfo)))))
	 truths
	 lies))))

(define (cinfos-update cinfos truths lies)
  (map (lambda (cinfo) (cinfo-update cinfo truths lies))
       cinfos))

;; returns a list of possible tests that can be done from cinfo
(define (cinfo->tests cinfo truths lies)
  (texpr->tests! (cinfo-pg-texpr cinfo)
		 (make-pinfo truths
			     lies
			     '()
			     (cinfo-bindings cinfo))))

(define (cinfos->tests cinfos truths lies)
  (apply union
	 (map (lambda (cinfo) (cinfo->tests cinfo truths lies))
	      cinfos)))

;; Algorithm to choose a test, here is sestoft(naive with info booking)
(define (choose-test tests cinfos truths lies)
  (car tests))


(define test-thunks #f)
(define (reset-test-thunks!) (set! test-thunks '()))
(define (testcode->code! testcode)
  (let ((found (assoc testcode test-thunks)))
    (if found
	(gen-apply (cdr found))
	(let ((newthunk (gensymbol 'tst)))
	  (set! test-thunks
		(cons (cons testcode newthunk)
		      test-thunks))
	  (gen-apply newthunk)))))
;; May return #f to indicate a contradiction
;; (this implies that the result of the test leading there is erroneous)
(define (compile-cinfos-aux cinfos truths lies)
;  (pp cinfos)
  (if (null? cinfos)
      #f
      (let ((tests (cinfo->tests (car cinfos) truths lies))
	    (cinfo (car cinfos)))
;	(pp (vector 'cinfo cinfo 'tests tests))
;	(pp 'hello)
	(if (null? tests)
	    (case (texpr-predict! (cinfo-pg-texpr cinfo)
				  (make-pinfo truths
					      lies
					      '()
					      (cinfo-bindings cinfo)))
	      ((#t)
	       (apply gen-apply
		      (cons (cinfo-bodycode cinfo)
			    (map (lambda (v)
				   (cdr (assq v (cinfo-bindings cinfo))))
				 (cinfo-bodyneed cinfo)))))
	      (else
	       (compile-cinfos-aux (cdr cinfos) truths lies)))
	    (let ((test (choose-test tests cinfos truths lies)))
	      (let ((truepart
		     (compile-cinfos-aux (cinfos-update cinfos
							(cons test truths)
							lies)
					 (cons test truths)
					 lies))
		    (falsepart
		     (compile-cinfos-aux (cinfos-update cinfos
							truths
							(cons test lies))
					 truths
					 (cons test lies))))
		(cond
		 ((and truepart falsepart)
		  (testcode->code! (gen-if test truepart falsepart)))
		 (truepart truepart)
		 (falsepart falsepart)
		 (else
		  (internal-error!!!
		   "compile-cinfos-aux: can't be matched!!!")))))))))


(define (compile-cinfos cinfos)
  (reset-test-thunks!)
;  (pp cinfos)
  (let ((bodiesnames (genvarlist-symbol "b" (length cinfos) 1)))
    (let ((code (compile-cinfos-aux
		 (cinfos-update (map (lambda (cinfo bn)
				       (cinfo-update-bodycode cinfo bn))
				     cinfos
				     bodiesnames)
				'()
				'())
		 '()
		 '())))
    (apply
     gen-apply
     (cons (gen-lambda
	    bodiesnames
	    (let loop ((tts (reverse test-thunks)))
	      (if (null? tts)
		  code
		  (let ((tt (car tts)))
		    (gen-apply (gen-lambda (list (cdr tt))
					   (loop (cdr tts)))
			       (gen-lambda '()
					   (car tt)))))))
	   (map (lambda (cinfo)
		  (gen-lambda (cinfo-bodyneed cinfo)
			      (cinfo-bodycode cinfo)))
		cinfos))))))

		     




