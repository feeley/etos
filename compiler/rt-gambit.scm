; File: "rt-gambit.scm"

; Copyright (C) 1999, Marc Feeley, Patrick Piche, All Rights Reserved.

; RunTime library for EtoS - Gambit version

(declare
 (standard-bindings)
 (extended-bindings)
 (block)
 (not safe)
 (inlining-limit 900)
; (inlining-limit 300)
)

(##define-macro (gensymbol . rest)
  `(string->symbol (string-append "$" (symbol->string (gensym ,@rest)))))

(##define-macro (continuation-save! v proc)
  `(let ((v ,v) (proc ,proc))
     (continuation-capture
      (lambda (cont)
        (vector-set! v 0 cont)
        (proc v)))))

(##define-macro (continuation-restore v val)
  `(continuation-return (vector-ref ,v 0) ,val))

(##define-macro (erl-fix? x) `(##fixnum? ,x))
(##define-macro (erl-sub? x) `(##subtyped? ,x))
(##define-macro (erl-spc? x) `(##special? ,x))
(##define-macro (erl-con? x) `(##pair? ,x))

; We can assume here that (erl-sub? x) is true...
(##define-macro (erl-big? x) `(##subtyped.bignum? ,x))
(##define-macro (erl-flo? x) `(##subtyped.flonum? ,x))
(##define-macro (erl-ato? x) `(##subtyped.symbol? ,x))
(##define-macro (erl-vec? x) `(##subtyped.vector? ,x))

(##define-macro (erl-chr? x) `(##char? ,x))
(##define-macro (erl-nil? x) `(##null? ,x))

(##define-macro (erl-int? x)
  `(let ((a ,x)) (or (erl-fix? a) (and (erl-sub? a) (erl-big? a)))))
(##define-macro (erl-num? x)
  `(let ((a ,x)) (or (erl-fix? a)
		     (and (erl-sub? a) (or (erl-flo? a) (erl-big? a))))))

(##define-macro (big.= x y) `(##bignum.= ,x ,y))
(##define-macro (erl-big=k x k) `(big.= ,x ,k))
(##define-macro (flo.= x y) `(##flonum.= ,x ,y))
(##define-macro (erl-flo=k x k) `(flo.= ,x ,k))
(##define-macro (fix.= x y) `(##fixnum.= ,x ,y))
(##define-macro (erl-fix=k x k) `(fix.= ,x ,k))
(##define-macro (ato.= x y) `(eq? ,x ,y))
(##define-macro (erl-ato=k x k) `(ato.= ,x ,k))
(##define-macro (chr.= x y) `(eq? ,x ,y))
(##define-macro (erl-chr=k x k) `(chr.= ,x ,k))
(##define-macro (num.= x y) `(= ,x ,y))

(##define-macro (fix.< x y) `(##fixnum.< ,x ,y))
(##define-macro (big.< x y) `(##bignum.< ,x ,y))
(##define-macro (flo.< x y) `(##flonum.< ,x ,y))
(##define-macro (num.< x y) `(< ,x ,y))

(##define-macro (fix.u+ x y) `(##fixnum.+ ,x ,y))
(##define-macro (big.+ x y) `(##bignum.+ ,x ,y))
(##define-macro (flo.+ x y) `(##flonum.+ ,x ,y))
(##define-macro (num.+ x y) `(+ ,x ,y))

(##define-macro (fix.u- x y) `(##fixnum.- ,x ,y))
(##define-macro (big.- x y) `(##bignum.- ,x ,y))
(##define-macro (flo.- x y) `(##flonum.- ,x ,y))
(##define-macro (num.- x y) `(- ,x ,y))

(##define-macro (fix.bor x y) `(##fixnum.bitwise-ior ,x ,y))
(##define-macro (fix.bxor x y) `(##fixnum.bitwise-xor ,x ,y))
(##define-macro (fix.band x y) `(##fixnum.bitwise-and ,x ,y))
(##define-macro (fix.bnot x) `(##fixnum.bitwise-not ,x))

(##define-macro (fix.u* x y) `(##fixnum.* ,x ,y))
(##define-macro (big.* x y) `(##bignum.* ,x ,y))
(##define-macro (flo.* x y) `(##flonum.* ,x ,y))
(##define-macro (num.* x y) `(* ,x ,y))

(##define-macro (flo./ x y) `(##flonum./ ,x ,y))

(##define-macro (fix.div x y) `(##fixnum.quotient ,x ,y))
(##define-macro (big.div x y) `(##bignum.quotient ,x ,y))

(##define-macro (fix.rem x y) `(##fixnum.remainder ,x ,y))
(##define-macro (big.rem x y) `(##bignum.remainder ,x ,y))

(##define-macro (fix.mod x y) `(##fixnum.modulo ,x ,y))
(##define-macro (big.mod x y) `(##bignum.modulo ,x ,y))

(##define-macro (fix.even? x) `(##fixnum.even? ,x))

;; Full type tests
(##define-macro (erl-fixnum? x) `(##fixnum? ,x))
(##define-macro (erl-bignum? x) `(##bignum? ,x))
(##define-macro (erl-flonum? x) `(##flonum? ,x))
(##define-macro (erl-atom? x) `(##symbol? ,x))
(##define-macro (erl-byte? x)
  `(let ((a ,x))
     (and (erl-fix? a) (fix.< a 256) (fix.< -1 a))))
(##define-macro (erl-boolean? x)
  `(let ((a ,x)) (or (erl-ato=k a 'true) (erl-ato=k a 'false))))
(##define-macro (erl-cons? x) `(erl-con? ,x))
(##define-macro (erl-char? x) `(##char? ,x))

;; Longer arithmetic macros
(##define-macro (if-fix? x y z) (if (##fixnum? x) y z))
(##define-macro (if-int? x y z) (if (or (##fixnum? x) (##bignum? x)) y z))
(##define-macro (if-zero-fix? x y z) (if (and (##fixnum? x) (= x 0)) y z))
(##define-macro (if-pos-fix? x y z) (if (and (##fixnum? x) (> x 0)) y z))
(##define-macro (if-neg-fix? x y z) (if (and (##fixnum? x) (< x 0)) y z))
(##define-macro (if-non-neg-fix? x y z) (if (and (##fixnum? x) (>= x 0)) y z))

(##define-macro (fixnum-specialized-=:= x y general-case)
  `(if-fix? ,x
     (fix.= ,x ,y)
     (if-fix? ,y
       (fix.= ,x ,y)
       (let ((a ,x) (b ,y))
         (if (or (erl-fix? a) (erl-fix? b))
	     (fix.= a b)
	     (,general-case a b))))))

(##define-macro (fixnum-specialized-< x y general-case)
  `(if-fix? ,x
     (let ((b ,y))
       (if (erl-fix? b) (fix.< ,x b) (,general-case ,x b)))
     (if-fix? ,y
       (let ((a ,x))
         (if (erl-fix? a) (fix.< a ,y) (,general-case a ,y)))
       (let ((a ,x) (b ,y))
         (if (and (erl-fix? a) (erl-fix? b))
	     (fix.< a b)
	     (,general-case a b))))))

(##define-macro (fixnum-specialized-+ x y general-case)
  `(if-zero-fix? ,x
     (let ((b ,y))
       (if (erl-fix? b)
         b
         (,general-case ,x b)))
     (if-pos-fix? ,x
       (let ((b ,y))
         (if (and (erl-fix? b) (fix.< b (fix.u+ ,x b)))
           (fix.u+ ,x b)
           (,general-case ,x b)))
       (if-neg-fix? ,x
         (let ((b ,y))
           (if (and (erl-fix? b) (fix.< (fix.u+ ,x b) b))
             (fix.u+ ,x b)
             (,general-case ,x b)))
         (if-zero-fix? ,y
           (let ((a ,x))
             (if (erl-fix? a)
               a
               (,general-case a ,y)))
           (if-pos-fix? ,y
             (let ((a ,x))
               (if (and (erl-fix? a) (fix.< a (fix.u+ a ,y)))
                 (fix.u+ a ,y)
                 (,general-case a ,y)))
             (if-neg-fix? ,y
               (let ((a ,x))
                 (if (and (erl-fix? a) (fix.< (fix.u+ a ,y) a))
                   (fix.u+ a ,y)
                   (,general-case a ,y)))
               (let ((a ,x) (b ,y))
                 (if (and (erl-fix? a)
			  (erl-fix? b)
			  (or (fix.< (fix.bxor a b) 0)
			      (not (fix.< (fix.bxor (fix.u+ a b) b) 0))))
	           (fix.u+ a b)
                   (,general-case a b))))))))))

(##define-macro (fixnum-specialized-- x y general-case)
  `(if-zero-fix? ,y
     (let ((a ,x))
       (if (erl-fix? a)
         a
         (,general-case a ,y)))
     (if-pos-fix? ,y
       (let ((a ,x))
         (if (and (erl-fix? a) (fix.< (fix.u- a ,y) a))
           (fix.u- a ,y)
           (,general-case a ,y)))
       (if-neg-fix? ,y
         (let ((a ,x))
           (if (and (erl-fix? a) (fix.< a (fix.u- a ,y)))
             (fix.u- a ,y)
             (,general-case a ,y)))
         (let ((a ,x) (b ,y))
           (if (and (erl-fix? a)
		    (erl-fix? b)
		    (or (not (fix.< (fix.bxor a b) 0))
			(fix.< (fix.bxor (fix.u- a b) b) 0)))
             (fix.u- a b)
             (,general-case a b)))))))

(##define-macro (fixnum-specialized-* x y general-case)
  `(,general-case ,x ,y))

(##define-macro (fixnum-specialized-div x y general-case)
  `(if-fix? ,y
     (let ((a ,x))
       (if (erl-fix? a)
	 (fix.div a ,y)
         (,general-case a ,y)))
     (let ((a ,x) (b ,y))
       (if (and (erl-fix? a) (erl-fix? b))
	   (fix.div a b)
	   (,general-case ,x ,y)))))

(##define-macro (fixnum-specialized-mod x y general-case)
  `(if-fix? ,y
     (let ((a ,x))
       (if (erl-fix? a)
         (fix.mod a ,y)
         (,general-case a ,y)))
     (let ((a ,x) (b ,y))
       (if (and (erl-fix? a) (erl-fix? b))
	   (fix.mod a b)
	   (,general-case ,x ,y)))))

(##define-macro (fixnum-specialized-rem x y general-case)
  `(if-fix? ,y
     (let ((a ,x))
       (if (erl-fix? a)
         (fix.rem a ,y)
         (,general-case a ,y)))
     (let ((a ,x) (b ,y))
       (if (and (erl-fix? a) (erl-fix? b))
	   (fix.rem a b)
	   (,general-case ,x ,y)))))

(##define-macro (fixnum-specialized-bor x y general-case)
  `(if-fix? ,x
     (let ((b ,y))
       (if (erl-fix? b)
	   (fix.bor ,x b)
	   (,general-case ,x b)))
     (if-fix? ,y
       (let ((a ,x))
	 (if (erl-fix? a)
	     (fix.bor a ,y)
	     (,general-case a ,y)))
       (let ((a ,x) (b ,y))
	 (if (and (erl-fix? a) (erl-fix? b))
	     (fix.bor a b)
	     (,general-case a b))))))

(##define-macro (fixnum-specialized-bxor x y general-case)
  `(if-fix? ,x
     (let ((b ,y))
       (if (erl-fix? b)
	   (fix.bxor ,x b)
	   (,general-case ,x b)))
     (if-fix? ,y
       (let ((a ,x))
	 (if (erl-fix? a)
	     (fix.bxor a ,y)
	     (,general-case a ,y)))
       (let ((a ,x) (b ,y))
	 (if (and (erl-fix? a) (erl-fix? b))
	     (fix.bxor a b)
	     (,general-case a b))))))

(##define-macro (fixnum-specialized-band x y general-case)
  `(if-fix? ,x
     (let ((b ,y))
       (if (erl-fix? b)
	   (fix.band ,x b)
	   (,general-case ,x b)))
     (if-fix? ,y
       (let ((a ,x))
	 (if (erl-fix? a)
	     (fix.band a ,y)
	     (,general-case a ,y)))
       (let ((a ,x) (b ,y))
	 (if (and (erl-fix? a) (erl-fix? b))
	     (fix.band a b)
	     (,general-case a b))))))

(##define-macro (fixnum-specialized-bsl x y general-case)
  `(if-zero-fix? ,y
     (let ((a ,x))
       (if (erl-fix? a)
	   a
	   (,general-case a ,y)))
     (,general-case ,x ,y)))

(##define-macro (fixnum-specialized-bsr x y general-case)
  `(if-zero-fix? ,y
     (let ((a ,x))
       (if (erl-fix? a)
	   a
	   (,general-case a ,y)))
     (,general-case ,x ,y)))
	     
	     

(##define-macro (number-specialized-=:= x y general-case)
  `(if-fix? ,x
     (eq? ,x ,y)
     (if-fix? ,y
       (eq? ,x ,y)
       (if-float? ,x
         (let ((b ,y))
           (if (erl-flonum? b) (flo.= ,x b) (,general-case ,x b)))
         (if-float? ,y
           (let ((a ,x))
             (if (erl-flonum? a) (flo.= a ,y) (,general-case a ,y)))
           (let ((a ,x) (b ,y))
             (cond ((or (erl-fix? a) (erl-fix? b))
                    (eq? a b))
                   ((and (erl-flonum? a) (erl-flonum? b))
                    (flo.= a b))
                   (else
                    (,general-case a b)))))))))

(##define-macro (number-specialized-== x y general-case)
  `(if-fix? ,x
     (let ((b ,y))
       (if (erl-fix? b) (fix.= ,x b) (,general-case ,x b)))
     (if-fix? ,y
       (let ((a ,x))
         (if (erl-fix? a) (fix.= a ,y) (,general-case a ,y)))
       (if-float? ,x
         (let ((b ,y))
           (if (erl-flonum? b) (flo.= ,x b) (,general-case ,x b)))
         (if-float? ,y
           (let ((a ,x))
             (if (erl-flonum? a) (flo.= a ,y) (,general-case a ,y)))
           (let ((a ,x) (b ,y))
             (cond ((erl-fix? a)
                    (if (erl-fix? b) (fix.= a b) (,general-case a b)))
                   ((erl-flonum? a)
                    (if (erl-flonum? b) (flo.= a b) (,general-case a b)))
                   (else
                    (,general-case a b)))))))))

(##define-macro (number-specialized-< x y general-case)
  `(if-fix? ,x
     (let ((b ,y))
       (if (erl-fix? b) (fix.< ,x b) (,general-case ,x b)))
     (if-fix? ,y
       (let ((a ,x))
         (if (erl-fix? a) (fix.< a ,y) (,general-case a ,y)))
       (if-float? ,x
         (let ((b ,y))
           (if (erl-flonum? b) (flo.< ,x b) (,general-case ,x b)))
         (if-float? ,y
           (let ((a ,x))
             (if (erl-flonum? a) (flo.< a ,y) (,general-case a ,y)))
           (let ((a ,x) (b ,y))
             (cond ((erl-fix? a)
                    (if (erl-fix? b) (fix.< a b) (,general-case a b)))
                   ((erl-flonum? a)
                    (if (erl-flonum? b) (flo.< a b) (,general-case a b)))
                   (else
                    (,general-case a b)))))))))

(##define-macro (number-specialized-+ x y general-case)
  `(if-zero-fix? ,x
     (let ((b ,y))
       (if (num? b)
         b
         (,general-case ,x b)))
     (if-pos-fix? ,x
       (let* ((b ,y) (res (fix.u+ ,x b)))
	 (if (and (erl-fix? b) (fix.< b res))
	     res
	     (,general-case ,x b)))
       (if-neg-fix? ,x
         (let* ((b ,y) (res (fix.u+ ,x b)))
           (if (and (erl-fix? b) (fix.< res b))
	       res
	       (,general-case ,x b)))
         (if-float? ,x
           (let ((b ,y))
             (if (erl-flonum? b)
		 (flo.+ ,x b)
		 (,general-case ,x b)))
           (if-zero-fix? ,y
             (let ((a ,x))
               (if (num? a)
                 a
                 (,general-case a ,y)))
             (if-pos-fix? ,y
               (let ((a ,x))
                 (if (and (erl-fix? a) (fix.< a (fix.u+ a ,y)))
                   (fix.u+ a ,y)
                   (,general-case a ,y)))
               (if-neg-fix? ,y
                 (let ((a ,x))
                   (if (and (erl-fix? a) (fix.< (fix.u+ a ,y) a))
                     (fix.u+ a ,y)
                     (,general-case a ,y)))
                 (if-float? ,y
                   (let ((a ,x))
                     (if (erl-flonum? a)
			 (flo.+ a ,y)
			 (,general-case a ,y)))
                   (let ((a ,x) (b ,y))
                     (cond ((erl-fix? a)
                            (if (and (erl-fix? b)
				     (or (fix.< (fix.bxor a b) 0)
					 (not (fix.< (fix.bxor (fix.u+ a b) b)
						     0))))
                              (fix.u+ a b)
                              (,general-case a b)))
                           ((erl-flonum? a)
                            (if (erl-flonum? b)
				(flo.+ a b)
				(,general-case a b)))
                           (else
                            (,general-case a b)))))))))))))

(##define-macro (number-specialized-- x y general-case)
  `(if-zero-fix? ,y
     (let ((a ,x))
       (if (erl-fix? a)
	   a
	   (,general-case a ,y)))
     (if-pos-fix? ,y
;       (let* ((a ,x) (res (fix.u- a ,y)))
;         (if (and (fix.< res a) (erl-fix? a))
;	     res
;	     (,general-case a ,y)))
        (let ((a ,x))
	  (if (and (erl-fix? a) (fix.< (fix.u- a ,y) a))
	      (fix.u- a ,y)
	      (,general-case a ,y)))
       (if-neg-fix? ,y
         (let* ((a ,x) (res (fix.u- a ,y)))
           (if (and (fix.< a res) (erl-fix? a))
	       res
	       (,general-case a ,y)))
         (if-float? ,y
           (let ((a ,x))
             (if (erl-flonum? a)
		 (flo.- a ,y)
		 (,general-case a ,y)))
           (let ((a ,x) (b ,y))
             (cond ((erl-fix? a)
		    (let ((res (fix.u- a b)))
		      (if (and (or (not (fix.< (fix.bxor a b) 0))
				   (fix.< (fix.bxor res b) 0))
			       (erl-fix? b))
			  res
			  (,general-case a b))))
		   ((erl-flonum? a)
                    (if (erl-flonum? a)
			(flo.- a b)
			(,general-case a b)))
                   (else
                    (,general-case a b)))))))))

(##define-macro (number-specialized-* x y general-case)
  `(if-int? ,x
     (,general-case ,x ,y)
     (if-int? ,y
       (,general-case ,x ,y)
       (if-float? ,x
         (let ((b ,y))
	   (if (erl-flonum? b)
	       (flo.* ,x b)
	       (,general-case ,x b)))
         (if-float? ,y
	   (let ((a ,x))
	     (if (erl-flonum? a)
		 (flo.* a ,y)
		 (,general-case a ,y)))
	   (let ((a ,x) (b ,y))
	     (if (and (erl-sub? a) (erl-sub? b) (erl-flo? a) (erl-flo? b))
		 (flo.* a b)
		 (,general-case a b))))))))

(##define-macro (number-specialized-/ x y general-case)
  `(if-non-zero-float? ,y
     (let ((a ,x))
       (if (and (erl-sub? a) (erl-flo? a))
	   (flo./ a ,y)
	   (,general-case a ,y)))
     (let ((a ,x) (b ,y))
       (if (and (erl-sub? a) (erl-sub? b) (erl-flo? a) (erl-flo? b) (not (flo.= b 0.0)))
         (flo./ a b)
         (,general-case a b)))))

(##define-macro (number-specialized-abs x general-case)
  `(if-non-neg-fix? ,x
     ,x
     (if-float? ,x
       (flo.abs ,x)
       (let ((a ,x))
	 (if (erl-flonum? a)
	     (flo.abs a)
	     (,general-case a))))))

(##define-macro (number-specialized-float x general-case)
  `(if-int? ,x
     (exact->inexact ,x)
     (if-float? ,x
       ,x
       (let ((a ,x))
	 (cond 
	  ((erl-fix? a) (exact->inexact a))
	  ((erl-flonum? a) a)
	  (else (,general-case a)))))))

(##define-macro (number-specialized-round x general-case)
  `(if-int? ,x
     ,x
     (if-float? ,x
       (inexact->exact (flo.round ,x))
       (let ((a ,x))
	 (cond
	  ((erl-flonum? a) (inexact->exact (flo.round a)))
	  ((erl-fix? a) a)
	  (else (,general-case a)))))))

(##define-macro (number-specialized-sign x general-case)
  `(if-non-neg-num? ,x
     0
     (if-neg-num? ,x
       1
       (let ((a ,x))
	 (cond
	  ((erl-fix? a) (if (fix.< a 0) 1 0))
	  ((erl-flonum? a) (if (flo.< a 0.0) 1 0))
	  (else (,general-case a)))))))

(##define-macro (number-specialized-trunc x general-case)
  `(if-int? ,x
     ,x
     (if-float? ,x
       (inexact->exact (flo.trunc ,x))
       (let ((a ,x))
	 (cond
	  ((erl-flonum? a) (inexact->exact (flo.trunc a)))
	  ((erl-fix? a) a)
	  (else (,general-case a)))))))

; chars
(##define-macro (chr.->integer x) `(char->integer ,x))

; integer arithmetic
(##define-macro (int.= x y) `(fixnum-specialized-=:= ,x ,y =))
(##define-macro (int.< x y) `(fixnum-specialized-< ,x ,y <))
(##define-macro (int.+ x y) `(fixnum-specialized-+ ,x ,y +))
(##define-macro (int.- x y) `(fixnum-specialized-- ,x ,y -))
(##define-macro (int.* x y) `(fixnum-specialized-* ,x ,y *))
(##define-macro (int.div x y) `(fixnum-specialized-div ,x ,y quotient))
(##define-macro (int.rem x y) `(fixnum-specialized-rem ,x ,y remainder))
(##define-macro (int.even? x) `(even? ,x))

; floating-point arithmetic
(##define-macro (if-float? x y z) (if (##flonum? x) y z))
(##define-macro (if-non-zero-float? x y z)
  (if (and (##flonum? x) (not (= x 0.0))) y z))
(##define-macro (if-non-neg-num? x y z)
  (if (and (number? x) (>= x 0)) y z))
(##define-macro (if-neg-num? x y z)
  (if (and (number? x) (< x 0)) y z))

(##define-macro (flo.abs x) `(##flonum.abs ,x))
(##define-macro (flo.acos x) `(##flonum.acos ,x))
(##define-macro (flo.acosh x)
  `(let ((a ,x))
     (flo.log (flo.+ a (flo.sqrt (flo.- (flo.* a a) 1.))))))
(##define-macro (flo.asin x) `(##flonum.asin ,x))
(##define-macro (flo.asinh x)
  `(let ((a ,x))
     (flo.log (flo.+ a (flo.sqrt (flo.+ (flo.* a a) 1.))))))
(##define-macro (flo.atan x) `(##flonum.atan ,x))
(##define-macro (flo.atan2 x y) `(##flonum.atan ,x ,y))
(##define-macro (flo.atanh x)
  `(let ((a ,x))
     (flo.* .5 (flo.log (flo./ (flo.+ 1. a) (flo.- 1. a))))))

(##define-macro (flo.cos x) `(##flonum.cos ,x))
(##define-macro (flo.cosh x)
  `(let ((a ,x))
     (flo./ (flo.- (flo.exp a) (flo.exp (flo.- 0. a))) 2.)))

(##define-macro (flo.erf x) `'not_implemented_yet)
(##define-macro (flo.erfc x) `'not_implemented_yet);`(flo.- 1. (flo.erf ,x)))

(##define-macro (flo.exp x) `(##flonum.exp ,x))
(##define-macro (flo.log x) `(##flonum.log ,x))
(##define-macro (flo.log10 x) `(flo./ (flo.log ,x) ,(##flonum.log 10.)))
(##define-macro (flo.pow x y) `(flo.exp (flo.* ,y (flo.log ,x))))
(##define-macro (flo.round x) `(##flonum.round ,x))
(##define-macro (flo.sin x) `(##flonum.sin ,x))
(##define-macro (flo.sinh x)
  `(let ((a ,x))
     (flo./ (flo.+ (flo.exp a) (flo.exp (flo.- 0. a))) 2.)))
(##define-macro (flo.sqrt x) `(##flonum.sqrt ,x))
(##define-macro (flo.tan x) `(##flonum.tan ,x))
(##define-macro (flo.tanh x)
  `(let ((a x))
     (let ((ea (flo.exp a)) (e-a (flo.exp (flo.- 0. a))))
       `(flo./ (flo.+ ea e-a) (flo.- ea e-a)))))
(##define-macro (flo.trunc x) `(##flonum.truncate ,x))

(##define-macro (erl-nil) ''())
(##define-macro (erl-cons x y) `(cons ,x ,y))
(##define-macro (erl-hd x) `(car ,x))
(##define-macro (erl-tl x) `(cdr ,x))
(##define-macro (erl-list . elems) `(list ,@elems))
(##define-macro (erl-append . lists) `(append ,@lists))
(##define-macro (erl-tuple . elems) `(vector 'tuple ,@elems))
(##define-macro (erl-tuple-size x) `(fix.u- (erl-vector-length ,x) 1))
(##define-macro (erl-tuple-ref x i) `(##vector-ref ,x ,i))

(##define-macro (erl-vector . elems) `(vector ,@elems))
(##define-macro (erl-vector-length v) `(##vector-length ,v))
(##define-macro (erl-vector-ref v i) `(##vector-ref ,v ,i))
(##define-macro (erl-vector-set! v i k) `(##vector-set! ,v ,i ,k))
(##define-macro (erl-make-vector n) `(make-vector ,n))

(##define-macro (erl-function arit lam) `(vector 'function ,arit ,lam))
(##define-macro (erl-function-arity f) `(vector-ref ,f 1))
(##define-macro (erl-function-lambda f) `(vector-ref ,f 2))

(##define-macro (erl-make-binary u8 off siz)
		`(vector 'binary ,u8 ,off ,siz))
(##define-macro (erl-u8vector->binary u8)
		`(let ((a ,u8))
		   (erl-make-binary a 0 (u8vector-length a))))
(##define-macro (erl-binary-u8vector x) `(vector-ref ,x 1))
(##define-macro (erl-binary-offset x) `(vector-ref ,x 2))
(##define-macro (erl-binary-size x) `(vector-ref ,x 3))


(##define-macro (erl-vector? x)
  `(let ((a ,x))
     (and (erl-sub? a) (erl-vec? a))))
(##define-macro (erl-tuple? x)
  `(let ((a ,x))
     (and (erl-sub? a) (erl-vec? a) (erl-ato=k (erl-vector-ref a 0) 'tuple))))
(##define-macro (erl-pid? x)
  `(let ((a ,x))
     (and (erl-sub? a) (erl-vec? a) (erl-ato=k (erl-vector-ref a 0) 'pid))))
(##define-macro (erl-port? x)
  `(let ((a ,x))
     (and (erl-sub? a) (erl-vec? a) (erl-ato=k (erl-vector-ref a 0) 'port))))
(##define-macro (erl-ref? x)
  `(let ((a ,x))
     (and (erl-sub? a) (erl-vec? a) (erl-ato=k (erl-vector-ref a 0) 'ref))))
(##define-macro (erl-binary? x)
  `(let ((a ,x))
     (and (erl-sub? a) (erl-vec? a) (erl-ato=k (erl-vector-ref a 0) 'binary))))
(##define-macro (erl-function? x)
  `(let ((a ,x))
     (and (erl-sub? a)
	  (erl-vec? a)
	  (erl-ato=k (erl-vector-ref a 0) 'function))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BIFS

;; abs/1 (Guard BIF)
(##define-macro (erl-tst-abs/1 x)
		`(number-specialized-abs ,x erl-generic-tst-abs/1))
(##define-macro (erl-safe-abs/1 x)
		`(number-specialized-abs ,x abs))
(##define-macro (erl-abs/1 x)
		`(number-specialized-abs ,x erl-generic-abs/1))

;; apply/3
(##define-macro (erl-apply/3 x y z) `(erlang:apply/3 ,x ,y ,z))

;; atom_to_list/1
(##define-macro (erl-atom_to_list/1 x) `(erlang:atom_to_list/1 ,x))

;; atom_to_string/1 (PROPOSED BIF)
(##define-macro (erl-atom_to_string/1 x) `(erlang:atom_to_string/1 ,x))

;; binary_to_list/1
(##define-macro (erl-binary_to_list/1 x) `(erlang:binary_to_list/1 ,x))

;; binary_to_list/3
(##define-macro (erl-binary_to_list/3 x y z)
  `(erlang:binary_to_list/3 ,x ,y ,z))

;; binary_to_string/1 (PROPOSED BIF)
(##define-macro (erl-binary_to_string/1 x) `(erlang:binary_to_string/1 ,x))

;; char_to_integer/1 (PROPOSED BIF)
(##define-macro (erl-char_to_integer/1 x) `(erlang:char_to_integer/1 ,x))

;; concat_binary/1
(##define-macro (erl-concat_binary/1 x) `(erlang:concat_binary/1 ,x))

;; date/0
(##define-macro (erl-date/0) `(erlang:date/0))

;; element/2 (Guard BIF)
(##define-macro (erl-tst-element/2 x y) `(erl-generic-tst-element/2 ,x ,y))
(##define-macro (erl-element/2 x y)
  `(let ((x ,x) (y ,y))
     (if (and (erl-fixnum? x)
              (erl-tuple? y))
         (if (and (fix.< 0 x)
                  (fix.< x (erl-vector-length y)))
             (erl-vector-ref y x)
             (erl-exit-badindex))
         (erl-exit-badarg))))

;; erase/0
(##define-macro (erl-erase/0) `(erlang:erase/0))

;; erase/1
(##define-macro (erl-erase/1 x) `(erlang:erase/1 x))

;; exit/1
(##define-macro (erl-exit/1 x) `(erlang:exit/1 ,x))

;; exit/2
(##define-macro (erl-exit/2 x y) `(erlang:exit/2 ,x ,y))

;; float/1 (Guard BIF)
(##define-macro (erl-tst-float/1 x)
		`(number-specialized-float ,x erl-generic-tst-float/1))
(##define-macro (erl-safe-float/1 x) `(exact->inexact ,x))
(##define-macro (erl-float/1 x)
		`(number-specialized-float ,x erl-generic-float/1))

;; float_to_list/1
(##define-macro (erl-float_to_list/1 x)
		`(erlang:float_to_list/1 ,x))

;; get/0
(##define-macro (erl-get/0) `(erlang:get/0))

;; get/1
(##define-macro (erl-get/1 x) `(erlang:get/1 ,x))

;; get_keys/1
(##define-macro (erl-get_keys/1 x) `(erlang:get_keys/1 x))

;; group_leader/0
(##define-macro (erl-group_leader/0)
		`(process-group-leader node.current-process))

;; group_leader/2
(##define-macro (erl-group_leader/2 x y) `(erlang:group_leader/2 ,x ,y))

;; hash/2
(##define-macro (erl-hash/2 x y) `(erlang:hash/2 ,x ,y))

;; hd/1 (Guard BIF)
(##define-macro (erl-hd/1 x) `(erl-generic-hd/1 ,x))

;; integer_to_char/1 (PROPOSED BIF)
(##define-macro (erl-integer_to_char/1 x)
		`(erlang:integer_to_char/1 ,x))

;; integer_to_list/1
(##define-macro (erl-integer_to_list/1 x)
		`(erlang:integer_to_list/1 ,x))

;; integer_to_string/1 (PROPOSED BIF)
(##define-macro (erl-integer_to_string/1 x)
		`(erlang:integer_to_string/1 ,x))

;; is_alive/0
(##define-macro (erl-is_alive/0) 'node.communicating)

;; is_atom/1 (Recognizer BIF)
(##define-macro (erl-is_atom/1 x) `(if (erl-atom? ,x) 'true 'false))
;; is_binary/1 (Recognizer BIF)
(##define-macro (erl-is_binary/1 x) `(if (erl-binary? ,x) 'true 'false))
;; is_char/1 (Recognizer BIF)
(##define-macro (erl-is_char/1 x) `(if (erl-chr? ,x) 'true 'false))
;; is_compound/1 (Recognizer BIF)
(##define-macro (erl-is_compound/1 x)
  `(let ((a ,x))
     (if (or (erl-nil? a) (erl-con? a) (erl-tuple? a))
	 'true
	 'false)))
;; is_cons/1 (Recognizer BIF)
(##define-macro (erl-is_cons/1 x) `(if (erl-con? ,x) 'true 'false))
;; is_float/1 (Recognizer BIF)
(##define-macro (erl-is_float/1 x) `(if (erl-flonum? ,x) 'true 'false))
;; is_function/1 (Recognizer BIF)
(##define-macro (erl-is_function/1 x) `(if (erl-function? ,x) 'true 'false))
;; is_integer/1 (Recognizer BIF)
(##define-macro (erl-is_integer/1 x) `(if (erl-int? ,x) 'true 'false))
;; is_list/1 (Recognizer BIF)
(##define-macro (erl-is_list/1 x) `(if (erl-lst? ,x) 'true 'false))
;; is_null/1 (Recognizer BIF)
(##define-macro (erl-is_null/1 x) `(if (erl-nil? ,x) 'true 'false))
;; is_number/1 (Recognizer BIF)
(##define-macro (erl-is_number/1 x) `(if (erl-num? ,x) 'true 'false))
;; is_pid/1 (Recognizer BIF)
(##define-macro (erl-is_pid/1 x) `(if (erl-pid? ,x) 'true 'false))
;; is_port/1 (Recognizer BIF)
(##define-macro (erl-is_port/1 x) `(if (erl-port? ,x) 'true 'false))
;; is_ref/1 (Recognizer BIF)
(##define-macro (erl-is_ref/1 x) `(if (erl-ref? ,x) 'true 'false))
;; is_string/1 (Recognizer BIF)
(##define-macro (erl-is_string/1 x) `(if (erl-str? ,x) 'true 'false))
;; is_tuple/1 (Recognizer BIF)
(##define-macro (erl-is_tuple/1 x) `(if (erl-tuple? ,x) 'true 'false))

;; length/1 (Guard BIF)
(##define-macro (erl-tst-length/1 x) `(erl-generic-tst-length/1 ,x))
(##define-macro (erl-safe-length/1 x) `(length ,x))
(##define-macro (erl-length/1 x) `(erlang:length/1 ,x))

;; link/1
(##define-macro (erl-link/1 x) `(erlang:link/1 ,x))

;; list_to_atom/1
(##define-macro (erl-list_to_atom/1 x) `(erlang:list_to_atom/1 ,x))

;; list_to_binary/1
(##define-macro (erl-safe-list_to_binary/1 x)
  `(erl-u8vector->binary (list->u8vector ,x)))
(##define-macro (erl-list_to_binary/1 x) `(erlang:list_to_binary/1 ,x))

;; list_to_float/1
(##define-macro (erl-list_to_float/1 x) `(erlang:list_to_float/1 ,x))

;; list_to_integer/1
(##define-macro (erl-list_to_integer/1 x) `(erlang:list_to_integer/1 ,x))

;; list_to_string/1 (PROPOSED BIF)
(##define-macro (erl-safe-list_to_string/1 x)
  `(map integer->char ,x))
(##define-macro (erl-list_to_string/1 x) `(erlang:list_to_string/1 ,x))

;; list_to_tuple/1
(##define-macro (erl-list_to_tuple/1 x) `(erlang:list_to_tuple/1 ,x))

;; make_ref/0
(##define-macro (erl-make_ref/0 x) `(erlang:make_ref/0 ,x))

;; node/0 (Guard BIF)
(##define-macro (erl-node/0)
		`(process-node node.current-process))

;; node/1 (Guard BIF)
(##define-macro (erl-tst-node/1 x) `(erl-generic-tst-node/1 ,x))
(##define-macro (erl-safe-node/1 x) `(erl-generic-safe-node/1 ,x))
(##define-macro (erl-node/1 x) `(erlang:node/1 ,x))

;; now/0
(##define-macro (erl-now/0)
  `(let* ((us (current-time-in-usecs))
	  (s (quotient us 1000000)))
     (erl-tuple (quotient s 1000000)
		(modulo s 1000000)
		(modulo us 1000000))))

;; open_port/2
(##define-macro (erl-open_port/2 x y) `(erlang:open_port/2 ,x ,y))

;; port_close/1
(##define-macro (erl-port_close/1 x) `(erl-generic-port_close/1 ,x))

;; port_info/1
(##define-macro (erl-port_info/1 x) `(erlang:port_info/1 ,x))

;; port_info/2
(##define-macro (erl-port_info/2 x y) `(erlang:port_info/2 ,x ,y))

;; ports/0
(##define-macro (erl-ports/0) `(erlang:ports/0))

;; process_info/2
(##define-macro (erl-process_info/2 x y) `(erlang:process_info/2 ,x ,y))

;; process_flag/2
(##define-macro (erl-process_flag/2 x y) `(erlang:process_flag/2 ,x ,y))

;; processes/0
(##define-macro (erl-processes/0) `(erlang:processes/0))

;; put/2
(##define-macro (erl-put/2 x y) `(erlang:put/2 ,x ,y))

;; register/2
(##define-macro (erl-register/2 x y) `(erlang:register/2 ,x ,y))

;; registered/0
(##define-macro (erl-registered/0) `(erlang:registered/0))

;; round/1 (Guard BIF)
(##define-macro (erl-tst-round/1 x)
		`(number-specialized-round ,x erl-generic-tst-round/1))
(##define-macro (erl-safe-round/1 x)
		`(number-specialized-round ,x round))
(##define-macro (erl-round/1 x)
		`(number-specialized-round ,x erl-generic-round/1))

;; self/0 (Guard BIF)
(##define-macro (erl-self/0) `(process-pid node.current-process))

;; setelement/3
(##define-macro (erl-setelement/3 x y z) `(erlang:setelement/3 ,x ,y ,z))

;; sign/1 (Guard BIF)
(##define-macro (erl-tst-sign/1 x)
		`(number-specialized-sign ,x erl-generic-tst-sign/1))
(##define-macro (erl-safe-sign/1 x)
		`(number-specialized-sign ,x erl-generic-safe-sign/1))
(##define-macro (erl-sign/1 x)
		`(number-specialized-sign ,x erl-generic-sign/1))

;; size/1 (Guard BIF)
(##define-macro (erl-tst-size/1 x) `(erl-generic-tst-size/1 ,x))
(##define-macro (erl-safe-size/1 x) `(erl-generic-safe-size/1 ,x))
(##define-macro (erl-size/1 x) `(erlang:size/1 ,x))

;; spawn/3
(##define-macro (erl-spawn/3 x y z) `(erlang:spawn/3 ,x ,y ,z))

;; spawn_link/3
(##define-macro (erl-spawn_link/3 x y z) `(erlang:spawn_link/3 ,x ,y ,z))

;; split_binary/2
(##define-macro (erl-split_binary/2 x y) `(erlang:split_binary/2 ,x ,y))

;; statistics/1
(##define-macro (erl-statistics/1 x) `(erlang:statistics/1 ,x))

;; string_to_list/1 (PROPOSED BIF)
(##define-macro (erl-string_to_list/1 x) `(erlang:string_to_list/1 ,x))

;; throw/1
(##define-macro (erl-throw/1 x) `(erlang:throw/1 ,x))

;; time/0
(##define-macro (erl-time/0) `(erlang:time/0))

;; tl/1 (Guard BIF)
(##define-macro (erl-tl/1 x) `(erl-generic-tl/1 ,x))

;; trunc/1 (Guard BIF)
(##define-macro (erl-tst-trunc/1 x)
		`(number-specialized-trunc ,x erl-generic-tst-trunc/1))
(##define-macro (erl-safe-trunc/1 x)
		`(number-specialized-trunc ,x truncate))
(##define-macro (erl-trunc/1 x)
		`(number-specialized-trunc ,x erl-generic-trunc/1))

;; tuple_to_list/1
(##define-macro (erl-tuple_to_list/1 x) `(erlang:tuple_to_list/1 ,x))

;; unlink/1
(##define-macro (erl-unlink/1 x) `(erlang:unlink/1 ,x))

;; unregister/1
(##define-macro (erl-unregister/1 x) `(erlang:unregister/1 ,x))

;; whereis/1
(##define-macro (erl-whereis/1 x) `(erlang:whereis/1 ,x))







(##define-macro (erl-=:= x y)
  `(number-specialized-=:= ,x ,y erl-generic-=:=))
(##define-macro (erl-== x y)
  `(number-specialized-== ,x ,y erl-generic-==))
(##define-macro (erl-< x y)
  `(number-specialized-< ,x ,y erl-generic-<))


;;;;;;;;;;;;;;;;;;;;
;; Operators 'BIFs'

;; +/1 (Guard BIF)
(##define-macro (erl-tst-+/1 x)
		`(number-specialized-+ 0 ,x erl-generic-tst-+/2))
(##define-macro (erl-safe-+/1 x)
		`(number-specialized-+ 0 ,x +))
(##define-macro (erl-+/1 x)
		`(number-specialized-+ 0 ,x erl-generic-+/2))

;; -/1 (Guard BIF)
(##define-macro (erl-tst--/1 x)
		`(number-specialized-- 0 ,x erl-generic-tst--/2))
(##define-macro (erl-safe--/1 x)
		`(number-specialized-- 0 ,x -))
(##define-macro (erl--/1 x)
		`(number-specialized-- 0 ,x erl-generic--/2))

;; bnot/1 (Guard BIF)
(##define-macro (erl-tst-bnot/1 x)
		`(fixnum-specialized-- -1 ,x erl-generic-tst--/2))
(##define-macro (erl-safe-bnot/1 x)
		`(fixnum-specialized-- -1 ,x -))
(##define-macro (erl-bnot/1 x)
		`(fixnum-specialized-- -1 ,x erl-generic--/2))

;; not/1 (Guard BIF)
(##define-macro (erl-tst-not/1 x) `(erl-generic-tst-not/1 ,x))
(##define-macro (erl-safe-not/1 x) `(erl-generic-safe-not/1 ,x))
(##define-macro (erl-not/1 x) `(erl-generic-not/1 ,x))
		
;; +/2 (Guard BIF)
(##define-macro (erl-tst-+/2 x y)
		`(number-specialized-+ ,x ,y erl-generic-tst-+/2))
(##define-macro (erl-safe-+/2 x y)
		`(number-specialized-+ ,x ,y +))
(##define-macro (erl-+/2 x y)
		`(number-specialized-+ ,x ,y erl-generic-+/2))

;; -/2 (Guard BIF)
(##define-macro (erl-tst--/2 x y)
		`(number-specialized-- ,x ,y erl-generic-tst--/2))
(##define-macro (erl-safe--/2 x y)
		`(number-specialized-- ,x ,y -))
(##define-macro (erl--/2 x y)
		`(number-specialized-- ,x ,y erl-generic--/2))

;; bor/2 (Guard BIF)
(##define-macro (erl-tst-bor/2 x y)
		`(fixnum-specialized-bor ,x ,y erl-generic-tst-bor/2))
(##define-macro (erl-safe-bor/2 x y)
		`(fixnum-specialized-bor ,x ,y int.bor))
(##define-macro (erl-bor/2 x y)
		`(fixnum-specialized-bor ,x ,y erl-generic-bor/2))

;; bxor/2 (Guard BIF)
(##define-macro (erl-tst-bxor/2 x y)
		`(fixnum-specialized-bxor ,x ,y erl-generic-tst-bxor/2))
(##define-macro (erl-safe-bxor/2 x y)
		`(fixnum-specialized-bxor ,x ,y int.bxor))
(##define-macro (erl-bxor/2 x y)
		`(fixnum-specialized-bxor ,x ,y erl-generic-bxor/2))

;; bsl/2 (Guard BIF)
(##define-macro (erl-tst-bsl/2 x y)
		`(fixnum-specialized-bsl ,x ,y erl-generic-tst-bsl/2))
(##define-macro (erl-safe-bsl/2 x y)
		`(fixnum-specialized-bsl ,x ,y int.bsl))
(##define-macro (erl-bsl/2 x y)
		`(fixnum-specialized-bsl ,x ,y erl-generic-bsl/2))

;; bsr/2 (Guard BIF)
(##define-macro (erl-tst-bsr/2 x y)
		`(fixnum-specialized-bsr ,x ,y erl-generic-tst-bsr/2))
(##define-macro (erl-safe-bsr/2 x y)
		`(fixnum-specialized-bsr ,x ,y int.bsl))
(##define-macro (erl-bsr/2 x y)
		`(fixnum-specialized-bsr ,x ,y erl-generic-bsr/2))

;; */2 (Guard BIF)
(##define-macro (erl-tst-*/2 x y)
		`(number-specialized-* ,x ,y erl-generic-tst-*/2))
(##define-macro (erl-safe-*/2 x y)
		`(number-specialized-* ,x ,y *))
(##define-macro (erl-*/2 x y)
		`(number-specialized-* ,x ,y erl-generic-*/2))

;; //2 (Guard BIF)
(##define-macro (erl-tst-//2 x y)
		`(number-specialized-/ ,x ,y erl-generic-tst-//2))
(##define-macro (erl-safe-//2 x y)
		`(number-specialized-/ ,x ,y erl-generic-safe-//2))
(##define-macro (erl-//2 x y)
		`(number-specialized-/ ,x ,y erl-generic-//2))

;; ///2 (Guard BIF)
(##define-macro (erl-tst-///2 x y)
		`(fixnum-specialized-// ,x ,y erl-generic-tst-///2))
(##define-macro (erl-safe-///2 x y)
		`(fixnum-specialized-// ,x ,y erl-generic-safe-///2))
(##define-macro (erl-///2 x y)
		`(fixnum-specialized-// ,x ,y erl-generic-///2))

;; div/2 (Guard BIF)
(##define-macro (erl-tst-div/2 x y)
		`(fixnum-specialized-div ,x ,y erl-generic-tst-div/2))
(##define-macro (erl-safe-div/2 x y)
		`(fixnum-specialized-div ,x ,y erl-generic-safe-div/2))
(##define-macro (erl-div/2 x y)
		`(fixnum-specialized-div ,x ,y erl-generic-div/2))

;; mod/2 (Guard BIF)
(##define-macro (erl-tst-mod/2 x y)
		`(fixnum-specialized-mod ,x ,y erl-generic-tst-mod/2))
(##define-macro (erl-safe-mod/2 x y)
		`(fixnum-specialized-mod ,x ,y erl-generic-safe-mod/2))
(##define-macro (erl-mod/2 x y)
		`(fixnum-specialized-mod ,x ,y erl-generic-mod/2))

;; rem/2 (Guard BIF)
(##define-macro (erl-tst-rem/2 x y)
		`(fixnum-specialized-rem ,x ,y erl-generic-tst-rem/2))
(##define-macro (erl-safe-rem/2 x y)
		`(fixnum-specialized-rem ,x ,y erl-generic-safe-rem/2))
(##define-macro (erl-rem/2 x y)
		`(fixnum-specialized-rem ,x ,y erl-generic-rem/2))

;; band/2 (Guard BIF)
(##define-macro (erl-tst-band/2 x y)
		`(fixnum-specialized-band ,x ,y erl-generic-tst-band/2))
(##define-macro (erl-safe-band/2 x y)
		`(fixnum-specialized-band ,x ,y int.band))
(##define-macro (erl-band/2 x y)
		`(fixnum-specialized-band ,x ,y erl-generic-band/2))

;; ++/2
(##define-macro (erl-safe-++/2 x y) `(erl-generic-safe-++/2 ,x ,y))
(##define-macro (erl-++/2 x y) `(erl-generic-++/2 ,x ,y))

;; --/2 (Guard BIF) ??
(##define-macro (erl---/2 x y) `(erl-generic---/2 ,x ,y))

;; or/2
(##define-macro (erl-or/2 x y) `(erl-generic-or/2 ,x ,y))

;; xor/2
(##define-macro (erl-xor/2 x y) `(erl-generic-xor/2 ,x ,y))

;; and/2
(##define-macro (erl-and/2 x y) `(erl-generic-and/2 ,x ,y))

;; Comparison 'BIFs'

;; =:=/2
(##define-macro (erl-=:=/2 x y)
  `(number-specialized-=:= ,x ,y erl-generic-=:=))

;; =/=/2
(##define-macro (erl-=/=/2 x y) `(not (erl-=:=/2 ,x ,y)))

;; ==/2
(##define-macro (erl-==/2 x y)
  `(number-specialized-== ,x ,y erl-generic-==))

;; /=/2
(##define-macro (erl-/=/2 x y) `(not (erl-==/2 ,x ,y)))

;; </2
(##define-macro (erl-</2 x y)
  `(number-specialized-< ,x ,y erl-generic-<))

;; >/2
(##define-macro (erl->/2 x y) `(erl-</2 ,y ,x))

;; >=/2
(##define-macro (erl->=/2 x y) `(not (erl-</2 ,x ,y)))

;; <=/2
(##define-macro (erl-<=/2 x y) `(not (erl-</2 ,y ,x)))

(##define-macro (erl-send/2 x y) `(erl-generic-send/2 ,x ,y))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Semi BIFS 
;; Cannot be invoked without 'erlang:' prefix

;; check_process_code/2
(##define-macro (erl-check_process_code/2 x y)
  `(erlang:check_process_code/2 ,x ,y))

;; delete_module/1
(##define-macro (erl-delete_module/1 x)
  `(erlang:delete_module/1 ,x))

;; etos_rootdir/0 (ETOS SPECIFIC BIF)
(##define-macro (erl-etos_rootdir/0) `(erlang:etos_rootdir/0))

;; get_cookie/0
(##define-macro (erl-get_cookie/0) 'node.magic_cookie)

;; halt/0
(##define-macro (erl-halt/0) `(erlang:halt/0))

;; hash/2
(##define-macro (erl-hash/2 x y) `(erlang:hash/2 ,x ,y))

;; load_module/2 (ETOS SPECIFIC VERSION)
(##define-macro (erl-load_module/2 x y) `(erlang:load_module/2 ,x ,y))

;; m_acos/1
;; m_acosh/1
;; m_asin/1
;; m_asinh/1
;; m_atan/1
;; m_atan2/2
;; m_atanh/1
;; m_cos/1
;; m_cosh/1
;; m_erf/1
;; m_erfc/1
;; m_exp/1
;; m_log/1
;; m_log10/1
;; m_pow/2
;; m_sin/1
;; m_sinh/1
;; m_sqrt/1
;; m_tan/1
;; m_tanh/1

;; module_loaded/1
(##define-macro (erl-module_loaded/1 x) `(erlang:module_loaded/1 ,x))

;; preloaded/0
(##define-macro (erl-preloaded/0) `(erlang:preloaded/0))

;; purge_module/1
(##define-macro (erl-purge_module/1 x) `(erlang:purge_module/1 ,x))

;; set_cookie/2
(##define-macro (erl-set_cookie/2 x y) `(erlang:set_cookie/2 ,x ,y))


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; List comprehension utils
(##define-macro (erl-map f l) `(map ,f ,l))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Erlang function definition

;; Returns a function definition from its name passed as an atom
;; will return #f if undefined
(##define-macro (erl-function-ref x)
		(if (and (pair? x)
			 (eq? (car x) 'quote)
			 (pair? (cdr x))
			 (null? (cddr x)))
		    `(and (not (##unbound? ,(cadr x)))
			  ,(cadr x))
		    `(erl-generic-function-ref ,x)))

;;;;;;;;;;;;
;; Processes

(##define-macro (make-process
                 state
                 continuation
                 mailbox
                 mailbox-probe
                 wake-up
                 trap-exit
                 pid
                 linked-pids
		 group-leader
		 error-handler
		 node
		 priority
		 mailbox-probe-cont
		 abrupt-stack
		 dict
		 initial-call
                 exit-hook)

  ; state of process can be: running, ready, waiting, dead

  `(let ((process
          (vector #f
                  #f
                  ,state
                  ,continuation
                  ,mailbox
                  ,mailbox-probe
                  ,wake-up
                  ,trap-exit
                  ,pid
                  ,linked-pids
		  ,group-leader
		  ,error-handler
		  ,node
		  ,priority
		  ,mailbox-probe-cont
		  ,abrupt-stack
		  ,dict
		  ,initial-call
                  ,exit-hook
		  )))
     (process-succ-set! process process)
     (process-prev-set! process process)
     process))

(##define-macro (process-succ process)
  `(vector-ref ,process 0))
(##define-macro (process-succ-set! process succ)
  `(vector-set! ,process 0 ,succ))

(##define-macro (process-prev process)
  `(vector-ref ,process 1))
(##define-macro (process-prev-set! process prev)
  `(vector-set! ,process 1 ,prev))

(##define-macro (process-state process)
  `(vector-ref ,process 2))
(##define-macro (process-state-set! process state)
  `(vector-set! ,process 2 ,state))

(##define-macro (process-continuation process)
  `(vector-ref ,process 3))
(##define-macro (process-continuation-set! process cont)
  `(vector-set! ,process 3 ,cont))

(##define-macro (process-mailbox process)
  `(vector-ref ,process 4))

(##define-macro (process-mailbox-probe process)
  `(vector-ref ,process 5))
(##define-macro (process-mailbox-probe-set! process probe)
  `(vector-set! ,process 5 ,probe))

(##define-macro (process-wake-up process)
  `(vector-ref ,process 6))
(##define-macro (process-wake-up-set! process cont)
  `(vector-set! ,process 6 ,cont))

(##define-macro (process-trap-exit process)
  `(vector-ref ,process 7))
(##define-macro (process-trap-exit-set! process cont)
  `(vector-set! ,process 7 ,cont))

(##define-macro (process-pid process)
  `(vector-ref ,process 8))
(##define-macro (process-pid-set! process pid)
  `(vector-set! ,process 8 ,pid))

(##define-macro (process-linked-pids process)
  `(vector-ref ,process 9))
(##define-macro (process-linked-pids-set! process pids)
  `(vector-set! ,process 9 ,pids))

(##define-macro (process-group-leader process)
  `(vector-ref ,process 10))
(##define-macro (process-group-leader-set! process group-leader)
  `(vector-set! ,process 10 ,group-leader))

(##define-macro (process-error-handler process)
  `(vector-ref ,process 11))
(##define-macro (process-error-handler-set! process error-handler)
  `(vector-set! ,process 11 ,error-handler))

(##define-macro (process-node process)
  `(vector-ref ,process 12))

(##define-macro (process-priority process)
  `(vector-ref ,process 13))
(##define-macro (process-priority-set! process priority)
  `(vector-set! ,process 13 ,priority))

(##define-macro (process-mailbox-probe-cont process)
  `(vector-ref ,process 14))
(##define-macro (process-mailbox-probe-cont-set! process priority)
  `(vector-set! ,process 14 ,priority))

(##define-macro (process-abrupt-stack process)
  `(vector-ref ,process 15))
(##define-macro (process-abrupt-stack-set! process abrupt-stack)
  `(vector-set! ,process 15 ,abrupt-stack))

(##define-macro (process-dict process)
  `(vector-ref ,process 16))
(##define-macro (process-dict-set! process dict)
  `(vector-set! ,process 16 ,dict))

(##define-macro (process-initial-call process)
  `(vector-ref ,process 17))
(##define-macro (process-initial-call-set! process initial-call)
  `(vector-set! ,process 17 ,initial-call))

(##define-macro (process-exit-hook process)
  `(vector-ref ,process 18))
(##define-macro (process-exit-hook-set! process exit-hook)
  `(vector-set! ,process 18 ,exit-hook))



(##define-macro (erl-error_handler)
  `(process-error-handler node.current-process))

(##define-macro (erl-push-abrupt! cont)
  `(process-abrupt-stack-set!
    node.current-process
    (cons ,cont
	  (process-abrupt-stack node.current-process))))

(##define-macro (erl-pop-abrupt!)
  `(process-abrupt-stack-set!
    node.current-process
    (cdr (process-abrupt-stack node.current-process))))

(##define-macro (erl-abrupt-top)
  `(car (process-abrupt-stack node.current-process)))

(##define-macro (erl-receive_accept)
  `(with-no-interrupts
    (lambda ()
      (timer-interrupt-disable!)
      (queue-extract! (process-mailbox node.current-process)
		      (process-mailbox-probe node.current-process))
      (timer-interrupt-enable!))))

(##define-macro (erl-receive_first timeout thunk)
  `(,thunk
    (with-no-interrupts
     (lambda ()
       (process-wake-up-set!
        node.current-process
        ,(if (and (pair? timeout)
                  (eq? (car timeout) 'quote)
                  (pair? (cdr timeout))
                  (eq? (cadr timeout) (string->symbol "infinity")))
             #f
             `(if-non-neg-fix? ,timeout
                (int.+ (current-time-in-msecs) ,timeout)
                (let ((timeout ,timeout))
                  (if (eq? timeout infinity-atom)
                      #f
                      (if (and (erl-int? timeout)
                               (not (int.< timeout 0)))
                          (int.+ (current-time-in-msecs) timeout)
                          (erl-exit-badarg)))))))
       (continuation-save!
        (process-mailbox-probe-cont node.current-process)
        (lambda (cont)
          (let ((probe (queue-probe (process-mailbox node.current-process))))
            (process-mailbox-probe-set! node.current-process probe)
            (erl-receive_check probe cont))))
       (let ((next (queue-next (process-mailbox-probe node.current-process))))
         (if (pair? next)
             (car next)
             '$timeout))))))

(##define-macro (erl-receive_next)
  `(let ((probe (queue-next (process-mailbox-probe node.current-process)))
         (cont (process-mailbox-probe-cont node.current-process)))
     (process-mailbox-probe-set! node.current-process probe)
     (erl-receive_check probe cont)))

(##define-macro (erl-receive_check probe cont)
  `(with-no-interrupts
    (lambda ()
      (if (pair? (queue-next ,probe))
          (continuation-restore ,cont 'dummy)
          (let ((wake-up (process-wake-up node.current-process)))
            (if (or (not wake-up)
                    (int.< (current-time-in-msecs) wake-up))
                (process-suspend-waiting-with-continuation! ,cont)
                (continuation-restore ,cont 'dummy)))))))

;;;;;;;
;; PIDs
(##define-macro (make-pid process id node creation)
  `(vector 'pid ,process ,id ,node ,creation))
(##define-macro (erl-pid-process pid) `(vector-ref ,pid 1))
(##define-macro (erl-pid-id pid) `(vector-ref ,pid 2))
(##define-macro (erl-pid-node pid) `(vector-ref ,pid 3))
(##define-macro (erl-pid-creation pid) `(vector-ref ,pid 4))
(##define-macro (erl-pid-local? pid) `(eq? (erl-pid-node pid) node.name))

;;;;;;;
;; Refs
(##define-macro (make-ref id node creation)
  `(vector 'ref ,id ,node ,creation))
(##define-macro (erl-ref-id ref) `(vector-ref ,ref 1))
(##define-macro (erl-ref-node ref) `(vector-ref ,ref 2))
(##define-macro (erl-ref-creation ref) `(vector-ref ,ref 3))

;;;;;;;;
;; Ports
(##define-macro (make-port pidx s_res owner packeting binary? linked-pids
			   in? eof? creation node id opened?)
  `(vector 'port ,pidx ,s_res ,owner ,packeting ,binary? ,linked-pids
	   ,in? ,eof? ,creation ,node ,id ,opened?))
(##define-macro (erl-port-pidx port) `(vector-ref ,port 1))
(##define-macro (erl-port-s_res port) `(vector-ref ,port 2))
(##define-macro (erl-port-owner port) `(vector-ref ,port 3))
(##define-macro (erl-port-owner-set! port owner)
		`(vector-set! ,port 3 ,owner))
(##define-macro (erl-port-packeting port) `(vector-ref ,port 4))
(##define-macro (erl-port-io_type port) `(vector-ref ,port 5))
(##define-macro (erl-port-linked-pids port) `(vector-ref ,port 6))
(##define-macro (erl-port-linked-pids-set! port linked-pids)
		`(vector-set! ,port 6 ,linked-pids))
(##define-macro (erl-port-in? port) `(vector-ref ,port 7))
(##define-macro (erl-port-eof? port) `(vector-ref ,port 8))
(##define-macro (erl-port-creation port) `(vector-ref ,port 9))
(##define-macro (erl-port-node port) `(vector-ref ,port 10))
(##define-macro (erl-port-id port) `(vector-ref ,port 11))
(##define-macro (erl-port-opened? port) `(vector-ref ,port 12))
(##define-macro (erl-port-opened?-set! port opened?)
		`(vector-set! ,port 12 ,opened?))

; (##define-macro (make-port pidx s_res owner packeting binary? in? eof?)
;   `(let ((p (vector 'port
; 		    ,pidx           ;; pidx
; 		    ,s_res          ;; start result
; 		    ,owner          ;; owner's PID
; 		    ,packeting      ;; packeting size
; 		    ,binary?        ;; receive binaries?
; 		    (erl-nil)       ;; linked processes
; 		    ,in?            ;; may receive input?
; 		    ,eof?           ;; the eof flag
; 		    node.creation   ;; creation
; 		    node.name       ;; node
; 		    node.port-count ;; id
; 		    )))
;      (set! node.port-count (int.+ node.port-count 1))
;      (vector-set! port-table idx p)
;      p))

;; try to convert ioterm to binary, #f if failure
(##define-macro (erl-ioterm_to_binary x)
  `(let ((a ,x))
     (if (erl-binary? a)
	 a
	 (let ((r (erl-generic-ioterm_to_list a)))
	   (and r (erl-safe-list_to_binary/1 r))))))

;; Timer interrupts

(##define-macro (setup-time!) #f)
(##define-macro (advance-time!) #f)
(##define-macro (current-time-in-usecs)
  `(with-no-interrupts
    (lambda ()
      (inexact->exact (flround (fl* 1e6 (time->seconds (current-time))))))))
(##define-macro (current-time-in-msecs)
  `(with-no-interrupts
    (lambda ()
      (inexact->exact (flround (fl* 1e3 (time->seconds (current-time))))))))
(##define-macro (current-cputime-in-msecs)
  `(with-no-interrupts
    (lambda ()
      (inexact->exact (flround (fl* 1e3 (f64vector-ref (##process-statistics) 0)))))))
(##define-macro (timer-interrupt-disable!)
  `(set! timer-interrupt-allowed? #f))
(##define-macro (timer-interrupt-enable!)
  `(set! timer-interrupt-allowed? #t))
(##define-macro (with-no-interrupts thunk)
  `(let ()
     (##declare (not interrupts-enabled))
     (,thunk)))
(##define-macro (allow-interrupts)
  `(##declare (interrupts-enabled)))

(##define-macro (add-timer-interrupt-job job)
  `(##interrupt-vector-set! 1 ,job))

(##define-macro (cleanup-timer-interrupt!)
  `(##interrupt-vector-set! 1 (lambda () #f)))

;;;;;;;;;;;;;;
;; FIFO Queues
(##define-macro (make-queue)
  `(let ((q (cons '() '())))
     (set-car! q q)
     q))

(##define-macro (queue-empty? q)
  `(let ((q ,q))
     (eq? (car q) q)))

(##define-macro (queue-probe q) q)

(##define-macro (queue-next p) `(cdr ,p))

(##define-macro (queue-add-to-tail! q x)
  `(let ((q ,q))
     (with-no-interrupts
      (lambda ()
	(let ((cell (cons ,x '())))
	  (set-cdr! (car q) cell)
	  (set-car! q cell))))))

(##define-macro (queue-extract! q probe)
  `(let ((q ,q) (probe ,probe))		
     (with-no-interrupts
      (lambda ()
	(let ((curr (cdr probe)))
	  (if (eq? curr (car q)) ; last element?
	      (set-car! q probe))
	  (set-cdr! probe (cdr curr)))))))

;;;;;;;;;;
;; Signals
;; Assume local for now.
(##define-macro (erl-group-leader-signal! dest_pid new_gl)
  `(process-group-leader-set! (erl-pid-process ,dest_pid) ,new_gl))

(##define-macro (erl-link-signal! dest_pid)
  `(process-link! (erl-pid-process ,dest_pid) (erl-self/0)))

(##define-macro (erl-unlink-signal! dest_pid)
  `(process-unlink! (erl-pid-process ,dest_pid) (erl-self/0)))

(##define-macro (erl-message-signal! dest_pid msg)
  `(process-deliver! (erl-pid-process ,dest_pid) ,msg))

(##define-macro (erl-info-signal dest_pid prop)
  `(process-get-property (erl-pid-process ,dest_pid) ,prop))

;; This one should check the trap flag
;; also self-signals!!!
(##define-macro (erl-exit-signal! dest_pid reason)
  `(process-die! (erl-pid-process ,dest_pid) ,reason))


;------------------------------------------------------------------------------

; Added by Marc Feeley for ETOS 2.2

(##define-macro (erl-function-unbound? m f a)
  (let ((var (string->symbol (string-append m ":" f "/" (number->string a)))))
    `(##unbound? (##global-var-ref (##make-global-var ',var)))))

(##define-macro (erl-undefined-function m f)
  (define-macro (erl-atom<-string str) `(string->symbol ,str));********kludge
  (let ((mod (erl-atom<-string m))
        (fun (erl-atom<-string f)))
    `(lambda args (erl-undefined-function-handler args ',mod ',fun))))

(##define-macro (erl-function-set! global-var val)
  `(##global-var-set! (##make-global-var ',global-var) ,val))

(define-macro (erl-false) `'false)
(define-macro (erl-true) `'true)
(define-macro (erl-impossible-obj1) `#f)
(define-macro (erl-impossible-obj2) `#t)

;(define-macro (erl-atom? x) `(symbol? ,x))
(define-macro (erl-atom<-string str) `(string->symbol ,str))
(define-macro (erl-atom->string atom) `(symbol->string ,atom))

;(define-macro (erl-false) `#f)
;(define-macro (erl-true) `#t)
;(define-macro (erl-impossible-obj1) `'false)
;(define-macro (erl-impossible-obj2) `'true)
;
;(define-macro (erl-atom? x) `(let ((x ,x)) (or (symbol? x) (boolean? x))))
;
;(define-macro (erl-atom<-string str)
;  `(let ((atom (string->symbol ,str)))
;     (cond ((eq? atom 'false) #f)
;           ((eq? atom 'true)  #t)
;           (else              atom))))
;
;(define-macro (erl-atom->string atom)
;  `(let ((atom ,atom))
;     (symbol->string
;      (cond ((eq? atom #f) 'false)
;            ((eq? atom #t) 'true)
;            (else          atom)))))

;(define-macro (erl-char? x)
;  `(let ((x ,x)) (and (erl-fix? x) (not (fix.< x 0)) (not (fix.< 65535 x)))))
;
;(define-macro (erl-char<-char c) `(char->integer ,c)) ; hope for Unicode
;(define-macro (erl-char->char c) `(integer->char ,c))

;(define-macro (erl-char? x) `(char? ,x))

(define-macro (erl-char<-char c) c)
(define-macro (erl-char->char c) c)

(define-macro (erl-float<-real n) `(exact->inexact ,n))
(define-macro (erl-float->real n) n)
(define-macro (erl-int<-exact-integer n) n)
(define-macro (erl-int->exact-integer n) n)
(define-macro (erl-true? x) `(not (eq? ,x (erl-false))))
(define-macro (erl-equal? x y) `(equal? ,x ,y))
;(define-macro (erl-nil) `'())
;(define-macro (erl-nil? x) `(null? ,x))
;(define-macro (erl-cons x y) `(cons ,x ,y))
;(define-macro (erl-cons? x) `(pair? ,x))
;(define-macro (erl-hd x) `(car ,x))
;(define-macro (erl-tl x) `(cdr ,x))
;(define-macro (erl-list . elems) `(list ,@elems))
;(define-macro (erl-append . lists) `(append ,@lists))
(define-macro (erl-list<-list lst) lst)
;(define-macro (erl-tuple? x)
;  `(let ((x ,x)) (and (vector? x) (fix.< 0 (vector-length x)) (eq? (vector-ref x 0) 'tuple))))
;(define-macro (erl-tuple . elems) `(vector 'tuple ,@elems))
;(define-macro (erl-tuple-size tup) `(fix.u- (vector-length ,tup) 1))
;(define-macro (erl-tuple-ref tup i) `(vector-ref ,tup ,i))
(define-macro (erl-tuple<-list lst) `(list->vector (cons 'tuple ,lst)))
