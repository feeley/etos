; File: "rt-lib.scm"

; Copyright (C) 1999, Marc Feeley, Patrick Piche, All Rights Reserved.

; (Generic) RunTime library for EtoS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Erlang function definition

(define erl-defined-functions `())

(define (erl-define-functions lst)
  (set! erl-defined-functions
	(append lst erl-defined-functions)))

(define (erl-generic-function-ref x)
  (and ;****************(memq x erl-defined-functions)
       (eval x)))


;(define (erl-atom<-string x) (string->symbol x))
;(define (erl-atom->string x) (symbol->string x))
(define erl-atom-append
  (lambda symbols
    (string->symbol (apply string-append (map symbol->string symbols)))))


;; Tuple assoc lists...
(define (erl-assoc term lst)
  (if (erl-cons? lst)
      (let ((tup (erl-hd lst)))
        (if (and (erl-tuple? tup)
                 (fix.< 1 (erl-tuple-size tup))
                 (erl-equal? (erl-tuple-ref tup 1) term))
            tup
            (erl-assoc term (erl-tl lst))))
      (erl-false)))

(define (erl-member term lst)
  (if (erl-cons? lst)
      (let ((elem (erl-hd lst)))
        (if (erl-equal? elem term)
            lst
            (erl-member term (erl-tl lst))))
      (erl-false)))

;; The hash function.
;; Very slow due to 32bit arithmetics
(define hash_c1 268440163)
(define hash_c2 268439161) 
(define hash_c3 268435459)
(define hash_c4 268436141) 
(define hash_c5 268438633)
(define hash_c6 268437017) 
(define hash_c7 268438039)
(define hash_c8 268437511) 
(define hash_c9 268439627)

(define pow32 (* 65536 65536))
(define pow28 (* 65536 4096))
(define (hash.+ x y) (modulo (+ x y) pow32))
(define (hash.* x y) (modulo (* x y) pow32))
(define (hash.or x y)
  (+ (fix.bor (fix.band x 65535) (fix.band y 65535))
     (* (fix.bor (fix.div x 65536) (fix.div y 65536)) 65536)))
(define (hash.foldl f h lst)
  (if (erl-con? lst)
      (hash.foldl f (f (erl-hd lst) h) (erl-tl lst))
      (if (erl-nil? lst) h (f lst h))))

(define (big->le x)
  (if (< x 0)
      (big->le (- x))
      (let loop ((n x))
	(if (< n pow32)
	    (list n)
	    (cons (modulo n pow32)
		  (loop (quotient n pow32)))))))

(define (hash t h)
  (cond
   ((erl-fix? t) (hash.+ (hash.* hash_c2 h) t))
   ((erl-sub? t)
    (cond
     ((erl-ato? t)
      (hash.+
       (hash.* hash_c1 h)
       (hash.foldl (lambda (i h)
		     (let ((j (hash.+ (hash.* 16 h) i)))
		       (hash.or (modulo j pow28)
				(hash.* 16 (quotient j pow28)))))
		   0
		   (map char->integer (string->list (erl-atom->string t))))))
     ((erl-big? t)
      (let ((le (big->le t)))
	(hash.+
	 (hash.* (if (< t 0) hash_c3 hash_c2)
		 (hash.foldl (lambda (w h) (hash.+ (hash.* hash_c2 h) w))
			     h
			     le))
	 (length le))))
     ((erl-flo? t) ;; *** not as in the spec
      (hash.+ (hash.* hash_c6 h)
	      (hash (denominator (inexact->exact t)) h)))
     (else
      (case (erl-vector-ref t 0)
	((tuple) (hash.+ (hash.* hash_c9
				 (hash.foldl hash h (erl-tuple_to_list/1 t)))
			 (erl-tuple-size t)))
	(else 0)))))
   ((erl-spc? t)
    (cond
     ((erl-nil? t) (hash.+ (hash.* hash_c3 h) 1))
     (else (hash (char->integer t) h))))
   (else
    (hash.* hash_c8 (hash.foldl hash h t)))))


;;;;;;;;;;;;;
;; Registries

;; Returns a new empty registry
(define (erl-registry.new default)
  (let ((v (make-vector 404 '())))
    (vector-set! v 0 default)
    v))

;; Removes the entry associated with given key, returns previous value
(define (erl-registry.remove! reg key)
  (let* ((h (erl-hash/2 key 403))
	 (row (vector-ref reg h)))
    (if (erl-cons? row)
	(let ((hd (erl-hd row)))
	  (if (erl-=:=/2 (erl-vector-ref hd 2) key)
	      (let ((old (erl-vector-ref hd 3)))
		(vector-set! reg h (erl-tl row))
		old)
	      (let loop ((parent row) (child (erl-tl row)))
		(if (erl-cons? child)
		    (let ((hd (erl-hd child)))
		      (if (erl-=:=/2 (erl-vector-ref hd 2) key)
			  (let ((old (erl-vector-ref hd 3)))
			    (set-cdr! parent (erl-tl child))
			    old)
			  (loop child (erl-tl child))))
		    (vector-ref reg 0)))))
	(vector-ref reg 0))))

;; Returns an Erlang association list from a registry
(define (erl-registry.to_list reg)
  (let loop ((idx 1) (res '()))
    (if (fix.< 403 idx)
	res
	(loop (fix.u+ idx 1) (erl-append (vector-ref reg idx) res)))))

;; Returns the list of keys having a specified value
(define (erl-registry.get_keys reg val)
  (let loop ((lst (erl-registry.to_list reg)) (res '()))
    (if (erl-nil? lst)
	res
	(loop (erl-tl lst)
	      (let ((hd (erl-hd lst)))
		(if (erl-=:=/2 (erl-vector-ref hd 3) val)
		    (cons (erl-vector-ref hd 2) res)
		    res))))))

;; Lookup a registry for a value associated with a key
(define (erl-registry.lookup reg key)
  (let* ((row (vector-ref reg (erl-hash/2 key 403)))
	 (found (erl-assoc key row)))
    (if (erl-tuple? found)
	(erl-vector-ref found 2)
	(vector-ref reg 0))))

(define (erl-registry.assign! reg key val)
  (let* ((h (erl-hash/2 key 403))
	 (row (vector-ref reg h))
	 (found (erl-assoc key row)))
    (if (erl-tuple? found)
	(let ((old (erl-vector-ref found 3)))
	  (erl-vector-set! found 3 val)
	  old)
	(vector-set! reg h (erl-cons (erl-tuple key val) row)))))


;;;;;;;;;;;;;;;;;;;;
;; Common arithmetic 
(define (fix.pow2 n)
  (if (fix.= n 0)
      1
      (let ((pow2div2 (fix.pow2 (fix.div n 2))))
	(if (fix.even? n)
	    (int.* pow2div2 pow2div2)
	    (int.* 2 (int.* pow2div2 pow2div2))))))
(define (int.pow2 n)
  (if (fix.= n 0)
      1
      (let ((pow2div2 (int.pow2 (int.div n 2))))
	(if (int.even? n)
	    (int.* pow2div2 pow2div2)
	    (int.* 2 (int.* pow2div2 pow2div2))))))

;; y guaranteed not to be negative
(define (int.bsl x y)
  (if (erl-fix? y)
      (int.* x (fix.pow2 y))
      (int.* x (int.pow2 y))))
(define (int.bsr x y)
  (if (erl-fix? y)
      (int.div x (fix.pow2 y))
      (int.div x (int.pow2 y))))

;; Pattern-matcher's composite tests
(define (erl-lst? x)
  (or (erl-nil? x) (and (erl-con? x) (erl-lst? (erl-tl x)))))
(define (erl-str? x)
  (or (erl-nil? x)
      (and (erl-con? x)
	   (let ((hd (erl-hd x)))
	     (and (erl-spc? hd) (erl-chr? hd) (erl-str? (erl-tl x)))))))

;; specialized =:=
(define (erl-pid-=:= x y)
  (and (int.= (erl-pid-id x) (erl-pid-id y))
       (int.= (erl-pid-creation x) (erl-pid-creation x))
       (ato.= (erl-pid-node x) (erl-pid-node y))))
(define (erl-ref-=:= x y)
  (and (int.= (erl-ref-id x) (erl-ref-id y))
       (int.= (erl-ref-creation x) (erl-ref-creation x))
       (ato.= (erl-ref-node x) (erl-ref-node y))))
(define (erl-port-=:= x y)
  (and (int.= (erl-port-id x) (erl-port-id y))
       (int.= (erl-port-creation x) (erl-port-creation x))
       (ato.= (erl-port-node x) (erl-port-node y))))
(define (erl-binary-=:= x y)
  (let ((size (erl-binary-size x)))
    (and (fix.= size (erl-binary-size y))
	 (let ((u81 (erl-binary-u8vector x)) (u82 (erl-binary-u8vector y)))
	   (let loop ((off1 (erl-binary-offset x))
		      (off2 (erl-binary-offset y))
		      (todo size))
	     (or (fix.= todo 0)
		 (and (fix.= (u8vector-ref u81 off1) (u8vector-ref u82 off2))
		      (loop (fix.u+ off1 1) (fix.u+ off2 1)))))))))

;; specialized <
(define (erl-pid-< x y)
  (let ((n1 (erl-pid-node x)) (n2 (erl-pid-node y)))
    (or (erl-generic-< n1 n2)
	(and (ato.= n1 n2) (fix.< (erl-pid-id x) (erl-pid-id y))))))
(define (erl-ref-< x y)
  (let ((n1 (erl-ref-node x)) (n2 (erl-ref-node y)))
    (or (erl-generic-< n1 n2)
	(and (ato.= n1 n2) (fix.< (erl-ref-id x) (erl-ref-id y))))))
(define (erl-port-< x y)
  (let ((n1 (erl-port-node x)) (n2 (erl-port-node y)))
    (or (erl-generic-< n1 n2)
	(and (ato.= n1 n2) (fix.< (erl-port-id x) (erl-port-id y))))))
(define (erl-binary-< x y)
  (let ((size1 (erl-binary-size x)) (size2 (erl-binary-size y)))
    (or (fix.< size1 size2)
	(and
	 (fix.= size1 size2)
	 (let ((u81 (erl-binary-u8vector x)) (u82 (erl-binary-u8vector y)))
	   (let loop ((off1 (erl-binary-offset x))
		      (off2 (erl-binary-offset y))
		      (todo size1))
	     (and (fix.< 0 todo)
		  (let ((b1 (u8vector-ref u81 off1))
			(b2 (u8vector-ref u82 off2)))
		    (or (fix.< b1 b2)
			(and (fix.= b1 b2)
			     (loop (fix.u+ off1 1) (fix.u+ off2 1))))))))))))

(define (erl-generic-=:= x y)
;  (pp (list 'erl-generic-=:= x y))
  (cond
   ((erl-fix? x) (and (erl-fix? y) (fix.= x y)))
   ((erl-sub? x)
    (and
     (erl-sub? y)
     (cond
      ((erl-flo? x) (and (erl-flo? y) (flo.= x y)))
      ((erl-big? x) (and (erl-big? y) (big.= x y)))
      ((erl-ato? x) (and (erl-ato? y) (ato.= x y)))
      ((erl-vec? x)
       (and
	(erl-vec? y)
	(let ((size (erl-vector-length x))
	      (type (erl-vector-ref x 0)))
	  (and
	   (fix.= size (erl-vector-length y))
	   (ato.= type (erl-vector-ref y 0))
	   (case type
	     ((tuple)
	      (let loop ((i (fix.u- size 1)))
		(if (fix.= i 0)
		    #t
		    (and (erl-generic-=:= (erl-vector-ref x i)
					  (erl-vector-ref y i))
			 (loop (fix.u- i 1))))))
	     ((ref) (erl-ref-=:= x y))
	     ((pid) (erl-pid-=:= x y))
	     ((port) (erl-port-=:= x y))
	     ((binary) (erl-binary-=:= x y))
	     (else (internal-error!!!! "=:="))))))))))
   ((erl-con? x) (and (erl-con? y)
		      (erl-generic-=:= (erl-hd x) (erl-hd y))
		      (erl-generic-=:= (erl-tl x) (erl-tl y))))
   ((erl-spc? x) (and (erl-spc? y)
		      (cond ((erl-nil? x) (erl-nil? y))
			    ((erl-chr? x) (and (erl-chr? y) (chr.= x y)))
			    (else (internal-error!!!! "=:=")))))))

(define (erl-generic-== x y)
  (cond
   ((erl-fix? x) (or (and (erl-fix? y) (fix.= x y))
		     (and (erl-sub? y)
			  (and (erl-flo? y) (flo.= (exact->inexact x) y))
			  (and (erl-big? y) (= x y)))))
   ((erl-sub? x)
    (and (erl-sub? y)
	 (cond
	  ((erl-flo? x) (or (and (erl-fix? y) (flo.= x (exact->inexact y)))
			    (and (erl-flo? y) (flo.= x y))
			    (and (erl-big? y) (flo.= x (exact->inexact y)))))
	  ((erl-big? x) (or (and (erl-fix? y) (= x y))
			    (and (erl-flo? y) (= x y))
			    (and (erl-big? y) (big.= x y))))
	  ((erl-ato? x) (and (erl-ato? y) (ato.= x y)))
	  ((erl-vec? x)
	   (and (erl-vec? y)
		(let ((size (erl-vector-length x))
		      (type (erl-vector-ref x 0)))
		  (and (fix.= size (erl-vector-length y))
		       (ato.= type (erl-vector-ref y 0))
		       (case type
			 ((tuple)
			  (let loop ((i (fix.u- size 1)))
			    (if (fix.= i 0)
				#t
				(and (erl-generic-== (erl-vector-ref x i)
						     (erl-vector-ref y i))
				     (loop (fix.u- i 1))))))
			 (else (erl-generic-=:= x y))))))))))
   ((erl-con? x) (and (erl-con? y)
		      (erl-generic-== (erl-hd x) (erl-hd y))
		      (erl-generic-== (erl-tl x) (erl-tl y))))
   ((erl-spc? x) (and (erl-spc? y)
		      (cond
		       ((erl-nil? x) (erl-nil? y))
		       ((erl-chr? x) (and (erl-chr? y) (chr.= x y)))
		       (else (internal-error!!!! "==")))))))

(define (erl-generic-< x y)
  (cond
   ((erl-fix? x) (cond
		  ((erl-fix? y) (fix.< x y))
		  ((erl-sub? y) (cond
				 ((erl-big? y) (< x y))
				 ((erl-flo? y) (flo.< (exact->inexact x) y))
				 (else #t)))
		  (else #t)))
   ((erl-sub? x)
    (cond
     ((erl-flo? x) (cond
		    ((erl-fix? y) (flo.< x (exact->inexact y)))
		    ((erl-sub? y) (cond
				   ((erl-big? y) (< x y))
				   ((erl-flo? y) (flo.< x y))
				   (else #t)))
		    (else #t)))
     ((erl-big? x) (cond
		    ((erl-fix? y) (< x y))
		    ((erl-sub? y) (cond
				   ((erl-big? y) (big.< x y))
				   ((erl-flo? y) (flo.< (exact->inexact x) y))
				   (else #t)))
		    (else #t)))
     ((erl-ato? x) (cond
		    ((erl-sub? y) (cond
				   ((erl-ato? y) (string<? (erl-atom->string x)
							   (erl-atom->string y)))
				   ((erl-vec? y) #t)
			       (else #f)))
		    ((erl-fix? y) #f)
		    ((erl-con? y) #t)
		    ((erl-spc? y) (erl-chr? y))))
     ((erl-vec? x)
      (case (erl-vector-ref x 0)
	((tuple)
	 (cond
	  ((erl-sub? y)
	   (cond
	    ((erl-vec? y)
	     (case (erl-vector-ref y 0)
	       ((tuple)
		(let ((sizex (erl-vector-length x))
		      (sizey (erl-vector-length y)))
		  (or (fix.< sizex sizey)
		      (and (fix.= sizex sizey)
			   (let loop ((i (fix.u- sizex 1)))
			     (if (fix.= i 0)
				 #f
				 (let ((eltx (erl-vector-ref x i))
				       (elty (erl-vector-ref y i)))
				   (or (erl-generic-< eltx elty)
				       (and (erl-generic-== eltx elty)
					    (loop (fix.u- i 1)))))))))))
	       ((binary) #t)
	       (else #f)))
	    (else #f)))
	  ((erl-spc? y) (erl-nil? y))
	  ((erl-con? y) #t)
	  (else #f)))
	((ref)
	 (cond
	  ((erl-sub? y)
	   (cond ((erl-vec? y) (if (ato.= (erl-vector-ref y 0) 'ref)
				   (erl-ref-< x y)
				   #t))
		 (else #f)))
	  ((erl-fix? y) #f)
	  ((erl-spc? y) (erl-nil? y))
	  (else #t)))
	((function) (cond ((erl-sub? y) (if (erl-vec? y)
					    (case (erl-vector-ref y 0)
					      ((ref function) #f)
					      (else #t))
					    #t))
			  ((erl-fix? y) #f)
			  ((erl-spc? y) (erl-nil? y))
			  (else #t)))
	((port) (cond ((erl-sub? y)
		       (cond ((erl-vec? y) (case (erl-vector-ref y 0)
					     ((port) (erl-port-< x y))
					     ((ref function) #f)
					     (else #t)))
			     (else #f)))
		      ((erl-fix? y) #f)
		      ((erl-spc? y) (erl-nil? y))
		      (else #t)))
	((pid) (cond
		((erl-sub? y)
		 (cond ((erl-vec? y)
			(case (erl-vector-ref y 0)
			  ((pid) (erl-pid-< x y))
			  ((ref function port) #f)
			  (else #t)))
		       (else #f)))
		((erl-fix? y) #f)
		((erl-spc? y) (erl-nil? y))
		(else #t)))
	((binary) (and (erl-sub? y)
		       (erl-vec? y)
		       (eq? (erl-vector-ref y 0) 'binary)
		       (erl-binary-< x y)))))))
   ((erl-con? x)
    (cond
     ((erl-con? y) (let ((hdx (erl-hd x)) (hdy (erl-hd y)))
		     (or (erl-generic-< hdx hdy)
			 (and (erl-generic-== hdx hdy)
			      (erl-generic-< (erl-tl x) (erl-tl y))))))
     ((erl-sub? y) (and (erl-vec? y) (eq? (erl-vector-ref y 0) 'binary)))
     (else #f)))
   ((erl-spc? x)
    (cond
     ((erl-nil? x)
      (cond
       ((erl-con? y) #t)
       ((erl-sub? y) (and (erl-vec? y) (eq? (erl-vector-ref y 0) 'binary)))
       (else #f)))
     ((erl-chr? x)
      (cond
       ((erl-spc? y) (or (erl-nil? y) (char<? x y)))
       ((erl-sub? y) (and (not (erl-flo? y)) (not (erl-big? y))))
       ((erl-fix? y) #f)
       (else #t)))))))


(define (erl-unicode-list? x)
  (or (and (erl-con? x)
	   (let ((hd (erl-hd x)))
	     (and (erl-fixnum? hd)
		  (fix.< hd 65536)
		  (fix.< -1 hd)
		  (erl-unicode-list? (erl-tl x)))))
      (erl-nil? x)))

;; Leapyear predicate
(define (erl-leapyear? y)
  (and (= (modulo y 4) 0)
       (or (not (= (modulo y 100) 0))
	   (= (modulo y 400) 0))))

(define (year->days y)
  (if (erl-leapyear? y) 366 365))

(define (yearmonth->days y m)
  (if (and (eq? m 1) (erl-leapyear? y))
      29
      (vector-ref '#(31 28 31 30 31 30 31 31 30 31 30 31) m)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BIFS

;; abs/1 (Guard BIF)
(define (erl-generic-abs/1 x)
  (if (erl-num? x) (abs x) (erl-exit-badarg)))
(define (erl-generic-tst-abs/1 x)
  (and (erl-num? x) (abs x)))
(define (erlang:abs/1 x) (erl-abs/1 x))

;; apply/2
(define (erlang:apply/2 x y)
  (if (erl-tuple? x)
      (if (fix.= (erl-vector-length x) 3)
	  (erl-apply/3 (erl-vector-ref x 1)
		       (erl-vector-ref x 2)
		       y)
	  (erl-exit-badarg))
      (let ((arit (erl-tst-length/1 y)))
	(if arit
	    (apply erl-fun-apply (cons x (cons arit y)))
	    (erl-exit-badarg)))))

;; apply/3
(define (erlang:apply/3 x y z)
  (if (and (erl-atom? x)
	   (erl-atom? y))
      (let loop ((arity 0) (args z))
	(cond
	 ((pair? args) (loop (fix.u+ arity 1) (erl-tl args)))
	 ((null? args)
	  (let ((closure (erl-function-ref (erl-mfa->fname x y arity))))
	    (if closure
		(apply closure z)
		(erl-exit-badarg))))
	 (else (erl-exit-badarg))))
      (erl-exit-badarg)))

;; atom_to_list/1
(define (erlang:atom_to_list/1 x)
  (if (erl-atom? x)
      (map char->integer (string->list (symbol->string x)))
      (erl-exit-badarg)))

;; atom_to_string/1 (PROPOSED BIF)
(define (erlang:atom_to_string/1 x)
  (if (erl-atom? x)
      (string->list (symbol->string x))
      (erl-exit-badarg)))

;; binary_to_list/1
(define (erlang:binary_to_list/1 x)
  (if (erl-binary? x)
      (let ((u8 (erl-binary-u8vector x)) (off (erl-binary-offset x)))
	(let loop
	    ((i (fix.u- (fix.u+ off (erl-binary-size x)) 1))
	     (lst '()))
	  (if (fix.< i off)
	      lst
	      (loop (fix.u- i 1) (erl-cons (u8vector-ref u8 i) lst)))))
      (erl-exit-badarg)))

;; binary_to_list/3
(define (erlang:binary_to_list/3 b start stop)
  (if (and (erl-binary? b) 
	   (erl-fixnum? start)
	   (erl-fixnum? stop)
	   (fix.< 0 start)
	   (not (fix.< stop start))
	   (not (fix.< (erl-binary-size b) stop)))
      (let* ((u8 (erl-binary-u8vector b))
	     (off (erl-binary-offset b))
	     (start_idx (fix.u- (fix.u+ start off) 1)))
        (let loop ((i (fix.u- (fix.u+ stop off) 1)) (lst '()))
          (if (fix.< i start_idx)
	      lst
	      (loop (fix.u- i 1) (erl-cons (u8vector-ref u8 i) lst)))))
      (erl-exit-badarg)))

;; binary_to_string/1 (PROPOSED BIF)
(define (erlang:binary_to_string/1 x)
  (if (erl-binary? x)
      (let ((u8 (erl-binary-u8vector x)) (off (erl-binary-offset x)))
	(let loop
	    ((i (fix.u- (fix.u+ off (erl-binary-size x)) 1))
	     (lst '()))
	  (if (fix.< i off)
	      lst
	      (loop (fix.u- i 1) (erl-cons (integer->char (u8vector-ref u8 i))
					   lst)))))
      (erl-exit-badarg)))

;; char_to_integer/1 (PROPOSED BIF)
(define (erlang:char_to_integer/1 x)
  (if (erl-char? x)
      (chr.->integer x)
      (erl-exit-badarg)))

;; concat_binary/1
(define (erlang:concat_binary/1 l)
  (let loop ((newsize 0) (lst l))
    (cond
     ((erl-cons? lst)
      (let ((b (erl-hd lst)))
	(if (erl-binary? b)
	    (loop (fix.u+ newsize (erl-binary-size b)) (erl-tl lst))
	    (erl-exit-badarg))))
     ((erl-nil? lst)
      (let ((u8 (make-u8vector newsize)))
	(let loop ((idx 0) (lst l))
	  (if (erl-cons? lst)
	      (let* ((b (erl-hd lst))
		     (u8l (erl-binary-u8vector b))
		     (siz (erl-binary-size b))
		     (off (erl-binary-offset b)))
		(let loop2 ((idx2 0))
		  (if (fix.= idx2 siz)
		      (loop (fix.u+ idx siz) (erl-tl lst))
		      (begin
			(u8vector-set! u8
				       (fix.u+ idx idx2)
				       (u8vector-ref u8l (fix.u+ idx2 off)))
			(loop2 (fix.u+ idx2 1))))))
	      (erl-u8vector->binary u8)))))
     (else (erl-exit-badarg)))))

;; date/0
(define (erlang:date/0)
  (let ((td (quotient (current-time-in-msecs) 86400000)))
    (let next_year ((year 1970) (days td))
      (let ((days_in_year (year->days year)))
	(if (fix.< days days_in_year)
	    (let next_month ((month 0) (days days))
	      (let ((days_in_month (yearmonth->days year month)))
		(if (fix.< days days_in_month)
		    (erl-tuple year (+ 1 month) (+ 1 days))
		    (next_month (+ 1 month) (- days days_in_month)))))
	    (next_year (+ 1 year) (- days days_in_year)))))))
    
;; element/2 (Guard BIF)
(define (erl-generic-tst-element/2 x y)
  (and (erl-fixnum? x)
       (erl-tuple? y)
       (fix.< 0 x)
       (fix.< x (erl-vector-length y))
       (erl-vector-ref y x)))

(define (erlang:element/2 x y)
  (if (and (erl-fixnum? x)
	   (erl-tuple? y))
      (if (and (fix.< 0 x)
	       (fix.< x (erl-vector-length y)))
	  (erl-vector-ref y x)
	  (erl-exit-badindex))
      (erl-exit-badarg)))

;; erase/0
(define (erlang:erase/0)
  (let ((old (erl-get/0)))
    (process-dict-set! node.current-process (erl-registry.new 'undefined))
    old))

;; erase/1
(define (erlang:erase/1 x)
  (erl-registry.remove! (process-dict node.current-process) x))

;; exit/1
(define (erlang:exit/1 x)
  ((erl-abrupt-top) (erl-tuple EXIT-atom x)))

;; exit/2
(define (erlang:exit/2 x y)
  (cond
   ((erl-pid? x) (erl-exit-signal! x y))
   ((erl-port? x) (erl-port_close/1 x)) ;; reason???
   (else (erl-exit-badarg))))

;; float/1 (Guard BIF)
(define (erl-generic-float/1 x)
  (if (erl-num? x) (exact->inexact x) (erl-exit-badarg)))
(define (erl-generic-tst-float/1 x)
  (and (erl-num? x) (exact->inexact x)))
(define (erlang:float/1 x) (erl-float/1 x))

;; float_to_list/1 *** Should be modified according to the specs
(define (erlang:float_to_list/1 x)
  (if (erl-flonum? x)
      (map char->integer (string->list (number->string x)))
      (erl-exit-badarg)))

;; get/0
(define (erlang:get/0)
  (erl-registry.to_list (process-dict node.current-process)))

;; get/1
(define (erlang:get/1 x)
  (erl-registry.lookup (process-dict node.current-process) x))

;; get_keys/1
(define (erlang:get_keys/1 x)
  (erl-registry.get_keys (process-dict node.current-process) x))

;; group_leader/0
(define (erlang:group_leader/0) (erl-group_leader/0))

;; group_leader/2
(define (erlang:group_leader/2 x y)
  (if (and (erl-pid? x)
	   (erl-pid? y))
      (begin (erl-group-leader-signal! y x)
	     'true)
      (erl-exit-badarg)))

;; hd/1 (Guard BIF)
(define (erl-generic-hd/1 x)
  (if (erl-cons? x) (erl-hd x) (erl-exit-badarg)))
(define (erlang:hd/1 x) (erl-hd/1 x))

;; integer_to_char/1 (PROPOSED BIF)
(define (erlang:integer_to_char/1 x)
  (if (and (erl-fix? x) (fix.< x 65536) (fix.< -1 x))
      (integer->char x)
      (erl-exit-badarg)))

;; integer_to_list/1
(define (erlang:integer_to_list/1 x)
  (if (erl-int? x)
      (map char->integer (string->list (number->string x)))
      (erl-exit-badarg)))

;; integer_to_string/1 (PROPOSED BIF)
(define (erlang:integer_to_string/1 x)
  (if (erl-int? x)
      (string->list (number->string x))
      (erl-exit-badarg)))

;; is_alive/0
(define (erlang:is_alive/0) node.communicating)

;; is_atom/1 (Recognizer BIF)
(define (erlang:is_atom/1 x) (erl-is_atom/1 x))
;; is_binary/1 (Recognizer BIF)
(define (erlang:is_binary/1 x) (erl-is_binary/1 x))
;; is_char/1 (Recognizer BIF)
(define (erlang:is_char/1 x) (erl-is_char/1 x))
;; is_compound/1 (Recognizer BIF)
(define (erlang:is_compound/1 x) (erl-is_compound/1 x))
;; is_cons/1 (Recognizer BIF)
(define (erlang:is_cons/1 x) (erl-is_cons/1 x))
;; is_float/1 (Recognizer BIF)
(define (erlang:is_float/1 x) (erl-is_float/1 x))
;; is_function/1 (Recognizer BIF)
(define (erlang:is_function/1 x) (erl-is_function/1 x))
;; is_integer/1 (Recognizer BIF)
(define (erlang:is_integer/1 x) (erl-is_integer/1 x))
;; is_list/1 (Recognizer BIF)
(define (erlang:is_list/1 x) (erl-is_list/1 x))
;; is_null/1 (Recognizer BIF)
(define (erlang:is_null/1 x) (erl-is_null/1 x))
;; is_number/1 (Recognizer BIF)
(define (erlang:is_number/1 x) (erl-is_number/1 x))
;; is_pid/1 (Recognizer BIF)
(define (erlang:is_pid/1 x) (erl-is_pid/1 x))
;; is_port/1 (Recognizer BIF)
(define (erlang:is_port/1 x) (erl-is_port/1 x))
;; is_ref/1 (Recognizer BIF)
(define (erlang:is_ref/1 x) (erl-is_ref/1 x))
;; is_string/1 (Recognizer BIF)
(define (erlang:is_string/1 x) (erl-is_string/1 x))
;; is_tuple/1 (Recognizer BIF)
(define (erlang:is_tuple/1 x) (erl-is_tuple/1 x))

;; length/1 (Guard BIF)
(define (erl-generic-tst-length/1 x)
  (or (and (erl-nil? x) 0)
      (and (erl-cons? x)
	   (let ((cdrlen (erl-generic-tst-length/1 (erl-tl x))))
	     (and cdrlen (fix.u+ cdrlen 1))))))
(define (erlang:length/1 x)
  (cond
   ((erl-nil? x) 0)
   ((erl-cons? x) (fix.u+ (erlang:length/1 (erl-tl x)) 1))
   (else (erl-exit-badarg))))

;; link/1
(define (erlang:link/1 x)
  (cond
   ((erl-pid? x) (if (eq? x node.current-process)
		     'true
		     (begin (process-link! node.current-process x)
			    (erl-link-signal! x)
			    'true)))
   ((erl-port? x) (begin (port-link! x (erl-self/0))
			 'true))
   (else (erl-exit-badarg))))

;; list_to_atom/1
(define (erlang:list_to_atom/1 x)
  (if (erl-unicode-list? x)
      (erl-atom<-string (list->string (map integer->char x)))
      (erl-exit-badarg)))

;; list_to_binary/1
(define (erlang:list_to_binary/1 x)
  (let loop ((newsize 0) (lst x))
    (cond ((and (erl-cons? lst)
		(erl-byte? (erl-hd lst)))
	   (loop (fix.u+ newsize 1) (erl-tl lst)))
	  ((erl-nil? lst) (let ((u8 (make-u8vector newsize)))
			    (let loop ((idx 0) (lst x))
			      (if (erl-cons? lst)
				  (begin
				    (u8vector-set! u8 idx (erl-hd lst))
				    (loop (erl-tl lst) (fix.u+ idx 1)))
				  (erl-u8vector->binary u8)))))
	  (else (erl-exit-badarg)))))

;; list_to_float/1
(define (erlang:list_to_float/1 x)
  (if (erl-unicode-list? x)
      (let ((num (string->number (list->string (map integer->char x)))))
	(if (erl-flonum? num)
	    num
	    (erl-exit-badarg)))
      (erl-exit-badarg)))

;; list_to_integer/1
(define (erlang:list_to_integer/1 x)
  (if (erl-unicode-list? x)
      (let ((num (string->number (list->string (map integer->char x)))))
	(if (erl-int? num)
	    num
	    (erl-exit-badarg)))
      (erl-exit-badarg)))

;; list_to_string/1 (PROPOSED BIF)
(define (erlang:list_to_string/1 x)
  (let loop ((lst x))
    (cond
     ((erl-cons? lst) (erl-cons (erl-integer_to_char/1 (erl-hd lst))
				(loop (erl-tl lst))))
     ((erl-nil? lst) '())
     (else (erl-exit-badarg)))))

;; list_to_tuple/1
(define (erlang:list_to_tuple/1 x)
  (if (erl-lst? x)
      (list->vector (cons 'vector x))
      (erl-exit-badarg)))

;; make_ref/0
(define (erlang:make_ref/0)
  (let ((ref (make-ref node.ref-count
		       node.name
		       node.creation)))
    (set! node.ref-count (int.+ node.ref-count 1))
    ref))

;; node/0 (Guard BIF)
(define (erlang:node/0) (erl-node/0))

;; node/1 (Guard BIF)
(define (erl-generic-tst-node/1 x)
  (and (erl-vector? x)
       (case (erl-vector-ref x 0)
	 ((pid) (erl-pid-node x))
	 ((ref) (erl-ref-node x))
	 ((port) (erl-port-node x))
	 (else #f))))
(define (erl-generic-safe-node/1 x)
  (case (erl-vector-ref x 0)
    ((pid) (erl-pid-node x))
    ((ref) (erl-ref-node x))
    (else (erl-port-node x))))
(define (erlang:node/1 x)
  (if (erl-vector? x)
      (case (erl-vector-ref x 0)
	((pid) (erl-pid-node x))
	((ref) (erl-ref-node x))
	((port) (erl-port-node x))
	(else (erl-exit-badarg)))
      (erl-exit-badarg)))

;; now/0
(define (erlang:now/0) (erl-now/0))

;; open_port/2
(define (erlang:open_port/2 x y) (erl-generic-open_port/2 x y))

;; port_close/1
(define (erlang:port_close/1 x) (erl-generic-port_close/1 x))

;; port_info/1
(define (erlang:port_info/1 x)
  ;; could have make it using port_info/2, but it's less efficient...
  (if (erl-port? x)
      (erl-list (erl-tuple 'connected (erl-port-owner x))
		(erl-tuple 'id (erl-port-id x))
		(erl-tuple 'input 'unknown) ; **** to be corrected
		(erl-tuple 'links (erl-port-linked-pids x))
		(erl-tuple 'name 'unknown) ; **** to be corrected
		(erl-tuple 'output 'unknown)) ; **** to be corrected
      (erl-exit-badarg)))

;; port_info/2
(define (erlang:port_info/2 x y)
  (if (erl-port? x)
      (case y
	((connected) (erl-tuple 'connected (erl-port-owner x)))
	((id) (erl-tuple 'id (erl-port-id x)))
	((input) (erl-tuple 'input 'unknown)) ; **** to be corrected
	((links) (erl-tuple 'links (erl-port-linked-pids x)))
	((name) (erl-tuple 'input 'unknown)) ; **** to be corrected
	((output) (erl-tuple 'input 'unknown)) ; **** to be corrected
	(else (erl-exit-badarg)))
      (erl-exit-badarg)))

;; ports/0
(define (erlang:ports/0) (erl-generic-ports/0))

;; process_info/2
(define (erlang:process_info/2 x y)
  (if (and (erl-pid? x) (erl-atom? y))
      (erl-info-signal x y)
      (erl-exit-badarg)))

;; process_flag/2
(define (erlang:process_flag/2 x y)
  (cond
    ((and (eq? x 'trap_exit) (erl-boolean? y))
     (let ((old (process-trap-exit node.current-process)))
       (process-trap-exit-set! node.current-process y)
       old))
    ((and (eq? x 'error_handler) (erl-atom? y))
     (let ((old (process-error-handler node.current-process)))
       (process-error-handler-set! node.current-process y)
       old))
    ((and (eq? x 'priority)
	  (or (eq? y 'low)
	      (eq? y 'normal)
	      (eq? y 'high)))
     (let ((old (process-priority node.current-process)))
       (process-change-priority! node.current-process y)
       old))
    (else (erl-exit-badarg))))

;; processes/0
(define (erlang:processes/0)
  (with-no-interrupts
   (lambda ()
     (erl-append (pqueue->pid-list node.timeout-pqueue)
		 (pqueue->pid-list node.ready-pqueue)
		 (pqueue->pid-list node.waiting-pqueue)))))

;; put/2
(define (erlang:put/2 x y)
  (erl-registry.assign! (process-dict node.current-process) x y))

;; register/2
(define (erlang:register/2 x y)
  (if (and (erl-atom? x)
	   (erl-pid? y))
      (if (and (eq? (erl-pid-node y) node.name)
	       (not (eq? (process-state (erl-pid-process y)) 'dead))
	       (eq? (erl-registry.lookup node.registry x) undefined-atom))
	  (begin (erl-registry.assign! x y)
		 'true)
	  (erl-exit-registry))
      (erl-exit-badarg)))

;; registered/0
(define (erlang:registered/0)
  (let ((lst (erl-registry.to_list node.registry)))
    (map (lambda (tup) (erl-vector-ref tup 2)) lst)))

;; round/1 (Guard BIF)
(define (erl-generic-round/1 x)
  (if (erl-num? x) (inexact->exact (round x)) (erl-exit-badarg)))
(define (erl-generic-tst-round/1 x)
  (and (erl-num? x) (inexact->exact (round x))))
(define (erlang:round/1 x) (erl-round/1 x))

;; self/0 (Guard BIF)
(define (erlang:self/0) (erl-self/0))

;; setelement/3
(define (erlang:setelement/3 x y z)
  (if (and (erl-fixnum? x)
	   (erl-tuple? y))
      (if (and (fix.< 0 x)
	       (fix.< x (erl-vector-length y)))
	  (let* ((n (erl-vector-length y))
		 (v (erl-make-vector n)))
	    (let loop ((i (fix.u- n 1)))
	      (if (fix.< i 0)
		  (begin
		    (vector-set! v x z)
		    v)
		  (begin
		    (vector-set! v i (erl-vector-ref y i))
		    (loop (fix.u- i 1))))))
	  (erl-exit-badindex))
      (erl-exit-badarg)))

;; sign/1 (Guard BIF)
(define (erl-generic-sign/1 x)
  (if (erl-num? x) (if (< x 0) 1 0) (erl-exit-badarg)))
(define (erl-generic-tst-sign/1 x)
  (and (erl-num? x) (if (< x 0) 1 0)))
(define (erl-generic-safe-sign/1 x)
  (if (< x 0) 1 0))
(define (erlang:sign/1 x) (erl-sign/1 x))

;; size/1 (Guard BIF)
(define (erl-generic-tst-size/1 x)
  (and (erl-vector? x)
       (case (erl-vector-ref x 0)
	 ((tuple) (erl-tuple-size x))
	 ((binary) (erl-binary-size x))
	 (else #f))))
(define (erl-generic-safe-size/1 x)
  (if (eq? (erl-vector-ref x 0) 'tuple)
      (erl-tuple-size x)
      (erl-binary-size x)))
(define (erlang:size/1 x)
  (if (erl-vector? x)
      (case (erl-vector-ref x 0)
	((tuple) (erl-tuple-size x))
	((binary) (erl-binary-size x))
	(else (erl-exit-badarg)))
      (erl-exit-badarg)))

;; spawn/3
(define (erlang:spawn/3 x y z)
  (process-spawn!
   (erl-tuple x y (erl-length/1 z))
   (lambda () (erl-apply/3 x y z))
   #f))

;; spawn_link/3
(define (erlang:spawn_link/3 x y z)
  (process-spawn!
   (erl-tuple x y (erl-length/1 z))
   (lambda () (erl-apply/3 x y z))
   #t))

;; split_binary/2
(define (erlang:split_binary/2 b n)
  (or (and (erl-binary? b)
	   (erl-fixnum? n)
	   (let ((u8 (erl-binary-u8vector b))
		 (siz (erl-binary-size b)) 
		 (off (erl-binary-offset b)))
	     (and (not (fix.< siz n))
		  (fix.< -1 n)
		  (erl-tuple
		   (erl-make-binary u8 off n)
		   (erl-make-binary u8 (fix.u+ off n) (fix.u- siz n))))))
      (erl-exit-badarg)))

;; statistics/1
(define (erlang:statistics/1 x)
  (cond ((eq? x runtime-atom)
	 (let* ((l node.last-runtime)
		(t (erl-runtime-msecs!)))
	   (erl-tuple t (int.- t l))))
	((eq? x run_queue-atom) (pqueue-count node.ready-pqueue))
	(else (erl-exit-badarg))))

;; string_to_list/1 (PROPOSED BIF)
(define (erlang:string_to_list/1 x)
  (let loop ((str x))
    (cond
     ((erl-cons? str) (let ((hd (erl-hd str)))
			(if (erl-char? hd)
			    (erl-cons (char->integer hd)
				      (loop (erl-tl str)))
			    (erl-exit-badarg))))
     ((erl-nil? str) '())
     (else (erl-exit-badarg)))))

;; throw/1
(define (erlang:throw/1 x)
  ((erl-abrupt-top) (erl-tuple THROW-atom x)))

;; time/0
(define (erlang:time/0)
  (let ((rts (quotient (current-time-in-msecs) 1000)))
    (erl-tuple (modulo (quotient rts 3600) 24)
	       (modulo (quotient rts 60) 60)
	       (modulo rts 60))))

;; tl/1 (Guard BIF)
(define (erl-generic-tl/1 x)
  (if (erl-cons? x) (erl-tl x) (erl-exit-badarg)))
(define (erlang:tl/1 x) (erl-tl/1 x))

;; trunc/1 (Guard BIF)
(define (erl-generic-trunc/1 x)
  (if (erl-num? x) (inexact->exact (truncate x)) (erl-exit-badarg)))
(define (erl-generic-tst-trunc/1 x)
  (and (erl-num? x) (inexact->exact (truncate x))))
(define (erlang:trunc/1 x) (erl-trunc/1 x))

;; tuple_to_list/1
(define (erlang:tuple_to_list/1 x)
  (if (erl-tuple? x)
      (cdr (vector->list x))
      (erl-exit-badarg)))

;; unlink/1
(define (erlang:unlink/1 x)
  (cond
   ((erl-pid? x) (if (eq? x node.current-process)
		     'true
		     (begin (process-unlink! node.current-process x)
			    (erl-unlink-signal! x)
			    'true)))
   ((erl-port? x) (begin (port-unlink! x (erl-self/0))
			 'true))
   (else (erl-exit-badarg))))

;; unregister/1
(define (erlang:unregister/1 x)
  (if (erl-atom? x)
      (begin (erl-registry.remove! node.registry x)
	     'true)
      (erl-exit-badarg)))

;; whereis/1
(define (erlang:whereis/1 x)
  (if (erl-atom? x)
      (erl-registry.lookup node.registry x)
      (erl-exit-badarg)))

;;;;;;;;;;;;;;;;;;;;
;; Operators 'BIFs'

;; +/1 (Guard BIF)

;; -/1 (Guard BIF)

;; bnot/1 (Guard BIF)

;; not/1 (Guard BIF)
(define (erl-generic-not/1 x)
  (cond
   ((eq? x 'true) 'false)
   ((eq? x 'false) 'true)
   (else (erl-exit-badbool))))
(define (erl-generic-tst-not/1 x)
  (or (and (eq? x 'true) 'false)
      (and (eq? x 'false) 'true)))
(define (erl-generic-safe-not/1 x)
  (if (eq? x 'true) 'false 'true))

;; +/2 (Guard BIF)
(define (erl-generic-+/2 x y)
  (if (and (erl-num? x) (erl-num? y))
      (+ x y)
      (erl-exit-badarg)))
(define (erl-generic-tst-+/2 x y)
  (and (erl-num? x) (erl-num? y) (+ x y)))

;; -/2 (Guard BIF)
(define (erl-generic--/2 x y)
  (if (and (erl-num? x) (erl-num? y))
      (- x y)
      (erl-exit-badarg)))
(define (erl-generic-tst--/2 x y)
  (and (erl-num? x) (erl-num? y) (- x y)))

;; bor/2 (Guard BIF)
(define (erl-generic-bor/2 x y)
  (if (and (erl-int? x) (erl-int? y))
      (int.bor x y)
      (erl-exit-badarg)))
(define (erl-generic-tst-bor/2 x y)
  (and (erl-int? x) (erl-int? y) (int.bor x y)))

;; bxor/2 (Guard BIF)
(define (erl-generic-bxor/2 x y)
  (if (and (erl-int? x) (erl-int? y))
      (int.bxor x y)
      (erl-exit-badarg)))
(define (erl-generic-tst-bxor/2 x y)
  (and (erl-int? x) (erl-int? y) (int.bxor x y)))


;; bsl/2 (Guard BIF)
(define (erl-generic-bsl/2 x y)
  (if (and (erl-int? x) (erl-int? y))
      (if (int.< y 0)
	  (int.bsr x (int.- 0 y))
	  (int.bsl x y))
      (erl-exit-badarg)))
(define (erl-generic-tst-bsl/2 x y)
  (and (erl-int? x)
       (erl-int? y)
       (if (int.< y 0)
	   (int.bsr x (int.- 0 y))
	   (int.bsl x y))))

;; bsr/2 (Guard BIF)
(define (erl-generic-bsr/2 x y)
  (if (and (erl-int? x) (erl-int? y))
      (if (int.< y 0)
	  (int.bsl x (int.- 0 y))
	  (int.bsr x y))
      (erl-exit-badarg)))
(define (erl-generic-tst-bsr/2 x y)
  (and (erl-int? x)
       (erl-int? y)
       (if (int.< y 0)
	   (int.bsl x (int.- 0 y))
	   (int.bsr x y))))

;; */2 (Guard BIF)
(define (erl-generic-*/2 x y)
  (if (and (erl-num? x) (erl-num? y))
      (* x y)
      (erl-exit-badarg)))
(define (erl-generic-tst-*/2 x y)
  (and (erl-num? x) (erl-num? y) (* x y)))


;; //2 (Guard BIF)
(define (erl-generic-//2 x y)
  (flo./ (erl-float/1 x) (erl-float/1 y)))
(define (erl-generic-tst-//2 x y)
  (and (erl-num? x)
       (erl-num? y)
       (flo./ (erl-safe-float/1 x) (erl-safe-float/1 y))))
(define (erl-generic-safe-//2 x y)
  (flo./ (erl-safe-float/1 x) (erl-safe-float/1 y)))

;; ///2 (Guard BIF)
(define (erl-generic-///2 x y)
  (if (and (erl-int? x) (erl-int? y))
      (if (erl-fix=k y 0)
	  undefined-atom
	  (floor (/ x y)))
      (erl-exit-badarg)))
(define (erl-generic-tst-///2 x y)
  (and (erl-int? x)
       (erl-int? y)
       (if (erl-fix=k y 0)
	   undefined-atom
	   (floor (/ x y)))))
(define (erl-generic-safe-///2 x y)
  (if (erl-fix=k y 0)
      undefined-atom
      (floor (/ x y))))

;; div/2 (Guard BIF)
(define (erl-generic-div/2 x y)
  (if (and (erl-int? x) (erl-int? y))
      (if (erl-fix=k y 0)
	  undefined-atom
	  (quotient x y))
      (erl-exit-badarg)))
(define (erl-generic-tst-div/2 x y)
  (and (erl-int? x)
       (erl-int? y)
       (if (erl-fix=k y 0)
	   undefined-atom
	   (quotient x y))))
(define (erl-generic-safe-div/2 x y)
  (if (erl-fix=k y 0)
      undefined-atom
      (quotient x y)))

;; mod/2 (Guard BIF)
(define (erl-generic-mod/2 x y)
  (if (and (erl-int? x) (erl-int? y))
      (if (erl-fix=k y 0)
	  undefined-atom
	  (modulo x y))
      (erl-exit-badarg)))
(define (erl-generic-tst-mod/2 x y)
  (and (erl-int? x)
       (erl-int? y)
       (if (erl-fix=k y 0)
	   undefined-atom
	   (modulo x y))))
(define (erl-generic-safe-mod/2 x y)
  (if (erl-fix=k y 0)
      undefined-atom
      (modulo x y)))

;; rem/2 (Guard BIF)
(define (erl-generic-rem/2 x y)
  (if (and (erl-int? x) (erl-int? y))
      (if (erl-fix=k y 0)
	  undefined-atom
	  (remainder x y))
      (erl-exit-badarg)))
(define (erl-generic-tst-rem/2 x y)
  (and (erl-int? x)
       (erl-int? y)
       (if (erl-fix=k y 0)
	   undefined-atom
	   (remainder x y))))
(define (erl-generic-safe-rem/2 x y)
  (if (erl-fix=k y 0)
      undefined-atom
      (remainder x y)))

;; band/2 (Guard BIF)
(define (erl-generic-band/2 x y)
  (if (and (erl-int? x) (erl-int? y))
      (int.band x y)
      (erl-exit-badarg)))
(define (erl-generic-tst-band/2 x y)
  (and (erl-int? x) (erl-int? y) (int.band x y)))

;; ++/2
(define (erl-generic-++/2 x y)
  (cond
   ((erl-cons? x) (erl-cons (erl-hd x)
			    (erl-generic-++/2 (erl-tl x) y)))
   ((erl-nil? x) y)
   (else (erl-exit-badarg))))
(define (erl-generic-safe-++/2 x y)
  (if (erl-nil? x)
      y
      (erl-cons (erl-hd x)
		(erl-generic-safe-++/2 (erl-tl x) y))))

;; --/2
(define (erl-generic---/2 x y)
  (define (erl-remove-first-occ elt lst)
    (cond
     ((erl-cons? lst)
      (let ((hd (erl-hd lst)))
	(if (erl-=:=/2 elt hd)
	    (erl-tl lst)
	    (erl-cons hd (erl-remove-first-occ elt (erl-tl lst))))))
     ((erl-nil? lst) lst)
     (else (erl-exit-badarg))))
  (define (erl-safe-remove-first-occ elt lst)
    (if (erl-nil? lst)
	lst
	(let ((hd (erl-hd lst)))
	  (if (erl-=:=/2 elt hd)
	      (erl-tl lst)
	      (erl-cons hd (erl-safe-remove-first-occ elt (erl-tl lst)))))))
  (cond
   ((erl-cons? y)
    (let ((yn (erl-hd y)))
      (let loop ((x (erl-remove-first-occ yn x)) (y (erl-tl y)))
	(cond
	 ((erl-cons? y)
	  (let ((yn (erl-hd y)))
	    (loop (erl-safe-remove-first-occ yn x) (erl-tl y))))
	 ((erl-nil? y) x)
	 (else (erl-exit-badarg))))))
   ((erl-nil? y) x)
   (else (erl-exit-badarg))))

;; or/2
(define (erl-generic-or/2 x y)
  (if (erl-ato=k x 'true)
      x
      (if (erl-ato=k x 'false)
	  (if (erl-boolean? y)
	      y
	      (erl-exit-badbool))
	  (erl-exit-badbool))))

;; xor/2
(define (erl-generic-xor/2 x y)
  (if (erl-ato=k x 'true)
      (if (erl-ato=k y 'true)
	  'false
	  (if (erl-ato=k y 'false)
	      x
	      (erl-exit-badbool)))
      (if (erl-ato=k x 'false)
	  (if (erl-boolean? y)
	      y
	      (erl-exit-badbool))
	  (erl-exit-badbool))))

;; and/2
(define (erl-generic-and/2 x y)
  (if (erl-ato=k x 'true)
      (if (erl-boolean? y)
	  y
	  (erl-exit-badbool))
      (if (erl-ato=k x 'false)
	  x
	  (erl-exit-badbool))))

;; send/2
(define (erl-generic-send/2 x y)
  (cond
   ((erl-pid? x) (begin (erl-message-signal! x y)
			y))
   ((erl-port? x) (begin (erl-port-send! x y)
			 y))
   (else (erl-exit-badarg))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Semi BIFS (Functions part of the erlang module)

;; check_process_code/2
;; Always false(may always load a new version)
(define (erlang:check_process_code/2 x y)
  (if (and (erl-pid? x) (erl-atom? y))
      'false
      (erl-exit-badarg)))

;; delete_module/1
;; Always true and no effect
(define (erlang:delete_module/1 x)
  (if (erl-atom? x)
      'true
      (erl-exit-badarg)))

;; etos_rootdir/0 (ETOS SPECIFIC BIF)
;; returns the root directory of ETOS, as set by environment variable ETOS_ROOT
;; defaults to "/usr/local/lib/etos"
(define (erlang:etos_rootdir/0)
  (let ((rd (getenv "ETOS_ROOT")))
    (if (string? rd)
	(string->list (string-append rd "/"))
	(string->list "/usr/local/lib/etos/"))))

;; get_cookie/0
(define (erlang:get_cookie/0) node.magic_cookie)

;; halt/0
(define (erlang:halt/0)
  (exit 0))

;; hash/2
(define (erlang:hash/2 x y)
  (if (and (erl-int? y) (int.< -1 y))
      (int.+ (modulo (hash x 0) y) 1)
      (erl-exit-badarg)))

;; load_module/2 (ETOS SPECIFIC VERSION)
;; the second argument is a string representing the full path
;; to the module precompiled code
;; **** Should handle errors more gracefully and return {error,module}
(define (erlang:load_module/2 x y)
  (if (and (erl-atom? x) (erl-str? y))
      (begin
	(load (erl-string->string y))
	(if (not (memq x node.modules_loaded))
	    (set! node.modules_loaded (cons x node.modules_loaded)))
	'true)
      (erl-exit-badarg)))

(define (erl-install-module module-name export-table install uninstall)
  (install))

;; m_acos/1
(define (erlang:m_acos/1 x) (flo.acos (erl-float/1 x)))

;; m_acosh/1
(define (erlang:m_acosh/1 x)
  (let ((a (erl-float/1 x)))
    (if (flo.< a 1)
	(erl-exit-badarg)
	(flo.acosh a))))

;; m_asin/1
(define (erlang:m_asin/1 x) (flo.asin (erl-float/1 x)))

;; m_asinh/1
(define (erlang:m_asinh/1 x) (flo.asinh (erl-float/1 x)))

;; m_atan/1
(define (erlang:m_atan/1 x) (flo.atan (erl-float/1 x)))

;; m_atan2/2
(define (erlang:m_atan2/2 x y) (flo.atan2 (erl-float/1 x) (erl-float/1 y)))

;; m_atanh/1
(define (erlang:m_atanh/1 x) (flo.atanh (erl-float/1 x)))

;; m_cos/1
(define (erlang:m_cos/1 x) (flo.cos (erl-float/1 x)))

;; m_cosh/1
(define (erlang:m_cosh/1 x) (flo.cosh (erl-float/1 x)))

;; m_erf/1
(define (erlang:m_erf/1 x) (flo.erf (erl-float/1 x)))

;; m_erfc/1
(define (erlang:m_erfc/1 x) (flo.erfc (erl-float/1 x)))

;; m_exp/1
(define (erlang:m_exp/1 x) (flo.exp (erl-float/1 x)))

;; m_log/1
(define (erlang:m_log/1 x) (flo.log (erl-float/1 x)))

;; m_log10/1
(define (erlang:m_log10/1 x) (flo.log10 (erl-float/1 x)))

;; m_pow/2
(define (erlang:m_pow/2 x y) (flo.pow (erl-float/1 x) (erl-float/1 y)))

;; m_sin/1
(define (erlang:m_sin/1 x) (flo.sin (erl-float/1 x)))

;; m_sinh/1
(define (erlang:m_sinh/1 x) (flo.sinh (erl-float/1 x)))

;; m_sqrt/1
(define (erlang:m_sqrt/1 x) (flo.sqrt (erl-float/1 x)))

;; m_tan/1
(define (erlang:m_tan/1 x) (flo.tan (erl-float/1 x)))

;; m_tanh/1
(define (erlang:m_tanh/1 x) (flo.tanh (erl-float/1 x)))

;; module_loaded/1
(define (erlang:module_loaded/1 x)
  (if (erl-atom? x)
      (if (memq x node.modules_loaded)
	  'true
	  'false)
      (erl-exit-badarg)))

;; preloaded/0
;; no modules are preloaded for now in Etos
(define (erlang:preloaded/0) (erl-nil))

;; purge_module/1
;; nothing is done here since Gambit will handle this automatically(TODO)
(define (erlang:purge_module/1 x)
  (if (erl-atom? x)
      'true
      (erl-exit-badarg)))

;; set_cookie/2
(define (erlang:set_cookie/2 x y)
  (if (and (erl-atom? x)
	   (erl-atom? y)
	   (erl-valid-node-name? x))
      (begin
	(if (eq? x node.name)
	    (begin
	      (set! node.magic_cookie y)
	      (let loop! ((ncs (erl-registry.to_list node.magic_cookies)))
		(if (erl-cons? ncs)
		    (let ((nc (erl-hd ncs)))
		      (if (eq? (erl-vector-ref nc 2) 'nocookie)
			  ;;(erl-vector-set! nc 2 y)
			  ;; this is more safe, but slower
			  ;; if we don't change to atom registries, change this
			  (erl-registry.assign! node.magic_cookies
						(erl-vector-ref nc 1) y))
		      (loop! (erl-tl ncs))))))
	    (erl-registry.assign! node.magic_cookies x y))
	'true)
      (erl-exit-badarg)))


;;;;;;;;;;;;;;;;;;;;
;; Some modules BIFs

(define (erl-default-abrupt-handler reason)
  (io:write/1 (erl-tuple 'unhandled_abrupt_completion reason))
  (io:nl/0)
  (process-exit-hook-set!
   node.current-process
   (lambda ()
     (process-die! node.current-process
		   (let ((subr (erl-tuple-ref reason 1)))
		     (if (erl-ato=k subr EXIT-atom)
			 (erl-tuple-ref reason 2)
			 'nocatch)))))
  (continuation-restore node.initial-cont 'dummy))

(define (io:write/1 x)
  (define (write-tail lst close)
    (cond ((erl-nil? lst)
	   (display close))
	  ((erl-con? lst)
	   (display ",")
	   (io:write/1 (erl-hd lst))
	   (write-tail (erl-tl lst) close))
	  (else
	   (display "|")
	   (io:write/1 lst)
	   (display close))))
  (cond
   ((erl-fix? x) (display x))
   ((erl-con? x) (if (erl-str? x)
		     (begin (display "\"")
			    (for-each display x)
			    (display "\""))
		     (begin
		       (display "[")
		       (io:write/1 (erl-hd x))
		       (write-tail (erl-tl x) "]"))))
   ((erl-spc? x) (cond
		  ((erl-nil? x) (display "[]"))
		  ((erl-chr? x) (display "$") (display (if (eq? x #\newline)
							   "\n"
							   x)))
		  (else (display "#UnknownTypeSpc: ")(pp x))))
   (else (cond
 	  ((or (erl-big? x) (erl-flo? x) (erl-ato? x)) (display x))
 	  (else
 	   (case (erl-vector-ref x 0)
 	     ((tuple)
 	      (let ((lst (cdr (vector->list x))))
 		(if (erl-nil? lst)
 		    (display "{}")
 		    (begin
		      (display "{")
 		      (io:write/1 (erl-hd lst))
 		      (write-tail (erl-tl lst) "}")))))
 	     ((function) (display "#Fun"))
 	     ((port) (display "#Port"))
 	     ((pid) (let ((id (erl-pid-id x))
			  (cr (erl-pid-creation x)))
		      (display "<")
		      (write (quotient id (* 65536 65536)))
		      (display ".")
		      (write (modulo id (* 65536 65536)))
		      (display ".")
		      (write cr)
		      (display ">")))
 	     ((ref) (display "#Ref"))
 	     ((binary) (display "#Bin"))
	     (else (display "#UnknownTypeVec"))))))))

(define (io:format/1 x)
  (for-each write-char x))

(define (io:nl/0)
  (newline))

(define (math:sqrt/1 x) (erl-generic-sqrt x))
(define (math:sin/1 x) (erl-generic-sin x))
(define (math:cos/1 x) (erl-generic-cos x))
(define (math:tan/1 x) (erl-generic-tan x))
(define (math:atan/1 x) (erl-generic-atan x))

(define (erl-generic-sqrt x)
  (cond
   ((erl-fix? x) (flo.sqrt (exact->inexact x)))
   ((erl-sub? x) (cond
		  ((erl-flo? x) (flo.sqrt x))
		  ((erl-big? x) (flo.sqrt (exact->inexact x)))
		  (else (erl-exit-badarg))))
   (else (erl-exit-badarg))))

(define (erl-generic-sin x)
  (cond
   ((erl-fix? x) (flo.sin (exact->inexact x)))
   ((erl-sub? x) (cond
		  ((erl-flo? x) (flo.sin x))
		  ((erl-big? x) (flo.sin (exact->inexact x)))
		  (else (erl-exit-badarg))))
   (else (erl-exit-badarg))))

(define (erl-generic-cos x)
  (cond
   ((erl-fix? x) (flo.cos (exact->inexact x)))
   ((erl-sub? x) (cond
		  ((erl-flo? x) (flo.cos x))
		  ((erl-big? x) (flo.cos (exact->inexact x)))
		  (else (erl-exit-badarg))))
   (else (erl-exit-badarg))))

(define (erl-generic-tan x)
  (cond
   ((erl-fix? x) (flo.tan (exact->inexact x)))
   ((erl-sub? x) (cond
		  ((erl-flo? x) (flo.tan x))
		  ((erl-big? x) (flo.tan (exact->inexact x)))
		  (else (erl-exit-badarg))))
   (else (erl-exit-badarg))))

(define (erl-generic-atan x)
  (cond
   ((erl-fix? x) (flo.atan (exact->inexact x)))
   ((erl-sub? x) (cond
		  ((erl-flo? x) (flo.atan x))
		  ((erl-big? x) (flo.atan (exact->inexact x)))
		  (else (erl-exit-badarg))))
   (else (erl-exit-badarg))))


(define (internal-error!!!! . msgs)
  (display "Internal-error!!!!") (newline)
  (for-each display msgs) (newline)
  (erl-halt/0))

;; For compilation without warning... to be implemented.
(define (int.bor x y) (internal-error!!!! "int.bor not implemented"))
(define (int.bxor x y) (internal-error!!!! "int.bxor not implemented"))
(define (int.band x y) (internal-error!!!! "int.band not implemented"))

(define (erl-remote-apply f . args)
  (let ((gvref (##global-var-ref f)))
    (if (procedure? gvref)
	(apply gvref args)
	(erl-exit-badarg)))) ; to be corrected...

(define (erl-fun-apply fun arit . args)
  (if (erl-function? fun)
      (let ((a (erl-function-arity fun))
	    (l (erl-function-lambda fun)))
	(if (fix.= a arit)
	    (apply l args)
	    (erl-exit-badarit)))
      (erl-exit-badarg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The error_handler module
(define (error_handler:undefined_function/3 m f args)
  (erl-exit/1 (erl-tuple 'undef (erl-tuple m f args))))

(define (erl-undefined-function-handler args mod fun)
  (let ((process_handler (erl-function-ref
                          (erl-mfa->fname (erl-error_handler)
                                          'undefined_function
                                          3))))
    (if process_handler
        (process_handler mod fun args)
        (error_handler:undefined_function/3 mod fun args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; List comprehension utils
(define (erl-flatmap f l)
  (if (erl-nil? l)
      l
      (erl-append (f (erl-hd l))
		  (erl-flatmap f (erl-tl l)))))


;;;;;;;;;;;;
;; Processes

(define (erl-runtime-msecs!)
  (set! node.last-runtime (current-cputime-in-msecs))
  node.last-runtime)
  
(define (process-setup! initial-call thunk)
  (setup-time!)
  (continuation-save!
   (vector #f #f #f)
   (lambda (cont)
     (set! node.initial-cont cont)
     (process-spawn!
      initial-call
      thunk
      #f)
     (setup-timer-interrupt! process-end-of-quantum!)
     (process-schedule-next!)))
  ((process-exit-hook node.current-process))
  (cleanup-timer-interrupt!))

;; Process activation expired
(define (process-end-of-quantum!)
  (with-no-interrupts
   (lambda ()
     (ports-check-io!) ; check ports for i/o
     (let loop () ; wake up processes that have timed-out
       (let ((p (pqueue-head node.timeout-pqueue)))
	 (if (and p (not (int.< (current-time-in-msecs) (process-wake-up p))))
	     (begin
	       (pqueue-move-to-tail! node.ready-pqueue p)
	       (loop)))))
     (timer-interrupt-enable!))))

(define (process-spawn! initial-call thunk link?)

  (define (exit-hook)
    (process-exit-hook-set! node.current-process (lambda () #f))
    (thunk)
    (process-die! node.current-process normal-atom))

  (let ((process
         (make-process 'ready              ; state
                       node.initial-cont   ; continuation
                       (make-queue)        ; mailbox
                       #f                  ; mailbox-probe
                       #f                  ; wake-up
                       'false              ; trap-exit
                       #f                  ; pid
                       '()                 ; linked pids
                       #f                  ; group-leader
                       'error_handler      ; error-handler
                       node.name           ; node
                       'normal             ; priority
                       (vector #f #f #f)   ; mailbox-probe-cont
                       #f                  ; abrupt-stack
                       (erl-registry.new 'undefined)  ; dict
                       initial-call        ; initial call(MFA)
                       exit-hook           ; exit-hook
                       )))
    (let ((pid (make-pid process
                         node.process-count
                         node.name
                         node.creation)))
      (set! node.process-count (int.+ node.process-count 1))
      (process-pid-set! process pid)
      (process-group-leader-set! process
                                 (if node.current-process
                                     (erl-group_leader/0)
                                     pid))
      (process-abrupt-stack-set! process (list erl-default-abrupt-handler))
      (if link?
          (begin (process-link! node.current-process pid)
                 (process-link! process (erl-self/0))))
      (pqueue-move-to-tail! node.ready-pqueue process)
      pid)))


(define (process-get-property process prop)
  (if (eq? (process-state process) 'dead)
      'undefined
      (case prop
	((current_function) (erl-tuple 'current_function 'undefined))
	((dictionary) (erl-tuple 'dictionary (erl-get/0)))
	((error_handler) (erl-tuple 'error_handler
				    (process-error-handler process)))
	((group_leader) (erl-tuple 'group_leader
				   (process-group-leader process)))
	((heap_size) (erl-tuple 'heap_size 'undefined))
	((initial_call) (erl-tuple 'initial_call
				   (process-initial-call process)))
	((links) (erl-tuple 'links (process-linked-pids process)))
	((memory) (erl-tuple 'memory 'undefined))
	((message_queue_len) (erl-tuple 'message_queue_len 'undefined))
	((messages) (erl-tuple 'messages 'undefined))
	((error_priority) (erl-tuple 'priority (process-priority process)))
	((reductions) (erl-tuple 'reductions 'undefined))
	((registered_name) (erl-tuple 'registered_name 'undefined))
	((stack_size) (erl-tuple 'stack_size 'undefined))
	((status) (erl-tuple 'status (process-state process)))
	((trap_exit) (erl-tuple 'trap_exit (process-trap-exit process)))
	(else 'undefined))))


;;;;;;;;;;;;;;;;;;
;; Process queues

(define (make-pqueue)
  (make-process #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f))

(define (pqueue-empty? pqueue)
  (eq? (process-succ pqueue) pqueue))

(define (pqueue-count pqueue)
  (with-no-interrupts
   (lambda ()
     (let loop ((n 0) (succ (process-succ pqueue)))
       (if (eq? succ pqueue)
	   n
	   (loop (fix.u+ n 1) (process-succ succ)))))))

(define (pqueue->pid-list pqueue)
  (with-no-interrupts
   (lambda ()
     (let loop ((lst (erl-nil)) (succ (process-succ pqueue)))
       (if (eq? succ pqueue)
	   lst
	   (loop (cons (process-pid pqueue) lst)
		 (process-succ succ)))))))

(define (pqueue-head pqueue)
  (let ((head (process-succ pqueue)))
    (if (eq? head pqueue)
	#f
	head)))

(define (pqueue-remove! process)
  (with-no-interrupts
   (lambda ()
     (let ((s (process-succ process))
	   (p (process-prev process)))
       (process-succ-set! p s)
       (process-prev-set! s p)))))

(define (pqueue-move-to-head! pqueue process)
  (with-no-interrupts
   (lambda ()
     (let ((s (process-succ process))
	   (p (process-prev process)))
       (process-succ-set! p s)
       (process-prev-set! s p)
       (let ((head (process-succ pqueue)))
	 (process-succ-set! process head)
	 (process-prev-set! process pqueue)
	 (process-succ-set! pqueue process)
	 (process-prev-set! head process))))))

(define (pqueue-move-to-tail! pqueue process)
  (with-no-interrupts
   (lambda ()
     (let ((s (process-succ process))
	   (p (process-prev process)))
       (process-succ-set! p s)
       (process-prev-set! s p)
       (let ((tail (process-prev pqueue)))
	 (process-prev-set! process tail)
	 (process-succ-set! process pqueue)
	 (process-prev-set! pqueue process)
	 (process-succ-set! tail process))))))

(define (pqueue-move-ordered! pqueue process)
  (with-no-interrupts
   (lambda ()
     (let loop ((x (process-succ pqueue)))
       (if (or (eq? x pqueue)
	       (int.< (process-wake-up process) (process-wake-up x)))
	   (pqueue-move-to-tail! x process)
	   (loop (process-succ x)))))))

;;;;;;;;;;;;;;;;;;;;;
;; Process operations
(define timer-interrupt-handler #f)
(define timer-interrupt-allowed? #f)
(define (setup-timer-interrupt! handler)
  (set! timer-interrupt-handler handler)
  (timer-interrupt-disable!)
  (add-timer-interrupt-job
   (lambda ()
     (with-no-interrupts
      (lambda ()
	(if timer-interrupt-allowed?
	    (begin
	      (timer-interrupt-disable!)
	      (timer-interrupt-handler))))))))

;(##add-timer-interrupt-job
; (lambda ()
;   (display "count:")
;   (pp node.process-count)
;   (display "timer-interrupts-allowed?:")
;   (pp timer-interrupt-allowed?)))

(define (process-die! process reason)
  ;; *** for debugging
;  (io:write/1 (erl-tuple 'process_died reason))
;  (io:nl/0)
  (let ((msg (erl-tuple 'EXIT (process-pid process) reason)))
    (for-each (lambda (pid) (erl-exit-signal! pid msg))
	      (process-linked-pids process))
    (timer-interrupt-disable!)
    (pqueue-remove! process)
    (process-state-set! process 'dead)
    (process-schedule-next!)))

(define (process-suspend! state)
  (with-no-interrupts
   (lambda ()
     (process-state-set! node.current-process state)
     (cond ((eq? state 'ready)
	    (pqueue-move-to-tail! node.ready-pqueue node.current-process))
	   ((process-wake-up node.current-process)
	    (pqueue-move-ordered! node.timeout-pqueue node.current-process))
	   (else
	    (pqueue-move-to-tail! node.waiting-pqueue node.current-process)))
     (continuation-save!
      (vector #f #f #f)
      (lambda (cont)
        (process-continuation-set! node.current-process cont)
	(process-schedule-next!))))))

(define (process-suspend-waiting!)
  (with-no-interrupts
   (lambda ()
     (process-state-set! node.current-process 'waiting)
     (cond ((process-wake-up node.current-process)
	    (pqueue-move-ordered! node.timeout-pqueue node.current-process))
	   (else
	    (pqueue-move-to-tail! node.waiting-pqueue node.current-process)))
     (continuation-save!
      (vector #f #f #f)
      (lambda (cont)
        (process-continuation-set! node.current-process cont)
	(process-schedule-next!))))))

(define (process-suspend-waiting-with-continuation! cont)
  (with-no-interrupts
   (lambda ()
     (process-state-set! node.current-process 'waiting)
     (cond ((process-wake-up node.current-process)
	    (pqueue-move-ordered! node.timeout-pqueue node.current-process))
	   (else
	    (pqueue-move-to-tail! node.waiting-pqueue node.current-process)))
     (process-continuation-set! node.current-process cont)
     (process-schedule-next!))))

(define (process-schedule-next!)
  (with-no-interrupts
   (lambda ()
     (if (pqueue-empty? node.ready-pqueue)
	 (if (and (pqueue-empty? node.timeout-pqueue)
		  (pqueue-empty? node.waiting-pqueue))
             (begin
               (process-exit-hook-set!
                node.current-process
                (lambda () 'all-processes-are-dead))
               (continuation-restore node.initial-cont 'dummy))
	     (let ()
	       (allow-interrupts)
	       (timer-interrupt-enable!)
	       (advance-time!)
	       (ports-check-io!)
	       (process-schedule-next!)))
	 ;; Do context switch
	 (let ((process (pqueue-head node.ready-pqueue)))
	   (process-state-set! process 'running)
	   (set! node.current-process process)
	   (timer-interrupt-enable!)
	   (continuation-restore (process-continuation process) 'dummy))))))

(define (process-sleep! timeout)
  (with-no-interrupts
   (lambda ()
     (timer-interrupt-disable!)
     (process-wake-up-set! node.current-process (int.+ (current-time-in-msecs) timeout))
     (process-suspend-waiting!))))

(define (process-deliver! process msg)
  ; **** should check for exit signals, exit trapping, etc.
  (with-no-interrupts
   (lambda ()
     (let ((mailbox (process-mailbox process)))
       (queue-add-to-tail! mailbox msg)
       (if (eq? (process-state process) 'waiting)
	   (begin
	     (process-state-set! process 'ready)
	     (pqueue-move-to-tail! node.ready-pqueue process)))))))


(define (erl-generic-link pid pidlist)
  (with-no-interrupts
   (lambda ()
     (let loop ((pids pidlist))
       (if (erl-nil? pids)
	   (erl-list pid)
	   (let ((p (erl-hd pids)))
	     (if (erl-pid-=:= p pid)
		 pids
		 (erl-cons p (loop (erl-tl pids))))))))))
(define (erl-generic-unlink pid pidlist)
  (with-no-interrupts
   (lambda ()
     (let loop ((pids pidlist))
       (if (erl-nil? pids)
	   pids
	   (let ((p (erl-hd pids)))
	     (if (erl-pid-=:= p pid)
		 (erl-tl pids)
		 (cons p (loop (erl-tl pids))))))))))


(define (process-link! process pid)
  (with-no-interrupts
   (lambda ()
     (process-linked-pids-set!
      process
      (erl-generic-link pid (process-linked-pids process))))))

(define (process-unlink! process pid)
  (with-no-interrupts
   (lambda ()
     (process-linked-pids-set!
      process
      (erl-generic-unlink pid (process-linked-pids process))))))

(define (process-change-priority! process priority)
  ;; priority isn't implemented for now... just update the tag.
  (process-priority-set! process priority))

;; module name is an Erlang atom,
;; modulepath is an Erlang string
(define (erl-startup modulename modulepath)
  (single-node-setup!)
  (erl-load_module/2 modulename modulepath)
  (let ((fun (erl-function-ref (erl-atom-append modulename ':start/0))))
    (if fun
        (process-setup! (erl-tuple modulename 'start 0) fun)
        (error "Module doesn't define the function start/0."))
    (single-node-cleanup!)))

;;;;;;;;
;; Ports
(define (port-link! port pid)
  (with-no-interrupts
   (lambda ()
     (erl-port-linked-pids-set!
      port
      (erl-generic-link pid (erl-port-linked-pids port))))))

(define (port-unlink! port pid)
  (with-no-interrupts
   (lambda ()
     (erl-port-linked-pids-set!
      port
      (erl-generic-unlink pid (erl-port-linked-pids port))))))

(define (erl-port-send! port msg)
  (let ((owner (erl-port-owner port)))
    (if (erl-port-opened? port)
	(if (and (erl-tuple? msg)
		 (erl-fix=k (erl-vector-length msg) 3))
	    (let ((pid (erl-tuple-ref msg 1))
		  (action (erl-tuple-ref msg 2)))
	      (if (erl-pid-=:= pid owner)
		  (cond 
		   ((and (erl-tuple? action)
			 (fix.= (erl-vector-length action) 3))
		    (let ((detail (erl-tuple-ref action 1))
			  (arg (erl-tuple-ref action 2)))
		      (case detail
			((command)
			 (if (erl-port-in? port)
			     (let ((bin (erl-ioterm_to_binary arg)))
			       (if bin
				   (erl-port-deliver! port bin)
				   (erl-exit-signal! owner 'badsig1)))
			     (erl-exit-signal! owner 'badsig2)))
			((connect)
			 (if (and (erl-pid? arg)
				  (ato.= (erl-port-node port)
					 (erl-pid-node arg)))
			     (begin
			       (erl-port-owner-set! port arg)
			       (erl-message-signal!
				arg
				(erl-tuple port 'connected)))
			     (erl-exit-signal! owner 'badsig3)))
			(else (erl-exit-signal! owner 'badsig4)))))
		   ((and (erl-atom? action) (ato.= action 'close))
		    (erl-port_close/1 port)
		    (erl-message-signal! owner (erl-tuple port 'closed)))
		   (else (erl-exit-signal! owner 'badsig5)))
		  (erl-exit-signal! owner 'badsig6)))
	    (erl-exit-signal! owner 'badsig7))
	msg)))

;; returns the content of x in a list, #f if not an ioterm
(define (erl-generic-ioterm_to_list x)
  (cond
   ((erl-cons? x)
    (let ((hd (erl-hd x)) (tlc (erl-generic-ioterm_to_list (erl-tl x))))
      (and tlc
	   (cond
	    ((erl-byte? hd) (erl-cons hd tlc))
	    ((erl-char? hd) (erl-cons (fix.band (chr.->integer hd) 255) tlc))
	    (else
	     (let ((hdc (erl-generic-ioterm_to_list hd)))
	       (and hdc (erl-safe-++/2 hdc tlc))))))))
   ((erl-nil? x) x)
   ((erl-binary? x) (erl-binary_to_list/1 x))
   (else #f)))

;;;;;;;;;;;;;;;;;;;;;;
;; Some atom constants

(define badarg-atom (erl-atom<-string "badarg"))
(define badmatch-atom (erl-atom<-string "badmatch"))
(define case_clause-atom (erl-atom<-string "case_clause"))
(define function_clause-atom (erl-atom<-string "function_clause"))
(define lambda_clause-atom (erl-atom<-string "lambda_clause"))
(define timeout_value-atom (erl-atom<-string "timeout_value"))
(define nocatch-atom (erl-atom<-string "nocatch"))
(define normal-atom (erl-atom<-string "normal"))
(define EXIT-atom (erl-atom<-string "EXIT"))
(define THROW-atom (erl-atom<-string "THROW"))
(define infinity-atom (erl-atom<-string "infinity"))
(define runtime-atom (erl-atom<-string "runtime"))
(define run_queue-atom (erl-atom<-string "run_queue"))
(define undefined-atom (erl-atom<-string "undefined"))

(define (erl-exit-badarg) (erl-exit/1 'badarg))
(define (erl-exit-badindex) (erl-exit/1 'badindex))
(define (erl-exit-badarit) (erl-exit/1 'badarit))
(define (erl-exit-badbool) (erl-exit/1 'badbool))
(define (erl-exit-case_clause) (erl-exit/1 'case_clause))
(define (erl-exit-function_clause) (erl-exit/1 'function_clause))
(define (erl-exit-lambda_clause) (erl-exit/1 'lambda_clause))
(define (erl-exit-registry) (erl-exit/1 'registry))

(define (erl-mfa->fname m f a)
  (if (and (erl-sub? m) (erl-sub? f) (erl-ato? m) (erl-ato? f) (erl-int? a))
      (erl-atom<-string
       (string-append (erl-atom->string m)
                      ":"
                      (erl-atom->string f)
                      "/"
                      (number->string a)))
      (erl-exit-badarg)))

;;;;;;;;
;; Nodes
(define node.name #f)            ; node name
(define node.creation #f)        ; node creation
(define node.communicating #f)   ; node communicating ?
(define node.current-process (make-pqueue)) ; process currently running
(define node.process-count #f)   ; number of processes created
(define node.ref-count #f)       ; number of refs created
(define node.port-count #f)      ; number of ports created
(define node.ready-pqueue #f)    ; processes ready to run
(define node.timeout-pqueue #f)  ; processes that are waiting with timeout
(define node.waiting-pqueue #f)  ; processes that are waiting without timeout
(define node.last-runtime #f)    ; last call to erl-runtime-msecs!
(define node.initial-cont #f)    ; initial continuation
(define node.port-table #f)      ; table of ports
(define node.registry #f)        ; registered processes
(define node.magic_cookie #f)    ; the magic cookie of this node
(define node.magic_cookies #f)   ; the presumed magic cookies of other nodes
(define node.modules_loaded #f)  ; the list of loaded modules

(define (single-node-setup!)
  (set! node.name 'nonode@nohost)
  (set! node.creation 0)
  (set! node.communicating 'false)
  (set! node.process-count 0)
  (set! node.ref-count 0)
  (set! node.port-count 0)
  (set! node.ready-pqueue (make-pqueue))
  (set! node.timeout-pqueue (make-pqueue))
  (set! node.waiting-pqueue (make-pqueue))
  (set! node.last-runtime (current-cputime-in-msecs))
  (set! node.port-table (make-port-table))
  (set! node.registry (erl-registry.new 'undefined))
  (set! node.magic_cookie 'nocookie)
  (set! node.magic_cookies (erl-registry.new 'nocookie))
  (set! node.modules_loaded (erl-nil))

  (erl-define-functions
   (map (lambda (bif)
	  (erl-atom<-string (string-append "erlang:" (erl-atom->string bif))))
	exported-bif-list))
  (ports-init!)
  )

(define (erl-valid-node-name? name)
  (let loop ((atfound? #f) (lst (string->list (erl-atom->string name))))
    (if (null? lst)
	atfound?
	(if (char=? (car lst) #\@)
	    (if atfound?
		#f
		(loop #t (cdr lst)))
	    (loop atfound? (cdr lst))))))

(define (single-node-cleanup!)
  (ports-deinit!))

;------------------------------------------------------------------------------

; Added by Marc Feeley for ETOS 2.2

(define (erl-string? lst)
  (let loop ((x lst))
    (if (erl-cons? x)
        (if (erl-char? (erl-hd x))
            (loop (erl-tl x))
            #f)
        (erl-nil? x))))

(define (erl-string<-string str)
  (let loop ((i (fix.u- (string-length str) 1))
             (lst (erl-nil)))
    (if (fix.< i 0)
        lst
        (let ((c (string-ref str i)))
          (loop (fix.u- i 1)
                (erl-cons (erl-char<-char c) lst))))))

(define (erl-string->string lst)
  (let* ((n (length lst))
         (str (make-string n #\space)))
    (let loop ((x lst) (i 0))
      (if (erl-cons? x)
          (begin
            (string-set! str i (erl-char->char (erl-hd x)))
            (loop (erl-tl x) (fix.u+ i 1)))
          str))))
