; file: "ks.scm"

; Copyright (C) 1998-1999 Universite de Montreal, All Rights Reserved.

; Constants

;; List of function and a slower version with larger domain
;; Used for common subexpression elimination
(define fun-super
  '((erl-safe-size/1    . erl-tst-size/1)
    (erl-safe-element/2 . erl-tst-element/2)
    (erl-safe-abs/1     . erl-tst-abs/1)
    (erl-safe-float/1   . erl-tst-float/1)
    (erl-safe-length/1  . erl-tst-length/1)
    (erl-safe-node/1    . erl-tst-node/1)
    (erl-safe-round/1   . erl-tst-round/1)
    (erl-safe-sign/1    . erl-tst-sign/1)
    (erl-safe-trunc/1   . erl-tst-trunc/1)
    (erl-safe-+/2       . erl-tst-+/2)
    (erl-safe--/2       . erl-tst--/2)
    (erl-safe-bor/2     . erl-tst-bor/2)
    (erl-safe-bxor/2    . erl-tst-bxor/2)
    (erl-safe-bsl/2     . erl-tst-bsl/2)
    (erl-safe-bsr/2     . erl-tst-bsr/2)
    (erl-safe-*/2       . erl-tst-*/2)
    (erl-safe-//2       . erl-tst-//2)
    (erl-safe-///2      . erl-tst-///2)
    (erl-safe-div/2     . erl-tst-div/2)
    (erl-safe-mod/2     . erl-tst-mod/2)
    (erl-safe-rem/2     . erl-tst-rem/2)
    (erl-safe-band/2    . erl-tst-band/2)
    (erl-safe--/1       . erl-tst--/1)
    (erl-safe-+/1       . erl-tst-+/1)
    (erl-safe-bnot/1    . erl-tst-bnot/1)
    (erl-safe-not/1     . erl-tst-not/1)))

;; Basic tests list
(define basic-tst-list
  '(erl-sub? erl-spc? erl-fix? erl-con?
	     erl-big? erl-flo? erl-ato? erl-vec?
	     erl-chr? erl-nil?
	     erl-big=k erl-flo=k erl-ato=k erl-chr=k erl-fix=k
	     erl-lst? erl-str? erl-int? erl-num?
	     erl-< erl-== erl-=:=
	     ))

;; Internal tests list
(define internal-tst-list (map cdr fun-super))

;; List of all tests
(define tst-list
  (append basic-tst-list internal-tst-list))

;; List of recognizer BIFs
(define recognizer-bif-list
  '(is_atom/1 is_binary/1 is_char/1 is_compound/1 is_cons/1
	      is_float/1 is_function/1 is_integer/1 is_list/1 is_null/1
	      is_number/1 is_pid/1 is_port/1 is_ref/1 is_string/1 is_tuple/1))

(define comparison-bif-list
  '(==/2 =:=/2 /=/2 =/=/2 >/2 </2 >=/2 <=/2))

(define guard-strict-bif-list
  '(abs/1 element/2 float/1 hd/1 length/1 node/0 node/1 nodes/0
	  round/1 self/0 sign/1 size/1 tl/1 trunc/1))

(define guard-non-strict-bif-list
  '(+/2 -/2 bor/2 bxor/2 bsl/2 bsr/2 
	*/2 //2 ///2 div/2 mod/2 rem/2 band/2
	+/1 -/1 bnot/1 not/1))

(define guard-bif-list (append guard-strict-bif-list
			       guard-non-strict-bif-list))

(define always-completing-guard-bif-list
  (append recognizer-bif-list
	  comparison-bif-list
	  '(node/0 nodes/0 self/0)))

(define non-guard-strict-bif-list '(apply/2
				    apply/3
				    atom_to_list/1
				    atom_to_string/1
				    binary_to_list/1
				    binary_to_list/3
				    binary_to_string/1
				    char_to_integer/1
				    concat_binary/1
				    date/0
				    erase/0
				    erase/1
				    exit/1
				    exit/2
				    float_to_list/1
				    get/0
				    get/1
				    get_keys/1
				    group_leader/0
				    group_leader/2
				    integer_to_char/1
				    integer_to_list/1
				    integer_to_string/1
				    is_alive/0
				    link/1
				    list_to_atom/1
				    list_to_binary/1
				    list_to_float/1
				    list_to_integer/1
				    list_to_string/1
				    list_to_tuple/1
				    make_ref/0
				    now/0
				    open_port/2
				    port_close/1
				    port_info/1
				    port_info/2
				    ports/0
				    process_info/2
				    process_flag/2
				    put/2
				    register/2
				    registered/0
				    setelement/3
				    spawn/3
				    spawn_link/3
				    split_binary/2
				    statistics/1
				    string_to_list/1
				    throw/1
				    time/0
				    tuple_to_list/1
				    unlink/1
				    unregister/1
				    whereis/1))

(define semi-bif-list '(check_process_code/2
			delete_module/1
			get_cookie/0
			halt/0
			hash/2
			load_module/2
			m_acos/1
			m_acosh/1
			m_asin/1
			m_asinh/1
			m_atan/1
			m_atan2/2
			m_atanh/1
			m_cos/1
			m_cosh/1
			m_erf/1
			m_erfc/1
			m_exp/1
			m_log/1
			m_log10/1
			m_pow/2
			m_sin/1
			m_sinh/1
			m_sqrt/1
			m_tan/1
			m_tanh/1
			module_loaded/1
			preloaded/0
			purge_module/1
			set_cookie/2))

(define bif-list
  (append recognizer-bif-list
	  comparison-bif-list
	  guard-bif-list
	  non-guard-strict-bif-list
	  '(++/2
	    --/2
	    or/2
	    xor/2
	    and/2)))

(define exported-bif-list (append recognizer-bif-list
				  non-guard-strict-bif-list
				  guard-strict-bif-list
				  semi-bif-list))

;; List of function having no (side)-effect
(define no-effect-fun-list
  (append recognizer-bif-list
	  guard-bif-list
	  basic-tst-list
	  internal-tst-list
	  '(erl-vector-ref
	    erl-tuple
	    erl-vector-length
	    erl-cons
	    erl-function
	    erl-tl
	    erl-hd)))
