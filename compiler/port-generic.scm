; file: "port-generic.scm"

; Copyright (C) 1998-1999 Universite de Montreal, All Rights Reserved.

; Dummy ports interface to compile the ETOS compiler

;(include "rt-gambit.scm")

(define (ports-check-io!) #f)
(define (erl-generic-open_port/2 name settings) 'false)
(define (erl-generic-port_close/1 x) 'true)
(define (erl-port-deliver! port bin) 'true)
(define (ports-init!) #f)
(define (ports-deinit!) #f)
(define (make-port-table) #f)
(define (erl-generic-ports/0) (erl-nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; In fact, this file is a prelude to rt-lib.scm
;(include "rt-lib.scm")

