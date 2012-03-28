#!

; file: "estart.scm"

; Copyright (C) 1998-1999 Universite de Montreal, All Rights Reserved.

; This is the program loader which executes Erlang modules that were
; compiled with ETOS.

;------------------------------------------------------------------------------

(include "rt-gambit.scm")
(include "rt-lib.scm")
(include "port-generic.scm")
(include "ks.scm")

;------------------------------------------------------------------------------

(define (command-error-usage)
  (display "Usage:  estart filename")
  (newline))

(define (main)
  (let ((args (cdr (command-line))))
    (if (not (= (length args) 1))
        (command-error-usage)
        (let* ((filename
                (car args))
               (module-name
                (path-strip-directory (path-strip-extension filename))))
          (erl-startup (erl-atom<-string module-name)
                       (erl-string<-string filename))))))

;(set! load (lambda (filename) #f))(include "../bench/sys/etos22/ring.scm")

(main)

;------------------------------------------------------------------------------
