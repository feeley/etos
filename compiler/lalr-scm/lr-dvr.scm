;; ---------------------------------------------------------------------- ;;
;; FICHIER               : lr-dvr.scm                                     ;;
;; DATE DE CREATION      : Fri May 31 15:47:05 1996                       ;;
;; DERNIERE MODIFICATION : Fri May 31 15:51:13 1996                       ;;
;; ---------------------------------------------------------------------- ;;
;; Copyright (c) 1996 Dominique Boucher                                   ;;
;; ---------------------------------------------------------------------- ;;
;; The LR parser driver                                                   ;;
;;                                                                        ;;
;; lr-dvr.scm is part of the lalr.scm distribution which is free          ;;
;; software; you can redistribute it and/or modify                        ;;
;; it under the terms of the GNU General Public License as published by   ;;
;; the Free Software Foundation; either version 2, or (at your option)    ;;
;; any later version.                                                     ;;
;;                                                                        ;;
;; lalr.scm is distributed in the hope that it will be useful,            ;;
;; but WITHOUT ANY WARRANTY; without even the implied warranty of         ;;
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the          ;;
;; GNU General Public License for more details.                           ;;
;;                                                                        ;;
;; You should have received a copy of the GNU General Public License      ;;
;; along with lalr.scm; see the file COPYING.  If not, write to           ;;
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  ;;
;; ---------------------------------------------------------------------- ;;

(define max-stack-size 500)
(define *debug* #f)

(define (push stack sp new-cat goto-table lval)
  (let* ((state     (vector-ref stack sp))
	 (new-state (cdr (assq new-cat (vector-ref goto-table state))))
	 (new-sp    (+ sp 2)))
    (if (>= new-sp max-stack-size)
	(error "PARSE ERROR : stack overflow")
	(begin
	  (vector-set! stack new-sp new-state)
	  (vector-set! stack (- new-sp 1) lval)
	  new-sp))))

(define (make-parser action-table goto-table reduction-table token-defs)
  (lambda (lexerp errorp)

    (define (action x l)
      (let ((y (assq x l)))
	(if y 
	    (cdr y) 
	    (cdar l))))
  
    (let ((stack (make-vector max-stack-size 0)))
      (let loop ((sp 0) (input (lexerp)))
	(let* ((state (vector-ref stack sp))
	       (i     (car input))
	       (act   (action i (vector-ref action-table state))))

	  (if *debug*
	      (begin
		(display "** PARSER TRACE: i=") 
		(display (cdr (assq i token-defs)))
		(display "  state=") 
		(display state) 
		(display "  sp=")
		(display sp) 
		(newline)))

	  (cond

	   ;; Input succesfully parsed
	   ((eq? act 'accept)
	    (vector-ref stack 1))

	   ;; Syntax error in input
	   ((eq? act '*error*)
	    (errorp "PARSE ERROR : unexpected token : " 
		    (cdr (assq i token-defs))))

	   ;; Shift current token on top of the stack
	   ((>= act 0)
	    (vector-set! stack (+ sp 1) (cdr input))
	    (vector-set! stack (+ sp 2) act)
	    (loop (+ sp 2) (lexerp)))

	   ;; Reduce by rule (- act)
	   (else 
	    (loop ((vector-ref reduction-table (- act)) stack sp goto-table) 
		  input))))))))

