; file: "parser.scm"

; Copyright (C) 1998-1999 Universite de Montreal, All Rights Reserved.

; This is the parser and preprocessor of the ETOS compiler.

;------------------------------------------------------------------------------

(include "erlang.scm") ; Erlang parsing tables generated from "grammar.scm"
(include "scanner.scm") ; Erlang scanner
(include "ast-comp.scm") ; AST compiler
;(include "ast-pp.scm") ; AST pretty printer

;------------------------------------------------------------------------------

; Parser interface:
;
; (parse filename abs-filename options)
;                             returns an AST for the file "filename"

;------------------------------------------------------------------------------

; Preprocessor interface:
;
; (preproc-setup filename abs-filename options)
;                             returns a "preprocessor state" object
; (preproc-cleanup pps)       closes ports of "preprocessor state" object "pps"
; (preproc-read-token pps)    read next token and return it
; (preproc-skip-token-sequence pps)
;                             advances to next token sequence

;------------------------------------------------------------------------------

; "Preprocessor state" objects.

(define (make-pps)
  (vector #f #f #f #f
	  #f #f #f #f
	  #f #f #f))

(define (pps-token-sequence pps)         (vector-ref pps 0))
(define (pps-token-sequence-set! pps x)  (vector-set! pps 0 x))
(define (pps-processing? pps)            (vector-ref pps 1))
(define (pps-processing?-set! pps x)     (vector-set! pps 1 x))
(define (pps-file-nesting pps)           (vector-ref pps 2))
(define (pps-file-nesting-set! pps x)    (vector-set! pps 2 x))
(define (pps-macros pps)                 (vector-ref pps 3))
(define (pps-macros-set! pps x)          (vector-set! pps 3 x))
(define (pps-module-name pps)            (vector-ref pps 4))
(define (pps-module-name-set! pps x)     (vector-set! pps 4 x))
(define (pps-export-list pps)            (vector-ref pps 5))
(define (pps-export-list-set! pps x)     (vector-set! pps 5 x))
(define (pps-import-list pps)            (vector-ref pps 6))
(define (pps-import-list-set! pps x)     (vector-set! pps 6 x))
(define (pps-wild-list pps)              (vector-ref pps 7))
(define (pps-wild-list-set! pps x)       (vector-set! pps 7 x))
(define (pps-record-defs pps)            (vector-ref pps 8))
(define (pps-record-defs-set! pps x)     (vector-set! pps 8 x))
(define (pps-compile-options pps)        (vector-ref pps 9))
(define (pps-compile-options-set! pps x) (vector-set! pps 9 x))
(define (pps-start-time pps)             (vector-ref pps 10))
(define (pps-start-time-set! pps x)      (vector-set! pps 10 x))

(define (preproc-setup filename abs-filename options)
  (let ((start-time (get-utc-time))
	(pps (make-pps)))

    (pps-token-sequence-set! pps '())

    (pps-processing?-set! pps #t)

    (pps-file-nesting-set!
     pps
     (vector #f (scanner-setup filename abs-filename) '()))

    (pps-macros-set! pps (make-dict #f))
    (dict-assign (pps-macros pps) "FILE" 'special)
    (dict-assign (pps-macros pps) "LINE" 'special)

    (let loop ((lst options))
      (if (not (erl-nil? lst))
          (let ((x (erl-hd lst)))
            (if (and (erl-tuple? x)
                     (= (erl-tuple-size x) 2)
                     (eq? (erl-tuple-ref x 1)
                          (erl-atom<-string "d")))
                (let ((name (erl-tuple-ref x 2)))
                  (if (erl-atom? name)
                      (macro-define pps (erl-atom->string name) '() '()))))
            (loop (erl-tl lst)))))

    (pps-module-name-set!
     pps
     (filename-strip-directory (filename-strip-extension filename)))

    (pps-export-list-set!
     pps
     (erl-list (erl-tuple (erl-atom<-string "module_info")
                          (erl-int<-exact-integer 0))
               (erl-tuple (erl-atom<-string "module_info")
                          (erl-int<-exact-integer 1))))

    (pps-import-list-set! pps (erl-nil))

    (pps-wild-list-set! pps (erl-nil))

    (pps-record-defs-set! pps '())

    (pps-compile-options-set! pps options)

    (pps-start-time-set!
     pps
     (utc-time-in-secs->broken-time start-time))

    pps))

(define (preproc-cleanup pps)
  (let loop ((file-nesting (pps-file-nesting pps)))
    (if file-nesting
      (let ((ss (vector-ref file-nesting 1)))
        (scanner-cleanup ss)
        (loop (vector-ref file-nesting 0))))))

(define (preproc-read-token pps)

  (define (read-next-token-sequence)
    (let ((ss (vector-ref (pps-file-nesting pps) 1)))

      (define (end-of-file t)
        (let loop ((lst (vector-ref (pps-file-nesting pps) 2)))
          (if (not (null? lst))
            (begin
              (pp-error-no-matching-endif-directive (vector-ref (car lst) 2))
              (loop (cdr lst)))))
        (vector-set! (pps-file-nesting pps) 2 '())
        (let ((container (vector-ref (pps-file-nesting pps) 0)))
          (if container
            (begin
              (scanner-cleanup ss)
              (pps-file-nesting-set! pps container)
              (read-next-token-sequence))
            t)))

      (let ((t1 (scanner-read-token ss)))
        (if (= (token-kind t1) *EOI*-tok)
          (end-of-file t1)
          (let loop ((lst (list t1)))
            (let* ((t2 (scanner-read-token ss))
                   (kind2 (token-kind t2)))
              (cond ((= kind2 FullStop-tok)
                     (process-token-sequence (reverse (cons t2 lst))))
                    ((= kind2 *EOI*-tok)
                     (pp-error-end-of-file t1 t2)
                     (end-of-file t2))
                    (else
                     (loop (cons t2 lst))))))))))

  (define (process-token-sequence token-sequence)

    ; note: token-sequence ends with a FullStop token so we can simplify
    ; or avoid tests for the end of sequence.

    (define (zero-arg exec)
      (let ((t3 (caddr token-sequence)))
        (if (= (token-kind t3) FullStop-tok)
          (exec t3)
          (pp-error-FullStop-expected t3))))

    (define (one-arg check1 exec)
      (let ((t3 (caddr token-sequence)))
        (if (= (token-kind t3) PAREN-OPEN-tok)
          (let* ((t4 (cadddr token-sequence))
                 (arg1 (check1 t4)))
            (and arg1
                 (let ((t5 (cadddr (cdr token-sequence))))
                   (if (= (token-kind t5) PAREN-CLOSE-tok)
                     (let ((t6 (cadddr (cddr token-sequence))))
                       (if (= (token-kind t6) FullStop-tok)
                         (exec arg1 t6)
                         (pp-error-FullStop-expected t6)))
                     (pp-error-close-paren-expected t5)))))
          (pp-error-open-paren-expected t3))))

    (define (two-arg check1 check2 exec)
      (let ((t3 (caddr token-sequence)))
        (if (= (token-kind t3) PAREN-OPEN-tok)
          (let* ((t4 (cadddr token-sequence))
                 (arg1 (check1 t4)))
            (and arg1
                 (let ((t5 (cadddr (cdr token-sequence))))
                   (if (= (token-kind t5) COMMA-tok)
                       (let* ((t6 (cadddr (cddr token-sequence)))
                              (arg2 (check2 t6)))
                         (and arg2
                              (let ((t7 (cadddr (cdddr token-sequence))))
                                (if (= (token-kind t7) PAREN-CLOSE-tok)
                                    (let ((t8
                                           (cadddr (cddddr token-sequence))))
                                      (if (= (token-kind t8) FullStop-tok)
                                          (exec arg1 arg2 t8)
                                          (pp-error-FullStop-expected t8)))
                                    (pp-error-close-paren-expected t7)))))
                       (pp-error-comma-expected t5)))))
          (pp-error-open-paren-expected t3))))

    (define (parse-macro-def-body tok name params seq)
      (let ((t1 (car seq)))
        (if (= (token-kind t1) COMMA-tok)
          (let loop ((lst (cdr seq)) (rev-body '()))
            (let ((t2 (car lst)))
              (cond ((and (= (token-kind t2) PAREN-CLOSE-tok)
                          (= (token-kind (cadr lst)) FullStop-tok))
                     (if (not (string=? name "LINE"))
                       (if (macro-defined? pps name)
                         (pp-error-macro-previously-defined tok)
                         (macro-define pps name params (reverse rev-body)))))
                    ((= (token-kind t2) FullStop-tok)
                     (pp-error-close-paren-expected t2))
                    (else
                     (loop (cdr lst) (cons t2 rev-body))))))
          (pp-error-comma-expected t1))))

    (define (parse-macro-def token-sequence)
      (let ((t3 (caddr token-sequence)))
        (if (= (token-kind t3) PAREN-OPEN-tok)
          (let* ((t4 (cadddr token-sequence))
                 (name (check-macro-name t4)))
            (and name
                 (let* ((rest (cddddr token-sequence))
                        (t5 (car rest)))
                   (if (= (token-kind t5) PAREN-OPEN-tok)
                     (if (= (token-kind (cadr rest)) PAREN-CLOSE-tok)
                       (parse-macro-def-body t4 name '() (cddr rest))
                       (let loop ((lst (cdr rest)) (rev-params '()))
                         (let ((t7 (car lst)))
                           (if (= (token-kind t7) Variable-tok)
                             (let ((t8
                                    (cadr lst))
                                   (new-rev-params
                                    (cons (token-value t7)
                                          rev-params)))
                               (cond ((= (token-kind t8) COMMA-tok)
                                      (loop (cddr lst) new-rev-params))
                                     ((= (token-kind t8) PAREN-CLOSE-tok)
                                      (parse-macro-def-body
                                       t4
                                       name
                                       (reverse new-rev-params)
                                       (cddr lst)))
                                     (else
                                      (pp-error-close-paren-expected t8))))
                             (pp-error-variable-expected t7)))))
                     (parse-macro-def-body t4 name #f rest)))))
          (pp-error-open-paren-expected t3))))

    (let ((t1 (car token-sequence)))
      (if (= (token-kind t1) MINUS-tok)
        (let* ((t2 (cadr token-sequence))
               (kind2 (token-kind t2)))

          (cond ((and (= kind2 NotQuotedAtomLiteral-tok)
                      (or (string=? (token-value t2) "ifdef")
                          (string=? (token-value t2) "ifndef")))
                 (one-arg
                  (lambda (t)
                    (check-macro-name t))
                  (lambda (name full-stop)
                    (let ((if-nestings (vector-ref (pps-file-nesting pps) 2)))
                      (vector-set! (pps-file-nesting pps) 2
                        (cons (vector 'if
                                      (pps-processing? pps)
                                      (generic-location-join t1 full-stop))
                              if-nestings))
                      (pps-processing?-set! pps
                        (and (pps-processing? pps)
                             (eq? (macro-defined? pps name)
                                  (string=? (token-value t2) "ifdef")))))))
                 (read-next-token-sequence))

                ((and (= kind2 NotQuotedAtomLiteral-tok)
                      (string=? (token-value t2) "else"))
                 (zero-arg
                  (lambda (full-stop)
                    (let ((if-nestings (vector-ref (pps-file-nesting pps) 2)))
                      (if (null? if-nestings)
                        (pp-error-unexpected-else t2)
                        (let ((if-nesting (car if-nestings)))
                          (if (not (eq? (vector-ref if-nesting 0) 'if))
                            (pp-error-unexpected-else t2)
                            (begin
                              (pps-processing?-set! pps
                                (and (vector-ref if-nesting 1)
                                     (not (pps-processing? pps))))
                              (vector-set! if-nesting 0 'else))))))))
                 (read-next-token-sequence))

                ((and (= kind2 NotQuotedAtomLiteral-tok)
                      (string=? (token-value t2) "endif"))
                 (zero-arg
                  (lambda (full-stop)
                    (let ((if-nestings (vector-ref (pps-file-nesting pps) 2)))
                      (if (null? if-nestings)
                        (pp-error-unexpected-endif t2)
                        (let ((if-nesting (car if-nestings)))
                          (pps-processing?-set! pps
                            (vector-ref if-nesting 1))
                          (vector-set! (pps-file-nesting pps) 2
                            (cdr if-nestings)))))))
                 (read-next-token-sequence))

                ((not (pps-processing? pps))
                 (read-next-token-sequence))

                ((= kind2 IF-tok)
                 (pp-error-if-directive-reserved t2)
                 (read-next-token-sequence))

                ((and (= kind2 NotQuotedAtomLiteral-tok)
                      (string=? (token-value t2) "elif"))
                 (pp-error-elif-directive-reserved t2)
                 (read-next-token-sequence))

                ((and (= kind2 NotQuotedAtomLiteral-tok)
                      (string=? (token-value t2) "define"))
                 (parse-macro-def token-sequence)
                 (read-next-token-sequence))

                ((and (= kind2 NotQuotedAtomLiteral-tok)
                      (string=? (token-value t2) "undef"))
                 (one-arg
                  (lambda (t)
                    (check-macro-name t))
                  (lambda (name full-stop)
                    (if (not (string=? name "LINE"))
                      (macro-undef pps name))))
                 (read-next-token-sequence))

                ((and (= kind2 NotQuotedAtomLiteral-tok)
                      (string=? (token-value t2) "file"))
                 (two-arg
                  (lambda (t)
                    (check-string-literal t))
                  (lambda (t)
                    (check-unsigned-decimal-literal t))
                  (lambda (filename line full-stop)
;                    (pp (list (token-value filename) (token-value line)));************do something with this info????
                    #f))
                 (read-next-token-sequence))

                ((and (= kind2 NotQuotedAtomLiteral-tok)
                      (string=? (token-value t2) "include"))
                 (one-arg
                  (lambda (t)
                    (check-string-literal t))
                  (lambda (filename full-stop)
                    (include-file filename #f)))
                 (read-next-token-sequence))

                ((and (= kind2 NotQuotedAtomLiteral-tok)
                      (string=? (token-value t2) "include_lib"))
                 (one-arg
                  (lambda (t)
                    (check-string-literal t))
                  (lambda (filename full-stop)
                    (include-file filename #t)))
                 (read-next-token-sequence))

                (else
                 (expand token-sequence))))

        (if (not (pps-processing? pps))
          (read-next-token-sequence)
          (expand token-sequence)))))

  (define (check-string-literal t)
    (if (= (token-kind t) OneStringLiteral-tok)
      t
      (begin
        (pp-error-string-literal-expected t)
        #f)))

  (define (check-unsigned-decimal-literal t)
    (if (= (token-kind t) UnsignedDecimalLiteral-tok)
      t
      (begin
        (pp-error-unsigned-decimal-literal-expected t)
        #f)))

  (define (check-macro-name t)
    (let ((kind (token-kind t)))
      (cond ((= kind Variable-tok)
             (token-value t))
            ((= kind NotQuotedAtomLiteral-tok)
             (token-value t))
            ((= kind QuotedAtomLiteral-tok)
             (token-value t))
            (else
             (pp-error-macro-name-expected t)
             #f))))

  (define (expand token-sequence)

    (define (expand-macro t1 t2 name)
      (let ((x (dict-lookup (pps-macros pps) name)))
        (cond ((not x)
               (pp-error-undefined-macro t2)
               (expand (cdr token-sequence)))
              ((eq? x 'special)
               (cond ((string=? name "MODULE")
                      (expand
                       (cons (make-token
                              QuotedAtomLiteral-tok
                              (token-filename t1)
                              (token-start-pos t1)
                              (token-end-pos t2)
                              (pps-module-name pps))
                             (cddr token-sequence))))
                     ((string=? name "FILE")
                      (expand
                       (cons (make-token
                              OneStringLiteral-tok
                              (token-filename t1)
                              (token-start-pos t1)
                              (token-end-pos t2)
                              (ss-abs-filename
                               (vector-ref (pps-file-nesting pps) 1)))
                             (cddr token-sequence))))
                     ((string=? name "LINE")
                      (expand
                       (cons (make-token
                              UnsignedDecimalLiteral-tok
                              (token-filename t1)
                              (token-start-pos t1)
                              (token-end-pos t2)
                              (position->line (token-start-pos t2)))
                             (cddr token-sequence))))
                     (else
                      (expand (cddr token-sequence)))))
              (else
               (let ((params (vector-ref x 0))
                     (body (vector-ref x 1)))
                 (if (not params)
                   (expand
                    (append body (cddr token-sequence)))
                   (parse-macro-args
                    (cddr token-sequence)
                    (lambda (rest t3 rev-args)
                      (let ((args
                             (if (and (= (length params) 1)
                                      (null? rev-args))
                                 (list '()) ; ?foo() is ok if foo has 1 param
                                 (reverse rev-args))))
                        (if (not (= (length params)
                                    (length args)))
                          (begin
                            (pp-error-wrong-nb-args t1 t3)
                            (expand rest))
                          (let ((param-args (map cons params args)))
                            (let loop ((rev-lst (reverse body))
                                       (new-sequence rest))
                              (if (null? rev-lst)
                                (expand new-sequence)
                                (let ((t (car rev-lst)))
                                  (if (= (token-kind t) Variable-tok)
                                    (let ((y (assoc (token-value t)
                                                    param-args)))
                                      (if y
                                        (loop (cdr rev-lst)
                                              (append (cdr y) new-sequence))
                                        (loop (cdr rev-lst)
                                              (cons t new-sequence))))
                                    (loop (cdr rev-lst)
                                          (cons t new-sequence)))))))))))))))))

    (define (parse-macro-args seq cont)
      (let ((t1 (car seq)))
        (if (= (token-kind t1) PAREN-OPEN-tok)
          (let ((t2 (cadr seq)))
            (if (= (token-kind t2) PAREN-CLOSE-tok)
              (cont (cddr seq)
                    t2
                    '())
              (parse-arguments
               (cdr seq)
               '()
               (lambda (rest rev-args)
                 (let ((t3 (car rest)))
                   (if (= (token-kind t3) PAREN-CLOSE-tok)
                     (cont (cdr rest)
                           t3
                           rev-args)
                     (begin
                       (pp-error-close-paren-expected t3)
                       (expand rest))))))))
          (begin
            (pp-error-open-paren-expected t1)
            (expand seq)))))

    (define (parse-arguments seq rev-args cont)
      (parse-balanced-expr
       seq
       '()
       (lambda (rest rev-expr)
         (let ((new-rev-args (cons (reverse rev-expr) rev-args)))
           (let ((t1 (car rest)))
             (if (= (token-kind t1) COMMA-tok)
               (parse-arguments
                (cdr rest)
                new-rev-args
                cont)
               (cont rest
                     new-rev-args)))))))

    (define (parse-balanced-exprs seq rev-expr cont)
      (parse-balanced-expr
       seq
       rev-expr
       (lambda (rest rev-expr)
         (let ((t1 (car rest)))
           (if (= (token-kind t1) COMMA-tok)
             (parse-balanced-exprs
              (cdr rest)
              (cons t1 rev-expr)
              cont)
             (cont rest
                   rev-expr))))))

    (define (parse-balanced-expr seq rev-expr cont)
      (let* ((t1 (car seq))
             (kind1 (token-kind t1))
             (close (balance-close kind1)))
        (cond (close
               (parse-balanced-exprs
                (cdr seq)
                (cons t1 rev-expr)
                (lambda (rest rev-expr)
                  (let* ((t2 (car rest))
                         (kind2 (token-kind t2)))
                    (if (= kind2 close)
                      (cont (cdr rest)
                            (cons t2 rev-expr))
                      (begin
                        (pp-error-unbalanced-macro-argument t1 (car rev-expr))
                        (expand rest)))))))
              ((paren-like? kind1)
               (cont seq
                     rev-expr))
              (else
               (parse-balanced-expr
                (cdr seq)
                (cons t1 rev-expr)
                cont)))))

    (define (balance-close kind)
      (cond ((= kind PAREN-OPEN-tok)
             PAREN-CLOSE-tok)
            ((= kind BRACK-OPEN-tok)
             BRACK-CLOSE-tok)
            ((= kind BRACE-OPEN-tok)
             BRACE-CLOSE-tok)
            ((= kind BEGIN-tok)
             END-tok)
            ((= kind ALL_TRUE-tok)
             END-tok)
            ((= kind SOME_TRUE-tok)
             END-tok)
            ((= kind COND-tok)
             END-tok)
            ((= kind IF-tok)
             END-tok)
            ((= kind CASE-tok)
             END-tok)
            ((= kind RECEIVE-tok)
             END-tok)
            ((= kind TRY-tok)
             END-tok)
            ((= kind FUN-tok)
             END-tok)
            (else
             #f)))

    (define (paren-like? kind)
      (or (= kind FullStop-tok)
          (= kind PAREN-OPEN-tok)
          (= kind PAREN-CLOSE-tok)
          (= kind BRACK-OPEN-tok)
          (= kind BRACK-CLOSE-tok)
          (= kind BRACE-OPEN-tok)
          (= kind BRACE-CLOSE-tok)
          (= kind BEGIN-tok)
          (= kind ALL_TRUE-tok)
          (= kind SOME_TRUE-tok)
          (= kind COND-tok)
          (= kind IF-tok)
          (= kind CASE-tok)
          (= kind RECEIVE-tok)
          (= kind TRY-tok)
          (= kind FUN-tok)
          (= kind END-tok)
          (= kind COMMA-tok)))

    (let* ((t1 (car token-sequence))
           (kind1 (token-kind t1)))
      (if (= kind1 QUESTION-tok)
        (let* ((t2 (cadr token-sequence))
               (kind2 (token-kind t2)))
          (cond ((= kind2 NotQuotedAtomLiteral-tok)
                 (expand-macro t1 t2 (token-value t2)))
                ((= kind2 QuotedAtomLiteral-tok)
                 (expand-macro t1 t2 (token-value t2)))
                ((= kind2 Variable-tok)
                 (expand-macro t1 t2 (token-value t2)))
                (else
                 (pp-error-macro-name-expected t2)
                 (if (= kind2 FullStop-tok)
                   t2
                   (expand (cddr token-sequence))))))
        (let ((rest (cdr token-sequence)))
          (if (null? rest)
              (pps-token-sequence-set!
               pps
               (list (make-token
                      *EOI*-tok
                      (token-filename t1)
                      (token-start-pos t1)
                      (token-end-pos t1)
                      #f)))
              (pps-token-sequence-set!
               pps
               rest))
          t1))))

  (define (include-file filename-tok library?)

    (define (include-from filename abs-filename)
      (let ((ss (scanner-setup filename abs-filename)))
	(let ((container (pps-file-nesting pps)))
	  (pps-file-nesting-set! pps (vector container ss '())))))

    (let ((filename (token-value filename-tok)))
      (if (filename-absolute? filename)
          (if (filename-exists? filename)
              (include-from filename filename)
              (file-not-found-error filename-tok filename))
          (let loop ((ipaths (get-include-path-list library?)))
            (if (null? ipaths)
                (file-not-found-error filename-tok filename)
                (let ((abs-filename
                       (filename->abs-filename
                        (filename-build (car ipaths) filename))))
                  (if (and abs-filename
                           (filename-exists? abs-filename))
                      (include-from filename abs-filename)
                      (loop (cdr ipaths)))))))))

  (let ((token-sequence (pps-token-sequence pps)))
    (let ((t
           (if (null? token-sequence)
               (read-next-token-sequence)
               (expand token-sequence))))
      (if debug?
        (write-tok t))
      t)))

(define (preproc-skip-token-sequence pps)
  (pps-token-sequence-set! pps '()))

(define (macro-defined? pps name)
  (and (dict-lookup (pps-macros pps) name)
       #t))

(define (macro-define pps name params body)
  (dict-assign (pps-macros pps) name (vector params body)))

(define (macro-undef pps name)
  (dict-assign (pps-macros pps) name #f))

; For debugging:

(define (write-tok t)
  (let ((kind (token-kind t)))
    (cond ((= kind *EOI*-tok)
           (display "<EOI>"))
          ((= kind FullStop-tok)
           (display "."))
          ((= kind Variable-tok)
           (display (token-value t)))
          ((= kind UniversalPattern-tok)
           (display "_"))
          ((= kind OneStringLiteral-tok)
           (write (token-value t)))
          ((= kind UnsignedDecimalLiteral-tok)
           (write (token-value t)))
          ((= kind NotUnsignedDecimalIntegerLiteral-tok)
           (write (token-value t)))
          ((= kind FloatLiteral-tok)
           (write (token-value t)))
          ((= kind CharLiteral-tok)
           (display "$\\")
           (display (token-value t)))
          ((= kind NotQuotedAtomLiteral-tok)
           (display (token-value t)))
          ((= kind QuotedAtomLiteral-tok)
           (display "'")
           (display (token-value t))
           (display "'"))
          ((= kind AFTER-tok)
           (display "after"))
          ((= kind ALL_TRUE-tok)
           (display "all_true"))
          ((= kind AND-tok)
           (display "and"))
          ((= kind BAND-tok)
           (display "band"))
          ((= kind BEGIN-tok)
           (display "begin"))
          ((= kind BNOT-tok)
           (display "bnot"))
          ((= kind BOR-tok)
           (display "bor"))
          ((= kind BSL-tok)
           (display "bsl"))
          ((= kind BSR-tok)
           (display "bsr"))
          ((= kind BXOR-tok)
           (display "bxor"))
          ((= kind CASE-tok)
           (display "case"))
          ((= kind CATCH-tok)
           (display "catch"))
          ((= kind COND-tok)
           (display "cond"))
          ((= kind DIV-tok)
           (display "div"))
          ((= kind END-tok)
           (display "end"))
          ((= kind FUN-tok)
           (display "fun"))
          ((= kind IF-tok)
           (display "if"))
          ((= kind LET-tok)
           (display "let"))
          ((= kind MOD-tok)
           (display "mod"))
          ((= kind NOT-tok)
           (display "not"))
          ((= kind OF-tok)
           (display "of"))
          ((= kind OR-tok)
           (display "or"))
          ((= kind RECEIVE-tok)
           (display "receive"))
          ((= kind REM-tok)
           (display "rem"))
          ((= kind SOME_TRUE-tok)
           (display "some_true"))
          ((= kind TRY-tok)
           (display "try"))
          ((= kind WHEN-tok)
           (display "when"))
          ((= kind XOR-tok)
           (display "xor"))
          ((= kind BANG-tok)
           (display "!"))
          ((= kind SHARP-tok)
           (display "#"))
          ((= kind PAREN-OPEN-tok)
           (display "("))
          ((= kind PAREN-CLOSE-tok)
           (display ")"))
          ((= kind STAR-tok)
           (display "*"))
          ((= kind PLUS-tok)
           (display "+"))
          ((= kind PLUS-PLUS-tok)
           (display "++"))
          ((= kind COMMA-tok)
           (display ","))
          ((= kind MINUS-tok)
           (display "-"))
          ((= kind MINUS-MINUS-tok)
           (display "--"))
          ((= kind MINUS-GT-tok)
           (display "->"))
          ((= kind PERIOD-tok)
           (display "."))
          ((= kind SLASH-tok)
           (display "/"))
          ((= kind SLASH-SLASH-tok)
           (display "//"))
          ((= kind SLASH-EQUAL-tok)
           (display "/="))
          ((= kind COLON-tok)
           (display ":"))
          ((= kind SEMICOLON-tok)
           (display ";"))
          ((= kind LT-tok)
           (display "<"))
          ((= kind LT-MINUS-tok)
           (display "<-"))
          ((= kind EQUAL-tok)
           (display "="))
          ((= kind EQUAL-SLASH-EQUAL-tok)
           (display "=/="))
          ((= kind EQUAL-COLON-EQUAL-tok)
           (display "=:="))
          ((= kind EQUAL-LT-tok)
           (display "=<"))
          ((= kind EQUAL-EQUAL-tok)
           (display "=="))
          ((= kind GT-tok)
           (display ">"))
          ((= kind GT-EQUAL-tok)
           (display ">="))
          ((= kind QUESTION-tok)
           (display "?"))
          ((= kind BRACK-OPEN-tok)
           (display "["))
          ((= kind BRACK-CLOSE-tok)
           (display "]"))
          ((= kind BRACE-OPEN-tok)
           (display "{"))
          ((= kind BAR-tok)
           (display "|"))
          ((= kind BAR-BAR-tok)
           (display "||"))
          ((= kind BRACE-CLOSE-tok)
           (display "}"))
          ((= kind MODULE-tok)
           (display "module"))
          ((= kind EXPORT-tok)
           (display "export"))
          ((= kind IMPORT-tok)
           (display "import"))
          ((= kind COMPILE-tok)
           (display "compile"))
          ((= kind RECORD-tok)
           (display "record"))
          (else
           (display "????")))
    (if (= kind FullStop-tok)
      (newline)
      (display " "))))

;------------------------------------------------------------------------------

; Driver for parser.

(define max-stack-size 4096)

(define (push stack sp new-cat goto-table lval)
  (let ((current-state (vector-ref stack sp)))
    (push-aux stack
              sp
              lval
              (cdr (assq new-cat (vector-ref goto-table current-state))))))

(define (push-aux stack sp item1 item2)
  (let ((new-sp (+ sp 2)))
    (if (>= new-sp max-stack-size)
      #f ; stack overflow
      (begin
        (vector-set! stack (- new-sp 1) item1)
        (vector-set! stack new-sp item2)
        new-sp))))

(define (parse-aux pps)

  (define (action x l)
    (let ((y (assq x l)))
      (if y
        (cdr y)
        (cdar l))))

  (let ((stack (make-vector max-stack-size 0)))
    (let loop ((sp 0)
               (tok (preproc-read-token pps)))
      (let* ((state (vector-ref stack sp))
             (kind  (token-kind tok))
             (act   (action kind (vector-ref action-table state))))

        (if debug?
          (begin
            (display "** PARSER TRACE: kind=")
            (display (cdr (assq kind token-defs)))
            (display "  line=")
            (display (position->line (token-start-pos tok)))
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
            (parser-error-unexpected-token tok)
            #f)

           ;; Shift current token on top of the stack
           ((>= act 0)
            (let ((new-sp
                   (push-aux stack sp tok act)))
              (if new-sp
                (loop new-sp
                      (preproc-read-token pps))
                (begin
                  (parser-error-stack-overflow tok)
                  #f))))

           ;; Reduce by rule (- act)
           (else
            (let ((new-sp
                   ((vector-ref reduction-table (- act)) stack sp goto-table)))
              (if new-sp
                (loop new-sp tok)
                (begin
                  (parser-error-stack-overflow tok)
                  #f)))))))))

;------------------------------------------------------------------------------

; Parser entry point.

(define (parse)
  (let ((phase 'before-header)
        (topfundefs '()))
    (let loop ()
      (preproc-skip-token-sequence global-pps)
      (let ((x (parse-aux global-pps)))
        (if x
            (begin

              (if (and (eq? phase 'before-header)
                       (not (module-attribute? x)))
                  (begin
                    (parser-error-module-attribute-expected x)
                    (set! phase 'header)))

              (cond ((module-attribute? x)
                     (if (not (eq? phase 'before-header))
                         (parser-error-illplaced-module-attribute x))
                     (parser-set-module-name!
                      (module-attribute-module-name x))
                     (set! phase 'header)
                     (loop))

                    ((export-attribute? x)
                     (if (not (eq? phase 'header))
                         (parser-error-illplaced-export-attribute x))
                     (parser-add-export-list!
                      (export-attribute-symbol-arity-list x))
                     (set! phase 'header)
                     (loop))

                    ((import-attribute? x)
                     (if (not (eq? phase 'header))
                         (parser-error-illplaced-import-attribute x))
                     (parser-add-import-list!
                      (import-attribute-module-name x)
                      (import-attribute-symbol-arity-list x))
                     (set! phase 'header)
                     (loop))

                    ((compile-attribute? x)
                     (if (not (eq? phase 'header))
                         (parser-error-illplaced-compile-attribute x))
                     (parser-add-compile-options!
                      (compile-attribute-option-list x))
                     (set! phase 'header)
                     (loop))

                    ((wild-attribute? x)
                     (if (not (eq? phase 'header))
                         (parser-error-illplaced-wild-attribute x))
                     (parser-add-wild!
                      (wild-attribute-name x)
                      (wild-attribute-term x))
                     (set! phase 'header)
                     (loop))

                    ((record-declaration? x)
                     (parser-add-record-definition!
                      (record-declaration-name x)
                      (record-declaration-fields x))
                     (loop))

                    ((end-of-source? x)
                     #f)

                    (else
                     (set! topfundefs (cons x topfundefs))
                     (set! phase 'after-header)
                     (loop))))

            (loop)))) ; parse error... skip token sequence

    (make-prog (reverse topfundefs))))

(define global-pps #f)

(define (parser-setup filename abs-filename options)
  (let ((pps (preproc-setup filename abs-filename options)))
    (set! global-pps pps)))

(define (parser-cleanup)
  (preproc-cleanup global-pps)
  (set! global-pps #f))

;------------------------------------------------------------------------------

; Handling of attributes.

(define (make-module-attribute location module-name)
  (vector 'module-attribute location module-name))

(define (module-attribute? x)
  (and (vector? x)
       (= (vector-length x) 3)
       (eq? (vector-ref x 0) 'module-attribute)))

(define (module-attribute-location x)
  (vector-ref x 1))

(define (module-attribute-module-name x)
  (vector-ref x 2))

(define (make-export-attribute location symbol-arity-list)
  (vector 'export-attribute location symbol-arity-list))

(define (export-attribute? x)
  (and (vector? x)
       (= (vector-length x) 3)
       (eq? (vector-ref x 0) 'export-attribute)))

(define (export-attribute-location x)
  (vector-ref x 1))

(define (export-attribute-symbol-arity-list x)
  (vector-ref x 2))

(define (make-import-attribute location module-name symbol-arity-list)
  (vector 'import-attribute location module-name symbol-arity-list))

(define (import-attribute? x)
  (and (vector? x)
       (= (vector-length x) 4)
       (eq? (vector-ref x 0) 'import-attribute)))

(define (import-attribute-location x)
  (vector-ref x 1))

(define (import-attribute-module-name x)
  (vector-ref x 2))

(define (import-attribute-symbol-arity-list x)
  (vector-ref x 3))

(define (make-compile-attribute location option-list)
  (vector 'compile-attribute location option-list))

(define (compile-attribute? x)
  (and (vector? x)
       (= (vector-length x) 3)
       (eq? (vector-ref x 0) 'compile-attribute)))

(define (compile-attribute-location x)
  (vector-ref x 1))

(define (compile-attribute-option-list x)
  (vector-ref x 2))

(define (make-wild-attribute location name term)
  (vector 'wild-attribute location name term))

(define (wild-attribute? x)
  (and (vector? x)
       (= (vector-length x) 4)
       (eq? (vector-ref x 0) 'wild-attribute)))

(define (wild-attribute-location x)
  (vector-ref x 1))

(define (wild-attribute-name x)
  (vector-ref x 2))

(define (wild-attribute-term x)
  (vector-ref x 3))

(define (make-record-declaration location name fields)
  (vector 'record-declaration location name fields))

(define (record-declaration? x)
  (and (vector? x)
       (= (vector-length x) 4)
       (eq? (vector-ref x 0) 'record-declaration)))

(define (record-declaration-location x)
  (vector-ref x 1))

(define (record-declaration-name x)
  (vector-ref x 2))

(define (record-declaration-fields x)
  (vector-ref x 3))

(define (make-end-of-source location)
  (vector 'end-of-source location))

(define (end-of-source? x)
  (and (vector? x)
       (= (vector-length x) 2)
       (eq? (vector-ref x 0) 'end-of-source)))

(define (end-of-source-location x)
  (vector-ref x 1))

;------------------------------------------------------------------------------

(define (parser-set-module-name! name-node)
  (if (not (erl-equal?
            (erl-atom->string (node-val1 name-node))
            (pps-module-name global-pps)))
      (parser-error-bad-module-name name-node))
  (dict-assign (pps-macros global-pps) "MODULE" 'special))

(define (get-module-name)
  (pps-module-name global-pps))

(define (parser-add-export-list! exports)
  (for-each
   (lambda (fa)
     (let ((fa-tup
            (erl-tuple (node-val1 (car fa))
                       (erl-int<-exact-integer (node-val1 (cdr fa)))))
           (export-list
            (pps-export-list global-pps)))
       (if (not (erl-cons? (erl-member fa-tup export-list)))
           (pps-export-list-set!
            global-pps
            (erl-cons fa-tup export-list)))))
   exports))

(define (parser-add-import-list! mod-name-node imports)

  (define (find-import m f a import-list)
    (let ((fa-tup (erl-tuple f a)))
      (let loop ((lst import-list))
        (if (erl-nil? lst)
            #f
            (let ((fam-tup (erl-hd lst)))
              (if (and (erl-equal? fa-tup (erl-tuple-ref fam-tup 1))
                       (not (erl-equal? m (erl-tuple-ref fam-tup 2))))
                  fam-tup
                  (loop (erl-tl lst))))))))

  (define (already-imported-from-other-module? m f a)
    (find-import m f a (pps-import-list global-pps)))

  (define (implicitly-imported? f a)
    (find-import (erl-nil) f a implicitly-imported-functions))

  (let ((mod-name (node-val1 mod-name-node)))
    (for-each
     (lambda (fa)
       (let* ((f-node (car fa))
              (f (node-val1 f-node))
              (a-node (cdr fa))
              (a (node-val1 a-node)))
         (cond ((already-imported-from-other-module? mod-name f a)
                =>
                (lambda (x)
                  (parser-error-function-already-imported
                   (generic-location-join f-node a-node)
                   (erl-atom->string (erl-tuple-ref x 2))
                   (erl-atom->string f)
                   (erl-int->exact-integer a))))
               ((implicitly-imported? f a)
                =>
                (lambda (x)
                  (parser-error-function-implicitly-imported
                   (generic-location-join f-node a-node)
                   (erl-atom->string (erl-tuple-ref x 2))
                   (erl-atom->string f)
                   (erl-int->exact-integer a))))
               (else
                (pps-import-list-set!
                 global-pps
                 (erl-cons (erl-tuple (erl-tuple f a) mod-name)
                           (pps-import-list global-pps)))))))
     imports)))

(define (parser-add-wild! name-node term-node)
  (let ((name (node-val1 name-node))
        (term (node-val1 term-node)))
    (pps-wild-list-set!
     global-pps
     (erl-cons (erl-tuple name term)
               (pps-wild-list global-pps)))))

(define (parser-add-record-definition! name-node fields)
  (if (get-record-definition name-node)
      (parser-error-record-already-defined name-node)
      (let loop ((lst1 fields)
                 (lst2 '())
                 (lst3 '()))
        (if (null? lst1)
            (pps-record-defs-set!
             global-pps
             (cons (cons (node-val1 name-node) (reverse lst2))
                   (pps-record-defs global-pps)))
            (let* ((field (car lst1))
                   (field-name-node (car field))
                   (field-name (node-val1 field-name-node))
                   (field-value (cdr field)))
              (if (memq field-name lst3)
                  (parser-error-record-fields-duplicated
                   name-node
                   field-name-node)
                  (loop (cdr lst1)
                        (cons (cons field-name field-value) lst2)
                        (cons field-name lst3))))))))

(define (get-record-definition name-node)
  (assq (node-val1 name-node)
        (pps-record-defs global-pps)))

(define (parser-add-compile-options! option-list)
  (pps-compile-options-set!
   global-pps
   (erl-append (pps-compile-options global-pps)
               (erl-list<-list (map node-val1 option-list)))))

(define (get-compile-options)
  (pps-compile-options global-pps))

(define (get-cwd)
  (let ((x
         (erl-assoc (erl-atom<-string "cwd")
                    (get-compile-options))))
    (if (and (erl-tuple? x)
             (= (erl-tuple-size x) 2)
             (erl-atom? (erl-tuple-ref x 2)))
        (erl-atom->string (erl-tuple-ref x 2))
        "")))

(define (get-outdir)
  (let ((x
         (erl-assoc (erl-atom<-string "outdir")
                    (get-compile-options))))
    (if (and (erl-tuple? x)
             (= (erl-tuple-size x) 2)
             (erl-string? (erl-tuple-ref x 2)))
        (erl-string->string (erl-tuple-ref x 2))
        "")))

(define (get-include-path-list library?)
  (let ((directory-of-current-file
         (filename-directory
          (ss-abs-filename
           (vector-ref (pps-file-nesting global-pps) 1))))
        (cwd
         (get-cwd)))

    (define (get-path-list library?)
      (let loop ((lst1 (get-compile-options))
                 (lst2 '()))
        (if (erl-nil? lst1)
            (reverse lst2)
            (let ((x (erl-hd lst1)))
              (loop (erl-tl lst1)
                    (if (and (erl-tuple? x)
                             (= (erl-tuple-size x) 2)
                             (eq? (erl-tuple-ref x 1)
                                  (erl-atom<-string "i")))
                        (let ((path (erl-tuple-ref x 2)))
                          (cond ((and library?
                                      (erl-atom? path))
                                 (cons (filename-build
                                        cwd
                                        (erl-atom->string path))
                                       lst2))
                                ((and (not library?)
                                      (erl-string? path))
                                 (cons (filename-build
                                        cwd
                                        (erl-string->string path))
                                       lst2))
                                (else
                                 lst2)))
                        lst2))))))

    (cons directory-of-current-file
          (if library?
              (append (get-path-list #f) (get-path-list #t))
              (get-path-list #f)))))

(define (get-start-time)
  (pps-start-time global-pps))

(define (get-import-decl fa)
;  (display "fa:") (pp fa)
;  (display "import-list:") (pp (pps-import-list global-pps))
;  (display "export-list:") (pp (pps-export-list global-pps))
;  (display "wild-list:") (pp (pps-wild-list global-pps))
  (erl-assoc fa (pps-import-list global-pps)))

(define (get-export-list)
  (pps-export-list global-pps))

(define (get-import-list)
  (pps-import-list global-pps))

(define (get-wild-list)
  (pps-wild-list global-pps))

(define (accept ast) ;; should never be called
  ast)

;------------------------------------------------------------------------------
