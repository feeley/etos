#!

; file: "etos.scm"

; Copyright (C) 1998-1999 Universite de Montreal, All Rights Reserved.

; This is the main program of the ETOS compiler.

;------------------------------------------------------------------------------

(include "rt-gambit.scm") ; ETOS library macros
(include "rt-lib.scm") ; ETOS library
(include "port-generic.scm")
(include "utils.scm") ; non-portable utilities
(include "error.scm") ; error handling
(include "ks.scm") ; some constants
(include "parser.scm") ; Erlang parser
(include "bifs.scm") ; database of builtin functions

;------------------------------------------------------------------------------

; Compile an Erlang file.

(define (compile-erlang-file filename options debug)
  (let ((output-port
         (current-output-port))
        (verbose
         (erl-true? (erl-member (erl-atom<-string "verbose") options))))

    (if verbose
        (begin
          (display "compiling " output-port)
          (write filename output-port)
          (newline output-port)))

    (error-setup filename debug output-port)

    (error-warnings-errors-select options)

    (catch-compiler-abort
     (lambda ()
       #f)
     (lambda ()
       (let ((abs-filename (filename->abs-filename filename)))
         (if (and abs-filename
                  (filename-exists? abs-filename))
             (compile-erlang-file-helper filename abs-filename options)
             (begin
               (file-not-found-error #f filename)
               (compiler-abort))))))

    (if verbose
        (error-summary-display output-port))

    (error-cleanup)))

(define (compile-erlang-file-helper filename abs-filename options)
  (parser-setup filename abs-filename options)
  (catch-compiler-abort
   (lambda ()
     (parser-cleanup)
     (compiler-abort))
   (lambda ()
     (let ((ast (parse)))
       (if (= error-counter 0)
           (let ((new-options (get-compile-options)))
             (error-warnings-errors-select new-options)
             (let ((exp-scm (compile-ast-to-scheme ast)))
               (if (= error-counter 0)
                   (let* ((exp
                           (car exp-scm))
                          (scm
                           (cdr exp-scm))
                          (outdir
                           (get-outdir))
                          (scm-filename
                           (filename-build
                            outdir
                            (string-append
                             (filename-strip-extension filename)
                             ".scm"))))
                     (with-output-to-file scm-filename (lambda () (pretty-print scm)))
                     (if (not (erl-true? (erl-member (erl-atom<-string "scm")
                                                     new-options)))
                         (compile-scheme-file scm-filename)))))))
       (parser-cleanup)))))

(define (get-etos-root)
  (let* ((arg0 (car (get-command-line-arguments)))
         (dir (filename-directory (filename->abs-filename arg0))))
    (if (string=? dir (current-directory))
        (get-install-dir)
        dir)))

(define (erl-term<-string str)
  (display "-Dname=value is not implemented")
  (newline)
  (exit-with-value 1))


#|
  (call-with-current-continuation
   (lambda (kont)
     (for-each display (list "Compiling '" filename "'."))
     (newline)
     (error-setup! filename (memq 'report_warnings options))
     (set! compiler.abort (lambda (x) (kont #f)))
     (let ((pps (preproc-setup filename)))
       (set! global-pps pps)
       (for-each (lambda (option) (add-compile! #f option)) options)
       (let* ((exp-scm (compile (parse-aux pps)))

	      (module (get-module-name))
	      (opath (pps-opath global-pps)))
	 (preproc-cleanup pps)
	 (set! global-pps #f)
	 (set! compiler.abort (lambda (x) (exit x))) ;restore normal abortion.
	 (and exp-scm
	      (let ((scm-name (string-append opath
					     "/"
                                             module
					     ".scm"))
		    (erl-name (string-append opath
					     "/"
                                             module
					     "_exp.erl")))
		(with-output-to-file scm-name (lambda () (pp (cdr exp-scm))))
		(if (memq 'expansion options)
		    (with-output-to-file erl-name
		      (lambda () (pp-ast (car exp-scm)))))
		(and (scm.compile scm-name)
		     module))))))))
|#

;------------------------------------------------------------------------------

; Parse command-line arguments.

(define (parse-args cwd args)
  (let ((debug #f)
        (option-expansion #f)
        (option-verbose #f)
        (option-report_warnings #f)
        (option-defines '())
        (option-b #f)
        (option-report_errors #t)
        (option-cwd cwd)
        (option-outdir cwd)
        (option-include-lib-dirs '())
        (option-include-dirs '()))

    (define (done filenames)
      (vector filenames
              debug
              (erl-list<-list
               (append
                (if option-verbose
                    (list (erl-atom<-string "verbose"))
                    '())
                (if option-report_warnings
                    (list (erl-atom<-string "report_warnings"))
                    '())
                (map (lambda (d)
                       (if (= (vector-length d) 1)
                           (erl-tuple (erl-atom<-string "d")
                                      (erl-atom<-string (vector-ref d 0)))
                           (erl-tuple (erl-atom<-string "d")
                                      (erl-atom<-string (vector-ref d 0))
                                      (erl-term<-string (vector-ref d 1)))))
                     option-defines)
                (if option-b
                    (list (erl-atom<-string option-b))
                    '())
                (if option-report_errors
                    (list (erl-atom<-string "report_errors"))
                    '())
                (if option-cwd
                    (list (erl-tuple (erl-atom<-string "cwd")
                                     (erl-atom<-string option-cwd)))
                    '())
                (if option-outdir
                    (list (erl-tuple (erl-atom<-string "outdir")
                                     (erl-string<-string option-outdir)))
                    '())
                (map (lambda (x)
                       (erl-tuple (erl-atom<-string "i") x))
                     (append (map (lambda (str) (erl-atom<-string str))
                                  (reverse option-include-lib-dirs))
                             (map (lambda (str) (erl-string<-string str))
                                  (reverse option-include-dirs))))
                (if option-expansion
                    (list (erl-atom<-string "E"))
                    '())))))

    (let loop3 ((args args))
      (if (null? args)
          (done '())
          (let ((arg (car args))
                (rest (cdr args)))

            (define (option-name-prefixed-with name)
              (let ((n (string-length name)))
                (and (> (string-length arg) n)
                     (string=? (substring arg 1 (+ n 1)) name)
                     (substring arg (+ n 1) (string-length arg)))))

            (define (option-name-is name)
              (let ((x (option-name-prefixed-with name)))
                (and x
                     (string=? x ""))))

            (if (and (> (string-length arg) 0)
                     (char=? (string-ref arg 0) #\-))

                (cond ((or (option-name-prefixed-with "b")
                           (option-name-prefixed-with "I")
                           (option-name-prefixed-with "ilroot")
                           (option-name-prefixed-with "o"))
                       =>
                       (lambda (x)

                         (define (option-set-to val)
                           (cond ((option-name-prefixed-with "b")
                                  (set! option-b val))
                                 ((option-name-prefixed-with "I")
                                  (set! option-include-dirs
                                        (cons val option-include-dirs)))
                                 ((option-name-prefixed-with "ilroot")
                                  (set! option-include-lib-dirs
                                        (cons val option-include-lib-dirs)))
                                 (else
                                  (set! option-outdir val))))

                         (if (string=? x "")
                             (if (null? rest)
                                 (command-error-incomplete-option arg)
                                 (let ((val (car rest)))
                                   (option-set-to val)
                                   (loop3 (cdr rest))))
                             (begin
                               (option-set-to x)
                               (loop3 rest)))))

                      ((option-name-is "d")
                       (set! debug #t)
                       (loop3 rest))

                      ((option-name-prefixed-with "D")
                       =>
                       (lambda (x)
                         (let ((n (string-length x)))
                           (let loop4 ((i 0))
                             (if (>= i n)
                                 (begin
                                   (set! option-defines
                                         (cons (vector x)
                                               option-defines))
                                   (loop3 rest))
                                 (let ((c (string-ref x i)))
                                   (if (char=? c #\=)
                                       (let ((name (substring x 0 i))
                                             (val (substring x (+ i 1) n)))
                                         (begin
                                           (set! option-defines
                                                 (cons (vector name val)
                                                       option-defines))
                                           (loop3 rest)))
                                         (loop4 (+ i 1)))))))))

                      ((option-name-is "help")
                       (command-error-usage))

                      ((option-name-is "v")
                       (set! option-verbose #t)
                       (loop3 rest))

                      ((option-name-is "W")
                       (set! option-report_warnings #t)
                       (loop3 rest))

                      ((option-name-is "E")
                       (set! option-expansion #t)
                       (loop3 rest))

                      ((option-name-is "-")
                       (done rest))

                      (else
                       (command-error-usage)))

                (done args)))))))

(define (command-error-incomplete-option option)
  (display "etos: No value given for ")
  (display option)
  (display " option")
  (newline)
  (exit-with-value 1))

(define (command-error-usage)
  (display "Usage:  etos [options] file1.erl ...")
  (newline)
  (display "Options:")
  (newline)
  (display "-b type        type of output file (e.g. o1 or scm)")
  (newline)
  (display "-d             turn on debugging of etos itself")
  (newline)
  (display "-Dname         define name")
  (newline)
  (display "-Dname=value   define name to have value (an Erlang term)")
  (newline)
  (display "-help          shows this help text")
  (newline)
  (display "-ilroot path   where to search for include_lib files")
  (newline)
  (display "-I path        where to search for include files")
  (newline)
  (display "-o path        name output directory")
  (newline)
  (display "-v             verbose compiler output")
  (newline)
  (display "-W             enable warnings")
  (newline)
  (display "-E             generate listing of expanded code in file1.E")
  (newline)
  (display "--             signal end of options")
  (newline)
  (exit-with-value 1))

(define (main)
  (let ((cwd (get-current-working-directory))
        (args (cdr (get-command-line-arguments))))
    (let ((x (parse-args cwd args)))
      (let ((filenames (vector-ref x 0))
            (debug (vector-ref x 1))
            (options (vector-ref x 2)))
        (for-each (lambda (filename)
                    (compile-erlang-file filename options debug))
                  filenames)))))

(main)

;------------------------------------------------------------------------------
