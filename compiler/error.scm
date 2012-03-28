; file: "error.scm"

; Copyright (C) 1998-1999 Universite de Montreal, All Rights Reserved.

; This file contains the error handling routines of the ETOS compiler.

;------------------------------------------------------------------------------

; Get location from a token, AST node or location object.

(define (generic-location loc-obj)
  (cond ((location? loc-obj)
         loc-obj)
        ((node? loc-obj)
         (node-location loc-obj)) ; this may return #f
        ((token? loc-obj)
         (token-location loc-obj))
        ((module-attribute? loc-obj)
         (module-attribute-location loc-obj))
        ((export-attribute? loc-obj)
         (export-attribute-location loc-obj))
        ((import-attribute? loc-obj)
         (import-attribute-location loc-obj))
        ((compile-attribute? loc-obj)
         (compile-attribute-location loc-obj))
        ((wild-attribute? loc-obj)
         (wild-attribute-location loc-obj))
        ((record-declaration? loc-obj)
         (record-declaration-location loc-obj))
        ((end-of-source? loc-obj)
         (end-of-source-location loc-obj))
        (else
         #f)))

(define (generic-location-join loc-obj1 loc-obj2)
  (let ((loc1 (generic-location loc-obj1))
        (loc2 (generic-location loc-obj2)))
    (and loc1 loc2 (location-join loc1 loc2))))

(define (generic-location-join-multi loc-objs)
  (if (null? loc-objs)
      #f
      (let loop ((lst loc-objs))
        (if (null? (cdr lst))
            (generic-location-join (car loc-objs) (car lst))
            (loop (cdr lst))))))

;------------------------------------------------------------------------------

; Error handling.

(define max-errors #f)
(define warning-counter #f)
(define warnings-enabled? #f)
(define error-counter #f)
(define errors-enabled? #f)
(define debug? #f)
(define error-port #f)

(define (error-setup filename debug port)
  (set! max-errors 200) ; Stop compiler when this number of errors is reached
  (set! warning-counter 0)
  (set! warnings-enabled? #f)
  (set! error-counter 0)
  (set! errors-enabled? #f)
  (set! debug? debug)
  (set! error-port port))

(define (error-cleanup)
  (set! max-errors #f)
  (set! warning-counter #f)
  (set! warnings-enabled? #f)
  (set! error-counter #f)
  (set! errors-enabled? #f)
  (set! debug? #f)
  (set! error-port #f))

(define (error-warnings-errors-select options)
  (let ((warnings
         (erl-true? (erl-member (erl-atom<-string "report_warnings") options)))
        (errors
         (erl-true? (erl-member (erl-atom<-string "report_errors") options))))
    (set! warnings-enabled? warnings)
    (set! errors-enabled? errors)))

(define (increment-error-counter)
  (set! error-counter (+ error-counter 1))
  (if (>= error-counter max-errors)
      (begin
        (display "too many errors" error-port)
        (newline error-port)
        (compiler-abort))))

(define (increment-warning-counter)
  (set! warning-counter (+ warning-counter 1)))

; Display error messages.

(define (internal-compiler-error proc . msgs)
  (display "*** internal compiler error in procedure " error-port)
  (write proc error-port)
  (display ": " error-port)
  (for-each (lambda (x) (display x error-port)) msgs)
  (newline error-port)
  (display "*** please send a bug report to etos@iro.umontreal.ca" error-port)
  (newline error-port)
  (increment-error-counter)
  (compiler-abort))

(define (compiler-error loc-obj . msgs)
  (if errors-enabled?
      (begin
        (location-display (generic-location loc-obj) error-port)
        (for-each (lambda (x) (display x error-port)) msgs)
        (newline error-port)))
  (increment-error-counter))

(define (compiler-warning loc-obj . msgs)
  (if warnings-enabled?
      (begin
        (location-display (generic-location loc-obj) error-port)
        (display "Warning: " error-port)
        (for-each (lambda (x) (display x error-port)) msgs)
	(newline error-port)))
  (increment-warning-counter))

(define (file-not-found-error loc-obj filename)
  (if (or errors-enabled? (not loc-obj))
      (begin
        (location-display (generic-location loc-obj) error-port)
        (display "file " error-port)
        (write filename error-port)
        (display " not found" error-port)
        (newline error-port)))
  (increment-error-counter))

(define (error-summary-display port)

  (define (pluralize n str)
    (if (= n 0)
        (begin
          (display "no " port)
          (display str port))
        (begin
          (display n port)
          (display " " port)
          (display str port)
          (if (> n 1)
              (display "s" port)))))

  (if (> error-counter 0)
      (display "compilation terminated abnormally: " port)
      (display "compilation ended: " port))

  (pluralize error-counter "error")
  (display " and " port)
  (pluralize warning-counter "warning")
  (newline port))

(define compiler-abort-continuation #f)

(define (compiler-abort)
  (let ((p compiler-abort-continuation))
    (if p
        (p (vector 'abort))
        (begin ; display on current-output-port in case error-setup! not called
          (display "no exception handler for compiler abort")
          (newline)
          (exit-with-value 1)))))

(define (catch-compiler-abort handler thunk)
  (let* ((old-compiler-abort-continuation
          compiler-abort-continuation)
         (result
          (call-with-current-continuation
           (lambda (cont)
             (set! compiler-abort-continuation cont)
             (vector 'normal (thunk))))))
    (set! compiler-abort-continuation old-compiler-abort-continuation)
    (if (eq? (vector-ref result 0) 'normal)
        (vector-ref result 1)
        (handler))))

; Internal errors

(define (internal-error!!! . msgs)
  (apply internal-compiler-error (cons 'unknown msgs)))

;------------------------------------------------------------------------------

; Error messages.

; From scanner.

(define (scanner-error ss start-pos end-pos msg)
  (compiler-error (make-location (ss-filename ss)
				 start-pos
				 end-pos)
		  msg))

(define (scanner-error-invalid-unicode-escape ss start-pos end-pos)
  (scanner-error
   ss
   start-pos
   end-pos
   "invalid Unicode escape"))

(define (scanner-error-invalid-control-escape ss start-pos end-pos)
  (scanner-error
   ss
   start-pos
   end-pos
   "invalid control escape"))

(define (scanner-error-invalid-escape-sequence ss start-pos end-pos)
  (scanner-error
   ss
   start-pos
   end-pos
   "invalid escape sequence"))

(define (scanner-error-invalid-character ss start-pos end-pos)
  (scanner-error
   ss
   start-pos
   end-pos
   "invalid character"))

(define (scanner-error-invalid-char-literal ss start-pos end-pos)
  (scanner-error
   ss
   start-pos
   end-pos
   "invalid character literal"))

(define (scanner-error-multiline-string-literal ss start-pos end-pos)
  (scanner-error
   ss
   start-pos
   end-pos
   "string literal spans more than one line"))

(define (scanner-error-control-char-in-string-literal ss start-pos end-pos)
  (scanner-error
   ss
   start-pos
   end-pos
   "string literal contains a control character"))

(define (scanner-error-multiline-atom-literal ss start-pos end-pos)
  (scanner-error
   ss
   start-pos
   end-pos
   "atom literal spans more than one line"))

(define (scanner-error-control-char-in-atom-literal ss start-pos end-pos)
  (scanner-error
   ss
   start-pos
   end-pos
   "atom literal contains a control character"))

; From preprocessor.

(define (pp-error-end-of-file t1 t2)
  (compiler-error (generic-location-join t1 t2)
		  "token sequence is not terminated with full stop"))

(define (pp-error-macro-name-expected t)
  (compiler-error t "macro name expected"))

(define (pp-error-macro-previously-defined t)
  (compiler-error t "macro previously defined"))

(define (pp-error-undefined-macro t)
  (compiler-error t "macro is not defined"))

(define (pp-error-string-literal-expected t)
  (compiler-error t "string literal expected"))

(define (pp-error-unsigned-decimal-literal-expected t)
  (compiler-error t "unsigned decimal literal expected"))

(define (pp-error-FullStop-expected t)
  (compiler-error t "full stop expected"))

(define (pp-error-close-paren-expected t)
  (compiler-error t "closing parenthesis expected"))

(define (pp-error-open-paren-expected t)
  (compiler-error t "opening parenthesis expected"))

(define (pp-error-comma-expected t)
  (compiler-error t "comma expected"))

(define (pp-error-no-matching-endif-directive loc)
  (compiler-error loc "no matching \"endif\" preprocessor directive"))

(define (pp-error-if-directive-reserved t)
  (compiler-error t "\"if\" preprocessor directive is reserved"))

(define (pp-error-elif-directive-reserved t)
  (compiler-error t "\"elif\" preprocessor directive is reserved"))

(define (pp-error-unexpected-else t)
  (compiler-error t "unexpected \"else\" preprocessor directive"))

(define (pp-error-unexpected-endif t)
  (compiler-error t "unexpected \"endif\" preprocessor directive"))

(define (pp-error-variable-expected t)
  (compiler-error t "variable expected"))

(define (pp-error-wrong-nb-args t1 t2)
  (compiler-error (generic-location-join t1 t2)
                  "wrong number of arguments"))

(define (pp-error-unbalanced-macro-argument t1 t2)
  (compiler-error (generic-location-join t1 t2)
                  "unbalanced macro argument"))

; From parser.

(define (parser-error-stack-overflow tok)
  (compiler-error tok "parsing stack overflow"))

(define (parser-error-unexpected-token tok)
  (compiler-error tok "unexpected token"))

(define (parser-error-bad-module-name loc-obj)
  (compiler-error loc-obj "module name and file name must match"))

(define (parser-error-module-attribute-expected loc)
  (compiler-error loc "module attribute expected"))

(define (parser-error-illplaced-module-attribute loc)
  (compiler-error loc "ill-placed module attribute"))

(define (parser-error-illplaced-export-attribute loc)
  (compiler-error loc "ill-placed export attribute"))

(define (parser-error-illplaced-import-attribute loc)
  (compiler-error loc "ill-placed import attribute"))

(define (parser-error-illplaced-compile-attribute loc)
  (compiler-error loc "ill-placed compile attribute"))

(define (parser-error-illplaced-wild-attribute loc)
  (compiler-error loc "ill-placed wild attribute"))

(define (parser-error-function-already-imported loc-obj m f a)
  (compiler-error loc-obj
                  "can't change previous import of "
                  m
                  ":"
                  f
                  "/"
                  a))

(define (parser-error-function-implicitly-imported loc-obj m f a)
  (compiler-error loc-obj
                  "can't shadow implicit import of "
                  m
                  ":"
                  f
                  "/"
                  a))

(define (parser-error-record-already-defined loc-obj)
  (compiler-error loc-obj "record type already defined"))

(define (parser-error-record-fields-duplicated name-node field-name-node)
  (compiler-error field-name-node "duplicate record field name"))

(define (parser-error-undefined-record-field loc-obj)
  (compiler-error loc-obj "undefined record field"))

(define (parser-error-undefined-record-type loc-obj)
  (compiler-error loc-obj "undefined record type"))

(define (parser-error-invalid-function-name loc-obj)
  (compiler-error loc-obj "invalid function name"))

; From analyzer.

;*************************haven't checked following:

(define (ast-error loc-obj . msgs)
  (apply compiler-error (cons loc-obj msgs))
  (newast-error))

(define (analyzer-error-mismatching-function-arity loc-obj)
  (ast-error loc-obj "mismatching function arity"))

(define (analyzer-error-mismatching-function-name loc-obj)
  (ast-error loc-obj "mismatching function name"))

(define (analyzer-error-unbound-variable loc-obj vname)
  (ast-error loc-obj "unbound variable " vname))

(define (analyzer-error-already-bound loc-obj)
  (ast-error loc-obj "variable already bound"))

(define (analyzer-error-invalid-guard loc-obj)
  (ast-error loc-obj "invalid guard"))

(define (analyzer-error-invalid-guard-expression loc-obj)
  (ast-error loc-obj "invalid guard expression"))

(define (analyzer-error-undefined-record-type loc-obj)
  (ast-error loc-obj "undefined record type"))

(define (analyzer-error-invalid-record-type loc-obj)
  (ast-error loc-obj "invalid record type"))

(define (analyzer-error-undefined-function loc-obj fname)
  (ast-error loc-obj "undefined function " fname))

(define (analyzer-warning-unused-variable loc-obj var)
  (compiler-warning loc-obj "unused variable " var))

; From generator.

(define (generator-error-abort)
  (compiler-abort))

;------------------------------------------------------------------------------
