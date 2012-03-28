; file: "build-parser.scm"

(display "Building \"erlang.scm\"...")

;; To avoid annoying warning at compile-time.
;; important not to put this after the include...
;; Gambit would see it as a constant(block flag)
(define erlang-grammar #f)

(include "lalr-scm/lalr.scm")

(load "grammar.scm")

(gen-lalr1 erlang-grammar "erlang.scm")

(display "done.") (newline)


