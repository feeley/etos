; file: "scanner.scm"

; Copyright (C) 1998-1999 Universite de Montreal, All Rights Reserved.

; This is the scanner for the ETOS compiler.

;------------------------------------------------------------------------------

; Scanner interface:
;
; (scanner-setup file abs-file)  returns a "scanner state" object
; (scanner-cleanup ss)           closes port of "scanner state" object "ss"
; (scanner-read-token ss)        read next token and return it
;
; (token-kind token)             access "kind" field of a token
; (token-filename token)         access "filename" field of a token
; (token-start-pos token)        access "start-pos" field of a token
; (token-end-pos token)          access "end-pos" field of a token
; (token-value token)            access "value" field of a token
; (token-location token)         get source code location of token
;
; (location-filename loc)        get filename of a source code location
; (location-start-pos loc)       get start-pos of a source code location
; (location-end-pos loc)         get end-pos of a source code location
; (location-display loc port)    display a source code location on port
; (location-join loc1 loc2)      combine two locations (start and end)
;
; (position->line pos)           get line number of a token position
; (position->column pos)         get column number of a token position

;------------------------------------------------------------------------------

; "Scanner state" objects.

(define (make-ss)
  (vector #f #f #f #f #f #f #f #f #f #f #f #f))

(define (ss-filename ss)                               (vector-ref ss 0))
(define (ss-filename-set! ss x)                        (vector-set! ss 0 x))
(define (ss-abs-filename ss)                           (vector-ref ss 1))
(define (ss-abs-filename-set! ss x)                    (vector-set! ss 1 x))
(define (ss-port ss)                                   (vector-ref ss 2))
(define (ss-port-set! ss x)                            (vector-set! ss 2 x))
(define (ss-current-char-pos ss)                       (vector-ref ss 3))
(define (ss-current-char-pos-set! ss x)                (vector-set! ss 3 x))
(define (ss-current-line ss)                           (vector-ref ss 4))
(define (ss-current-line-set! ss x)                    (vector-set! ss 4 x))
(define (ss-current-line-pos ss)                       (vector-ref ss 5))
(define (ss-current-line-pos-set! ss x)                (vector-set! ss 5 x))
(define (ss-previous-non-backslash-char-pos ss)        (vector-ref ss 6))
(define (ss-previous-non-backslash-char-pos-set! ss x) (vector-set! ss 6 x))
(define (ss-peeked-char ss)                            (vector-ref ss 7))
(define (ss-peeked-char-set! ss x)                     (vector-set! ss 7 x))
(define (ss-peeked-char-pos ss)                        (vector-ref ss 8))
(define (ss-peeked-char-pos-set! ss x)                 (vector-set! ss 8 x))
(define (ss-pos-window ss)                             (vector-ref ss 9))
(define (ss-pos-window-set! ss x)                      (vector-set! ss 9 x))
(define (ss-char-window ss)                            (vector-ref ss 10))
(define (ss-char-window-set! ss x)                     (vector-set! ss 10 x))
(define (ss-window-size ss)                            (vector-ref ss 11))
(define (ss-window-size-set! ss x)                     (vector-set! ss 11 x))

; Token objects.

(define (make-token kind filename start-pos end-pos value)
  (vector 'token kind filename start-pos end-pos value))

(define (token? x)
  (and (vector? x)
       (> (vector-length x) 0)
       (eq? (vector-ref x 0) 'token)))

(define (token-kind token)      (vector-ref token 1))
(define (token-filename token)  (vector-ref token 2))
(define (token-start-pos token) (vector-ref token 3))
(define (token-end-pos token)   (vector-ref token 4))
(define (token-value token)     (vector-ref token 5))

(define (token-location tok)
  (make-location (token-filename tok)
                 (token-start-pos tok)
                 (token-end-pos tok)))

; Location objects.

(define (make-location filename start-pos end-pos)
  (vector 'location filename start-pos end-pos))

(define (location? x)
  (and (vector? x)
       (> (vector-length x) 0)
       (eq? (vector-ref x 0) 'location)))

(define (location-filename loc)  (vector-ref loc 1))
(define (location-start-pos loc) (vector-ref loc 2))
(define (location-end-pos loc)   (vector-ref loc 3))

(define use-gnu-error-style #f)

(define (location-display loc port)
  (if loc
      (let ((filename (location-filename loc))
            (start-pos (location-start-pos loc))
            (end-pos (location-end-pos loc)))
        (if use-gnu-error-style
            (begin
              (display filename port)
              (display ":" port)
              (display (position->line start-pos) port))
            (begin
              (write filename port)
              (display "@" port)
              (display (position->line start-pos) port)
              (display "." port)
              (display (position->column start-pos) port)
              (display "-" port)
              (display (position->line end-pos) port)
              (display "." port)
              (display (position->column end-pos) port)))
        (display ": " port))))

(define (location-join loc1 loc2)
  (make-location (location-filename loc1)
                 (location-start-pos loc1)
                 (location-end-pos loc2)))

(define (scanner-setup filename abs-filename)
  (let ((ss (make-ss))
        (port (open-binary-input-file abs-filename)))
    (ss-filename-set! ss abs-filename) ; error messages will use abs filename
    (ss-abs-filename-set! ss abs-filename)
    (ss-port-set! ss port)
    (ss-current-char-pos-set! ss 0)
    (ss-current-line-pos-set! ss 0)
    (ss-current-line-set! ss 0)
    (ss-previous-non-backslash-char-pos-set! ss 0)
    (ss-peeked-char-set! ss #f)
    (ss-peeked-char-pos-set! ss #f)
    (ss-pos-window-set! ss (make-vector 5 #f))
    (ss-char-window-set! ss (make-vector 5 #f))
    (ss-window-size-set! ss 0)
    ss))

(define (scanner-cleanup ss)
  (close-input-port (ss-port ss)))

(define (scanner-get-char ss)

  (define (read-char-possibly-as-unicode-escape)
    (let ((c (read-char (ss-port ss))))
      (cond ((eof-object? c)
             (ss-previous-non-backslash-char-pos-set! ss
               (ss-current-char-pos ss))
             eof-ch)
            ((char=? c #\\)
             (if (and (eqv? (peek-char (ss-port ss)) #\u)
                      (even? (- (ss-current-char-pos ss)
                                (ss-previous-non-backslash-char-pos ss))))
               (let ((unicode-escape-pos (ss-current-char-pos ss)))
                 (read-char (ss-port ss)) ; skip the #\u
                 (ss-current-char-pos-set! ss
                   (+ (ss-current-char-pos ss) 2))
                 (let loop1 ()
                   (if (eqv? (peek-char (ss-port ss)) #\u)
                     (begin
                       (read-char (ss-port ss)) ; skip the #\u
                       (ss-current-char-pos-set! ss
                         (+ (ss-current-char-pos ss) 1))
                       (loop1))))
                 (let loop2 ((i 0) (n 0))
                   (if (= i 4)
                     n
                     (let ((c (peek-char (ss-port ss))))
                       (let ((x (and (not (eof-object? c))
                                     (class-ref hexadecimal-class
                                                (char->unicode c)))))
                         (if x
                           (begin
                             (read-char (ss-port ss)) ; skip the hex digit
                             (ss-current-char-pos-set! ss
                               (+ (ss-current-char-pos ss) 1))
                             (loop2 (+ i 1) (+ (* n 16) x)))
                           (begin
                             (scanner-error-invalid-unicode-escape
                              ss
                              (scanner-char-pos->position
                               ss
                               unicode-escape-pos)
                              (scanner-char-pos->position
                               ss
                               (+ (ss-current-char-pos ss) 1)))
                             (read-char-possibly-as-unicode-escape))))))))
               (begin
                 (ss-current-char-pos-set! ss
                   (+ (ss-current-char-pos ss) 1))
                 (char->unicode c))))
            (else
             (ss-current-char-pos-set! ss
               (+ (ss-current-char-pos ss) 1))
             (ss-previous-non-backslash-char-pos-set! ss
               (ss-current-char-pos ss))
             (char->unicode c)))))

  (let ((c (or (ss-peeked-char ss) ; use previous character if available
               (read-char-possibly-as-unicode-escape))))
    (ss-peeked-char-set! ss #f)
    (cond ((= c lf-ch)
           (ss-current-line-set! ss (+ (ss-current-line ss) 1))
           (ss-current-line-pos-set! ss (ss-current-char-pos ss))
           eol-ch)
          ((= c cr-ch)
           (ss-peeked-char-pos-set! ss ; set to position of char after CR
             (ss-current-char-pos ss))
           (ss-current-line-set! ss (+ (ss-current-line ss) 1))
           (ss-current-line-pos-set! ss (ss-current-char-pos ss))
           (let ((next (read-char-possibly-as-unicode-escape)))
             (if (= next lf-ch)
               (ss-current-line-pos-set! ss (ss-current-char-pos ss))
               (ss-peeked-char-set! ss next)) ; remember for next time
             eol-ch))
          (else
           c))))

(define scanner-max-lines 65536)

(define (scanner-char-pos->position ss char-pos)
  (+ (ss-current-line ss)
     (* (- char-pos (ss-current-line-pos ss))
        scanner-max-lines)))

(define (position->line pos)
  (+ (modulo pos scanner-max-lines) 1))

(define (position->column pos)
  (+ (quotient pos scanner-max-lines) 1))

(define (scanner-advance ss i)
  (scanner-prepare-window ss i)
  (let loop ((i i) (j 0))
    (if (< i (ss-window-size ss))
      (begin
        (vector-set! (ss-pos-window ss) j
          (vector-ref (ss-pos-window ss) i))
        (vector-set! (ss-char-window ss) j
          (vector-ref (ss-char-window ss) i))
        (loop (+ i 1) (+ j 1)))
      (ss-window-size-set! ss j))))

(define (scanner-lookahead-pos ss i)
  (scanner-prepare-window ss (+ i 1))
  (vector-ref (ss-pos-window ss) i))

(define (scanner-lookahead-char ss i)
  (scanner-prepare-window ss (+ i 1))
  (vector-ref (ss-char-window ss) i))

(define (scanner-prepare-window ss i)
  (let ((j (ss-window-size ss)))
    (if (< j i)
      (scanner-fill-window ss i j))))

(define (scanner-fill-window ss i j)
  (let loop ((j j))
    (if (< j i)
      (let* ((p
              (scanner-char-pos->position
               ss
               (if (ss-peeked-char ss)
                 (ss-peeked-char-pos ss)
                 (ss-current-char-pos ss))))
             (c
              (scanner-get-char ss)))
        (vector-set! (ss-pos-window ss) j p)
        (vector-set! (ss-char-window ss) j c)
        (loop (+ j 1)))
      (ss-window-size-set! ss j))))

(define (scanner-read-token ss) ; read next token

  (define (skip-comment)
    (let loop ()
      (let ((x (scanner-lookahead-char ss 0)))
        (scanner-advance ss 1)
        (if (not (class-ref eol-class x))
          (loop)))))

  (define (parse-identifier)
    (let* ((start-pos
            (scanner-lookahead-pos ss 0))
           (first-ch
            (scanner-lookahead-char ss 0))
           (str
            (begin
              (scanner-advance ss 1)
              (let loop ((i 1))
                (let ((c (scanner-lookahead-char ss 0)))
                  (if (class-ref ident-class c)
                    (begin
                      (scanner-advance ss 1)
                      (let ((s (loop (+ i 1))))
                        (string-set! s i (unicode->char c))
                        s))
                    (make-string i (unicode->char first-ch)))))))
           (end-pos
            (scanner-lookahead-pos ss 0))
           (h
            (scanner-hash-string str))
           (x
            (vector-ref scanner-keyword-perfect-hashtable h)))
      (cond ((and x (string=? str (car x)))
             (make-token
              (cdr x)
              (ss-filename ss)
              start-pos
              end-pos
              #f))
            ((eq? (class-ref ident-class first-ch) 'uppercase)
             (if (and (= first-ch underscore-ch)
                     (= (string-length str) 1))
               (make-token
                UniversalPattern-tok
                (ss-filename ss)
                start-pos
                end-pos
                #f)
               (make-token
                Variable-tok
                (ss-filename ss)
                start-pos
                end-pos
                str)))
            (else
             (make-token
              NotQuotedAtomLiteral-tok
              (ss-filename ss)
              start-pos
              end-pos
              str)))))

  (define (parse-escape)
    (let* ((c1 (scanner-lookahead-char ss 1))
           (d1 (class-ref octal-class c1)))
      (cond (d1
             (let* ((c2 (scanner-lookahead-char ss 2))
                    (d2 (class-ref octal-class c2)))
               (if d2
                 (let* ((c3 (scanner-lookahead-char ss 3))
                        (d3 (class-ref octal-class c3)))
                   (if (and d3 (< d1 4))
                     (begin
                       (scanner-advance ss 4)
                       (+ (* (+ (* d1 8) d2) 8) d3))
                     (begin
                       (scanner-advance ss 3)
                       (+ (* d1 8) d2))))
                 (begin
                   (scanner-advance ss 2)
                   d1))))
            ((= c1 caret-ch)
             (let ((c2 (scanner-lookahead-char ss 2)))
               (if (and (>= c2 #x40) (<= c2 #x5f))
                 (begin
                   (scanner-advance ss 3)
                   (- c2 #x40))
                 (let ((c0 (scanner-lookahead-char ss 0)))
                   (scanner-error-invalid-control-escape
                    ss
                    (scanner-lookahead-pos ss 0)
                    (scanner-lookahead-pos ss 3))
                   (scanner-advance ss 3)
                   c0))))
            ((= c1 lower-n-ch)
             (scanner-advance ss 2)
             #xa)
            ((= c1 lower-b-ch)
             (scanner-advance ss 2)
             #x8)
            ((= c1 lower-d-ch)
             (scanner-advance ss 2)
             #x7f)
            ((= c1 lower-e-ch)
             (scanner-advance ss 2)
             #x1b)
            ((= c1 lower-f-ch)
             (scanner-advance ss 2)
             #xc)
            ((= c1 lower-r-ch)
             (scanner-advance ss 2)
             #xd)
            ((= c1 lower-s-ch)
             (scanner-advance ss 2)
             #x20)
            ((= c1 lower-t-ch)
             (scanner-advance ss 2)
             #x9)
            ((= c1 lower-v-ch)
             (scanner-advance ss 2)
             #xb)
            ((= c1 backslash-ch)
             (scanner-advance ss 2)
             #x5c)
            ((= c1 quote-ch)
             (scanner-advance ss 2)
             #x27)
            ((= c1 doublequote-ch)
             (scanner-advance ss 2)
             #x22)
            (else
             (let ((c0 (scanner-lookahead-char ss 0)))
               (scanner-error-invalid-escape-sequence
                ss
                (scanner-lookahead-pos ss 0)
                (scanner-lookahead-pos ss 2))
               (scanner-advance ss 2)
               c0)))))

  (define (parse-char)
    (let ((c (scanner-lookahead-char ss 1)))
      (cond ((or (class-ref space-class c)
                 (= c 127))
             (scanner-error-invalid-char-literal
              ss
              (scanner-lookahead-pos ss 0)
              (scanner-lookahead-pos ss 2))
             (scanner-advance ss 2)
             c)
            ((= c backslash-ch)
             (scanner-advance ss 1)
             (parse-escape))
            (else
             (scanner-advance ss 2)
             c))))

  (define (parse-string)
    (let* ((close (scanner-lookahead-char ss 0))
           (start-pos (scanner-lookahead-pos ss 0)))

      (define max-chunk-length 512)

      (define (read-chunk)
        (let loop ((i 0))
          (if (< i max-chunk-length)
            (let ((c (scanner-lookahead-char ss 0)))
              (cond ((or (= c close)
                         (= c eof-ch)
                         (= c eol-ch))
                     (if (not (= c close))
                       (if (= close doublequote-ch)
                         (scanner-error-multiline-string-literal
                          ss
                          start-pos
                          (scanner-lookahead-pos ss 0))
                         (scanner-error-multiline-atom-literal
                          ss
                          start-pos
                          (scanner-lookahead-pos ss 0))))
                     (scanner-advance ss 1)
                     (make-string i #\space))
                    ((= c backslash-ch)
                     (let* ((c (parse-escape))
                            (s (loop (+ i 1))))
                       (string-set! s i (unicode->char c))
                       s))
                    (else
                     (if (or (< c space-ch)
                             (= c 127))
                       (if (= close doublequote-ch)
                         (scanner-error-control-char-in-string-literal
                          ss
                          (scanner-lookahead-pos ss 0)
                          (scanner-lookahead-pos ss 1))
                         (scanner-error-control-char-in-atom-literal
                          ss
                          (scanner-lookahead-pos ss 0)
                          (scanner-lookahead-pos ss 1))))
                     (scanner-advance ss 1)
                     (let ((s (loop (+ i 1))))
                       (string-set! s i (unicode->char c))
                       s))))
            (begin
              (scanner-advance ss 1)
              (make-string i #\space)))))

      (scanner-advance ss 1)
      (let ((chunk1 (read-chunk)))
        (if (< (string-length chunk1) max-chunk-length)
          chunk1
          (let loop ((chunks (list chunk1)))
            (let* ((new-chunk (read-chunk))
                   (new-chunks (cons new-chunk chunks)))
              (if (< (string-length new-chunk) max-chunk-length)
                (append-strings (reverse new-chunks))
                (loop new-chunks))))))))

  (define (append-strings lst)
    (let loop1 ((n 0) (x lst) (y '()))
      (if (pair? x)
        (let ((s (car x)))
          (loop1 (+ n (string-length s)) (cdr x) (cons s y)))
        (let ((result (make-string n #\space)))
          (let loop2 ((k (- n 1)) (y y))
            (if (pair? y)
              (let ((s (car y)))
                (let loop3 ((i k) (j (- (string-length s) 1)))
                  (if (not (< j 0))
                    (begin
                      (string-set! result i (string-ref s j))
                      (loop3 (- i 1) (- j 1)))
                    (loop2 i (cdr y)))))
              result))))))

  (define (parse-number sign start-pos)

    (define (make-integer-literal kind n)
      (let ((val
             (if (eq? sign '-)
               (let () (declare (generic)) (- n))
               n))
            (end-pos
             (scanner-lookahead-pos ss 0)))
        (make-token
         kind
         (ss-filename ss)
         start-pos
         end-pos
         val)))

    (define (make-float-literal n scale power)
      (let ((val
             (if (eq? sign '-)
               (let () (declare (generic)) (- n))
               n))
            (end-pos
             (scanner-lookahead-pos ss 0)))
        (make-token
         FloatLiteral-tok
         (ss-filename ss)
         start-pos
         end-pos
         (let ()
           (declare (generic))
           (* val (expt 10 (- power scale)))))))

    (define (digits radix n scale)
      (let loop ((n n) (scale scale))
        (let ((c1 (scanner-lookahead-char ss 0)))
          (if (and (= c1 underscore-ch)
                   (let* ((c2 (scanner-lookahead-char ss 1))
                          (d2 (class-ref hexadecimal-class c2)))
                     (and d2 (< d2 radix))))
            (begin
              (scanner-advance ss 1)
              (loop n scale))
            (let ((d1 (class-ref hexadecimal-class c1)))
              (if (and d1 (< d1 radix))
                (begin
                  (scanner-advance ss 1)
                  (loop (let () (declare (generic)) (+ (* n radix) d1))
                        (if scale (+ scale 1) (and (> d1 0) 1))))
                (vector n scale)))))))

    (let ((d1 (class-ref decimal-class (scanner-lookahead-char ss 0))))
      (scanner-advance ss 1)
      (let loop ((n d1) (scale (if (> d1 0) 1 #f)))
        (let* ((c2 (scanner-lookahead-char ss 0))
               (d2 (class-ref decimal-class c2)))
          (cond (d2
                 (scanner-advance ss 1)
                 (loop (let () (declare (generic)) (+ (* n 10) d2))
                       (if scale (+ scale 1) (and (> d2 0) 1))))
                ((and (> d1 0)
                      (let () (declare (generic)) (and (>= n 2) (<= n 16)))
                      (= c2 sharp-ch))
                 (let* ((c3 (scanner-lookahead-char ss 1))
                        (d3 (class-ref hexadecimal-class c3)))
                   (if (and d3 (< d3 n))
                     (begin
                       (scanner-advance ss 1)
                       (let ((x (digits n 0 #f)))
                         (make-integer-literal
                          NotUnsignedDecimalIntegerLiteral-tok
                          (vector-ref x 0))))
                     (make-integer-literal
                      NotUnsignedDecimalIntegerLiteral-tok
                      n))))
                (else
                 (let* ((integer-part
                         (digits 10 n scale))
                        (c4
                         (scanner-lookahead-char ss 0)))
                   (if (and (= c4 period-ch)
                            (class-ref decimal-class
                                       (scanner-lookahead-char ss 1)))
                     (begin
                       (scanner-advance ss 1)
                       (let ((int-and-fract-part
                              (digits 10
                                      (vector-ref integer-part 0)
                                      0)))
                         (let ((c5 (scanner-lookahead-char ss 0)))
                           (if (or (= c5 lower-e-ch)
                                   (= c5 upper-e-ch))
                             (let ((c6 (scanner-lookahead-char ss 1)))
                               (cond ((class-ref decimal-class c6)
                                      (scanner-advance ss 1)
                                      (let* ((exponent-part
                                              (digits 10 0 #f))
                                             (power
                                              (vector-ref exponent-part 0)))
                                        (make-float-literal
                                         (vector-ref int-and-fract-part 0)
                                         (vector-ref int-and-fract-part 1)
                                         power)))
                                     ((and (or (= c6 plus-ch)
                                               (= c6 minus-ch))
                                           (class-ref
                                            decimal-class
                                            (scanner-lookahead-char ss 2)))
                                      (scanner-advance ss 2)
                                      (let* ((exponent-part
                                              (digits 10 0 #f))
                                             (power
                                              (vector-ref exponent-part 0)))
                                        (make-float-literal
                                         (vector-ref int-and-fract-part 0)
                                         (vector-ref int-and-fract-part 1)
                                         (if (= c6 plus-ch)
                                           power
                                           (let ()
                                             (declare (generic))
                                             (- power))))))
                                     (else
                                      (make-float-literal
                                       (vector-ref int-and-fract-part 0)
                                       (vector-ref int-and-fract-part 1)
                                       0))))
                             (make-float-literal
                              (vector-ref int-and-fract-part 0)
                              (vector-ref int-and-fract-part 1)
                              0)))))
                     (make-integer-literal
                      (if sign
                        NotUnsignedDecimalIntegerLiteral-tok
                        UnsignedDecimalLiteral-tok)
                      (vector-ref integer-part 0))))))))))

  (define (make-simple-token kind nb-chars)
    (let* ((start-pos (scanner-lookahead-pos ss 0))
           (end-pos (scanner-lookahead-pos ss nb-chars)))
      (scanner-advance ss nb-chars)
      (make-token
       kind
       (ss-filename ss)
       start-pos
       end-pos
       #f)))

  (define (next)
    (let ((c (scanner-lookahead-char ss 0)))
      (if (class-ref space-class c)
        (if (= c eof-ch)
          (make-simple-token *EOI*-tok 0)
          (begin
            (scanner-advance ss 1)
            (next)))
        (let ((x (class-ref ident-class c)))
          (cond ((and x (or (eq? x 'uppercase) (eq? x 'lowercase)))
                 (parse-identifier))
                ((= c comma-ch)
                 (make-simple-token COMMA-tok 1))
                ((= c paren-open-ch)
                 (make-simple-token PAREN-OPEN-tok 1))
                ((= c paren-close-ch)
                 (make-simple-token PAREN-CLOSE-tok 1))
                ((= c brace-open-ch)
                 (make-simple-token BRACE-OPEN-tok 1))
                ((= c brace-close-ch)
                 (make-simple-token BRACE-CLOSE-tok 1))
                ((= c brack-open-ch)
                 (make-simple-token BRACK-OPEN-tok 1))
                ((= c brack-close-ch)
                 (make-simple-token BRACK-CLOSE-tok 1))
                ((= c minus-ch)
                 (let ((x (scanner-lookahead-char ss 1)))
                   (cond ((= x gt-ch)
                          (make-simple-token MINUS-GT-tok 2))
                         ((= x minus-ch)
                          (make-simple-token MINUS-MINUS-tok 2))

; This code allows tokens that are numbers to start with a "-" sign.
;
;                         ((class-ref decimal-class x)
;                          (let ((start-pos (scanner-lookahead-pos ss 0)))
;                            (scanner-advance ss 1)
;                            (parse-number '- start-pos)))

                         (else
                          (make-simple-token MINUS-tok 1)))))
                ((= c semicolon-ch)
                 (make-simple-token SEMICOLON-tok 1))
                ((= c percent-ch)
                 (skip-comment)
                 (next))
                ((class-ref decimal-class c)
                 (let ((start-pos (scanner-lookahead-pos ss 0)))
                   (parse-number #f start-pos)))
                ((= c bar-ch)
                 (let ((x (scanner-lookahead-char ss 1)))
                   (if (= x c)
                     (make-simple-token BAR-BAR-tok 2)
                     (make-simple-token BAR-tok 1))))
                ((= c equal-ch)
                 (let ((x (scanner-lookahead-char ss 1)))
                   (cond ((= x equal-ch)
                          (make-simple-token EQUAL-EQUAL-tok 2))
                         ((= x lt-ch)
                          (make-simple-token EQUAL-LT-tok 2))
                         ((and (= x colon-ch)
                               (= (scanner-lookahead-char ss 2) equal-ch))
                          (make-simple-token EQUAL-COLON-EQUAL-tok 3))
                         ((and (= x slash-ch)
                               (= (scanner-lookahead-char ss 2) equal-ch))
                          (make-simple-token EQUAL-SLASH-EQUAL-tok 3))
                         (else
                          (make-simple-token EQUAL-tok 1)))))
                ((= c period-ch)
                 (let ((x (scanner-lookahead-char ss 1)))
                   (if (or (class-ref space-class x)
                           (= x percent-ch))
                     (make-simple-token FullStop-tok 1)
                     (make-simple-token PERIOD-tok 1))))
                ((= c sharp-ch)
                 (make-simple-token SHARP-tok 1))
                ((= c plus-ch)
                 (let ((x (scanner-lookahead-char ss 1)))
                   (cond ((= x plus-ch)
                          (make-simple-token PLUS-PLUS-tok 2))

; This code allows tokens that are numbers to start with a "+" sign.
;
;                         ((class-ref decimal-class x)
;                          (let ((start-pos (scanner-lookahead-pos ss 0)))
;                            (scanner-advance ss 1)
;                            (parse-number '+ start-pos)))

                         (else
                          (make-simple-token PLUS-tok 1)))))
                ((= c slash-ch)
                 (let ((x (scanner-lookahead-char ss 1)))
                   (cond ((= x slash-ch)
                          (make-simple-token SLASH-SLASH-tok 2))
                         ((= x equal-ch)
                          (make-simple-token SLASH-EQUAL-tok 2))
                         (else
                          (make-simple-token SLASH-tok 1)))))
                ((= c colon-ch)
                 (make-simple-token COLON-tok 1))
                ((= c question-ch)
                 (make-simple-token QUESTION-tok 1))
                ((= c doublequote-ch)
                 (let ((start-pos (scanner-lookahead-pos ss 0)))
                   (let ((str (parse-string)))
                     (let ((end-pos (scanner-lookahead-pos ss 0)))
                       (make-token
                        OneStringLiteral-tok
                        (ss-filename ss)
                        start-pos
                        end-pos
                        str)))))

; This code treats a succession of strings (such as "hello" " world") as
; a single string.
;
;                 (let ((start-pos (scanner-lookahead-pos ss 0)))
;                   (let loop1 ((strings (list (parse-string))))
;                     (let ((end-pos (scanner-lookahead-pos ss 0)))
;                       (let loop2 ()
;                         (let ((c (scanner-lookahead-char ss 0)))
;                           (cond ((and (not (= c eof-ch))
;                                       (class-ref space-class c))
;                                  (scanner-advance ss 1)
;                                  (loop2))
;                                 ((= c percent-ch)
;                                  (skip-comment)
;                                  (loop2))
;                                 ((= c doublequote-ch)
;                                  (loop1 (cons (parse-string) strings)))
;                                 (else
;                                  (make-token
;                                   OneStringLiteral-tok
;                                   (ss-filename ss)
;                                   start-pos
;                                   end-pos
;                                   (append-strings (reverse strings))))))))))

                ((= c quote-ch)
                 (let ((start-pos (scanner-lookahead-pos ss 0)))
                   (let ((str (parse-string)))
                     (let ((end-pos (scanner-lookahead-pos ss 0)))
                       (make-token
                        QuotedAtomLiteral-tok
                        (ss-filename ss)
                        start-pos
                        end-pos
                        str)))))
                ((= c gt-ch)
                 (let ((x (scanner-lookahead-char ss 1)))
                   (cond ((= x equal-ch)
                          (make-simple-token GT-EQUAL-tok 2))
                         (else
                          (make-simple-token GT-tok 1)))))
                ((= c lt-ch)
                 (let ((x (scanner-lookahead-char ss 1)))
                   (cond ((= x minus-ch)
                          (make-simple-token LT-MINUS-tok 2))
                         (else
                          (make-simple-token LT-tok 1)))))
                ((= c dollar-ch)
                 (let ((start-pos (scanner-lookahead-pos ss 0)))
                   (let ((char (parse-char)))
                     (let ((end-pos (scanner-lookahead-pos ss 0)))
                       (make-token
                        CharLiteral-tok
                        (ss-filename ss)
                        start-pos
                        end-pos
                        char)))))
                ((= c star-ch)
                 (make-simple-token STAR-tok 1))
                ((= c bang-ch)
                 (make-simple-token BANG-tok 1))
                (else
                 (scanner-error-invalid-character
                  ss
                  (scanner-lookahead-pos ss 0)
                  (scanner-lookahead-pos ss 1))
                 (scanner-advance ss 1)
                 (next)))))))

  (next))

; Characters and character classes

(define eof-ch          -1)
(define eol-ch          10)
(define lf-ch           10)
(define cr-ch           13)
(define space-ch        32)
(define bang-ch         33)
(define doublequote-ch  34)
(define sharp-ch        35)
(define dollar-ch       36)
(define percent-ch      37)
(define quote-ch        39)
(define paren-open-ch   40)
(define paren-close-ch  41)
(define star-ch         42)
(define plus-ch         43)
(define comma-ch        44)
(define minus-ch        45)
(define period-ch       46)
(define slash-ch        47)
(define colon-ch        58)
(define semicolon-ch    59)
(define lt-ch           60)
(define equal-ch        61)
(define gt-ch           62)
(define question-ch     63)
(define upper-e-ch      69)
(define brack-open-ch   91)
(define backslash-ch    92)
(define brack-close-ch  93)
(define caret-ch        94)
(define underscore-ch   95)
(define lower-b-ch      98)
(define lower-d-ch     100)
(define lower-e-ch     101)
(define lower-f-ch     102)
(define lower-n-ch     110)
(define lower-r-ch     114)
(define lower-s-ch     115)
(define lower-t-ch     116)
(define lower-v-ch     118)
(define brace-open-ch  123)
(define bar-ch         124)
(define brace-close-ch 125)

(define (make-class str . vals)
  (let ((v (make-vector 257 #f))
        (chars (string->list str)))
    (for-each (lambda (char val) (class-set! v (char->unicode char) val))
              chars
              (if (null? vals) chars (car vals)))
    v))

(define (class-ref class c)
  (if (< c 256)
    (vector-ref class (+ c 1))
    #f))

(define (class-set! class c val)
  (if (< c 256)
    (vector-set! class (+ c 1) val)
    #f))

(define space-class
  (let ((class (make-class "")))
    (class-set! class eof-ch #t)
    (let loop ((i space-ch))
      (if (< i 0)
        class
        (begin
          (class-set! class i #t)
          (loop (- i 1)))))))

(define eol-class
  (let ((class (make-class "")))
    (class-set! class eol-ch #t)
    (class-set! class eof-ch #t)
    class))

(define ident-class
  (let ((class (make-class "")))
    (class-set! class #x40 'at)
    (class-set! class #x5f 'uppercase)
    (let loop1 ((i #x41))
      (if (<= i #x5a)
        (begin
          (class-set! class i 'uppercase)
          (loop1 (+ i 1)))))
    (let loop2 ((i #xc0))
      (if (<= i #xd6)
        (begin
          (class-set! class i 'uppercase)
          (loop2 (+ i 1)))))
    (let loop3 ((i #xd8))
      (if (<= i #xde)
        (begin
          (class-set! class i 'uppercase)
          (loop3 (+ i 1)))))
    (let loop4 ((i #x61))
      (if (<= i #x7a)
        (begin
          (class-set! class i 'lowercase)
          (loop4 (+ i 1)))))
    (let loop5 ((i #xdf))
      (if (<= i #xf6)
        (begin
          (class-set! class i 'lowercase)
          (loop5 (+ i 1)))))
    (let loop6 ((i #xf8))
      (if (<= i #xff)
        (begin
          (class-set! class i 'lowercase)
          (loop6 (+ i 1)))))
    (let loop7 ((i #x30))
      (if (<= i #x39)
        (begin
          (class-set! class i 'digit)
          (loop7 (+ i 1)))))
    class))

(define octal-class
  (make-class "01234567"
              '(0 1 2 3 4 5 6 7)))

(define decimal-class
  (make-class "0123456789"
              '(0 1 2 3 4 5 6 7 8 9)))

(define hexadecimal-class
  (make-class "0123456789abcdefABCDEF"
              '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 10 11 12 13 14 15)))

; Note: all token tags (e.g. *EOI*-tok) are defined in the file "erlang.scm".

(define (scanner-hash-string str)
  (let ((len (string-length str)))
    (let loop ((h 0) (i (- len 1)))
      (if (< i 0)
        h
        (let ((n (modulo (+ (* h scanner-hash-mult)
                            (char->unicode (string-ref str i)))
                         scanner-hash-mod)))
          (loop n
                (- i 1)))))))

; These constants give perfect hashing of the Erlang
; keywords and operators.

(define scanner-hash-mod 101)
(define scanner-hash-mult 17)
(define scanner-keyword-perfect-hashtable
  (vector (cons "receive" RECEIVE-tok)
          #f
          (cons "rem" REM-tok)
          #f
          #f
          #f
          #f
          (cons "xor" XOR-tok)
          (cons "record" RECORD-tok)
          #f
          #f
          #f
          #f
          #f
          #f
          (cons "bxor" BXOR-tok)
          #f
          #f
          #f
          (cons "export" EXPORT-tok)
          #f
          (cons "if" IF-tok)
          #f
          #f
          #f
          #f
          #f
          (cons "of" OF-tok)
          (cons "after" AFTER-tok)
          (cons "or" OR-tok)
          #f
          (cons "div" DIV-tok)
          #f
          (cons "compile" COMPILE-tok)
          #f
          #f
          (cons "bsl" BSL-tok)
          (cons "case" CASE-tok)
          (cons "import" IMPORT-tok)
          #f
          #f
          (cons "band" BAND-tok)
          #f
          #f
          #f
          #f
          (cons "fun" FUN-tok)
          #f
          (cons "when" WHEN-tok)
          #f
          #f
          #f
          #f
          (cons "bsr" BSR-tok)
          #f
          #f
          #f
          (cons "try" TRY-tok)
          #f
          #f
          (cons "all_true" ALL_TRUE-tok)
          #f
          (cons "and" AND-tok)
          #f
          #f
          #f
          (cons "end" END-tok)
          #f
          #f
          #f
          (cons "not" NOT-tok)
          #f
          #f
          #f
          (cons "begin" BEGIN-tok)
          #f
          (cons "bnot" BNOT-tok)
          (cons "catch" CATCH-tok)
          (cons "cond" COND-tok)
          (cons "module" MODULE-tok)
          #f
          #f
          #f
          #f
          #f
          #f
          (cons "bor" BOR-tok)
          #f
          #f
          #f
          #f
          (cons "mod" MOD-tok)
          #f
          #f
          #f
          #f
          (cons "some_true" SOME_TRUE-tok)
          #f
          #f
          #f
          (cons "let" LET-tok)))

;------------------------------------------------------------------------------

'(begin ; unquote this code to generate a perfect hash table

(define words '(
"after"
"all_true"
"and"
"band"
"begin"
"bnot"
"bor"
"bsl"
"bsr"
"bxor"
"case"
"catch"
"compile"
"cond"
;"define"
"div"
;"else"
"end"
;"endif"
"export"
;"file"
"fun"
"if"
;"ifdef"
;"ifndef"
"import"
;"include"
;"include_lib"
"let"
"mod"
"module"
"not"
"of"
"or"
;"query"
"receive"
"record"
"rem"
"some_true"
"try"
;"true"
;"undef"
"when"
"xor"
))

(define (sort l)

  (define (mergesort l)

    (define (merge l1 l2)
      (cond ((null? l1) l2)
            ((null? l2) l1)
            (else
             (let ((e1 (car l1)) (e2 (car l2)))
               (if (< e1 e2)
                 (cons e1 (merge (cdr l1) l2))
                 (cons e2 (merge l1 (cdr l2))))))))

    (define (split l)
      (if (or (null? l) (null? (cdr l)))
        l
        (cons (car l) (split (cddr l)))))

    (if (or (null? l) (null? (cdr l)))
      l
      (let* ((l1 (mergesort (split l)))
             (l2 (mergesort (split (cdr l)))))
        (merge l1 l2))))

  (mergesort l))

(define (duplicates? lst)
  (let loop ((l (sort lst)))
    (if (or (null? l) (null? (cdr l)))
      #f
      (if (= (car l) (cadr l))
        #t
        (loop (cdr l))))))

(define (found)

  (define v (make-vector scanner-hash-mod #f))

  (for-each
   (lambda (word)
     (vector-set! v (scanner-hash-string word)
       (list 'cons
             word
             (string->symbol
              (string-append
               (list->string (map char-upcase (string->list word)))
               "-tok")))))
   words)
  (pp `(define scanner-hash-mod ,scanner-hash-mod))
  (pp `(define scanner-hash-mult ,scanner-hash-mult))
  (pp (list 'define
            'scanner-keyword-perfect-hashtable
            (cons 'vector (vector->list v)))))

(call-with-current-continuation
 (lambda (abort)
   (let loop1 ((i (length words)))
     (if (<= i 1024)
         (begin
           (set! scanner-hash-mod i)
           (let loop2 ((j 1))
             (if (<= j 16384)
                 (begin
                   (set! scanner-hash-mult j)
                   (if (duplicates? (map scanner-hash-string words))
                       (loop2 (+ j 1))
                       (begin
                         (found)
                         (abort #f))))))
           (loop1 (+ i 1)))))))
)

;------------------------------------------------------------------------------
