; file: "utils.scm"

; Copyright (C) 1998-1999 Universite de Montreal, All Rights Reserved.

; This file contains non-portable code and miscellaneous definitions.

;------------------------------------------------------------------------------

; Portability section.

; This section contains non-portable code.  It is currently setup for
; Gambit-C 4.6.5.

"<-- put a semicolon at the start of this line to get Standard Scheme version.

; Standard Scheme version.

(define (declare . args) #f)
(define (standard-bindings) #f)
(define (block) #f)
(define (fixnum) #f)
(declare (generic) #f)
(define safe #f)
(define (inlining-limit n) #f)

(define include load)

(define (open-binary-input-file file)
  (open-input-file file))

"; ends comment of standard Scheme version or starts comment of Gambit version.

; Gambit Scheme version.

(declare
 (standard-bindings)
; (block)
; (fixnum)
; (not safe)
)

(define (open-binary-input-file file)
  (open-input-file file))

(define (filename-extension path)
  (path-extension path))

(define (filename-strip-extension path)
  (path-strip-extension path))

(define (filename-directory path)
  (path-directory path))

(define (filename-strip-directory path)
  (path-strip-directory path))

(define (filename-build dir filename)
  (path-expand filename dir))

(define (filename->abs-filename filename)
  (path-expand filename))

(define (filename-absolute? filename)
  (string=? filename (path-expand filename)))

(define (filename-exists? filename)
  (file-exists? filename))

(define (get-current-working-directory)
  (current-directory))

(define (get-command-line-arguments)
  (command-line))

(define (get-install-dir)

  (define-macro (install-dir-macro)
    (let ((inst-dir (##global-var-ref (##make-global-var 'install-dir))))
      (if (##unbound? inst-dir) "." inst-dir)))

  (install-dir-macro))

(define (get-utc-time)
  (inexact->exact (truncate (time->seconds (current-time)))))

(define (exit-with-value x)
  (exit x))

;"; ends comment of Gambit version

; This works on most implementations of Scheme:

(define (char->unicode c) (char->integer c))
(define (unicode->char n) (integer->char n))

;------------------------------------------------------------------------------

; Portable functions.


(define (hash-string str)
  (let ((len (string-length str)))
    (let loop ((h 0) (i 0))
      (if (< i len)
        (let ((n (modulo (+ (* h 256)
                            (char->unicode (string-ref str i)))
                         40003)))
          (loop n
                (+ i 1)))
        h))))

(define (make-dict default)
  (let ((v (make-vector 404 '())))
    (vector-set! v 0 default)
    v))

(define (dict-lookup dict key)
  (let* ((h (+ 1 (modulo (hash-string key) 403)))
         (x (vector-ref dict h))
         (y (assoc key x)))
    (if y
      (cdr y)
      (vector-ref dict 0))))

(define (dict-assign dict key val)
  (let* ((h (+ 1 (modulo (hash-string key) 403)))
         (x (vector-ref dict h))
         (y (assoc key x)))
    (if y
      (set-cdr! y val)
      (vector-set! dict h (cons (cons key val) x)))))


;; Added by Patrick:
(define (filter pred lst)
  (if (null? lst)
      lst
      (let ((h (car lst)) (rest (filter pred (cdr lst))))
        (if (pred h) (cons h rest) rest))))

(define (mapfilter pred lst)
  (if (null? lst)
      lst
      (let ((h (pred (car lst))) (rest (mapfilter pred (cdr lst))))
        (if h (cons h rest) rest))))

(define (fold f lst)
  (if (null? lst)
      '()
      (let ((hd (car lst)) (tl (cdr lst)))
        (if (null? tl)
            hd
            (fold f (cons (f hd (car tl)) (cdr tl)))))))

(define (union . lst)
  (fold (lambda (l1 l2) (append l1 (difference l2 l1))) lst))
(define (intersection . lst)
  (fold (lambda (l1 l2) (filter (lambda (h) (member h l2)) l1)) lst))
(define (difference l1 l2)
  (filter (lambda (h) (not (member h l2))) l1))
(define (remove-duplicates lst)
  (reverse
   (fold (lambda (lst1 elt) 
           (if (member elt lst1) lst1 (cons elt lst1))) 
         (cons '() lst))))
(define (get-duplicates lst)
  (remove-duplicates
   (if (null? lst)
       '()
       (let ((hd (car lst))
             (tl (cdr lst)))
         (if (member hd tl)
             (cons hd (get-duplicates tl))
             (get-duplicates tl))))))

(define (pp-val x) (pp x) x)

(define symbol-append
  (lambda symbols
    (string->symbol (apply string-append (map symbol->string symbols)))))

(define (copy-vector vect)
  (let* ((n (vector-length vect))
         (result (make-vector n #f)))
    (let loop ((i (- n 1)))
      (if (< i 0)
          result
          (begin
            (vector-set! result i (vector-ref vect i))
            (loop (- i 1)))))))

;------------------------------------------------------------------------------

(define (compile-scheme-file filename)
  (= (shell-command (string-append "gsc \"" filename "\"")) 0))


; Gregorian calendar calculations.
;
; This code is inspired from the book:
;
;   "Calendrical Calculations" by Nachum Dershowitz and Edward
;   M. Reingold (Cambridge University Press, 1997, ISBN 0-521-56413-1)
;   http://emr.cs.uiuc.edu/home/reingold/calendar-book/
;
;   Also see http://tycho.usno.navy.mil/leapsec.html about leap seconds.

(define (floor/ num den) ; assumes: den > 0
  (if (negative? num)
      (quotient (- num (- den 1)) den)
      (quotient num den)))

(define (leap-year? year)
  (and (= (modulo year 4) 0)
       (or (not (= (modulo year 100) 0))
           (= (modulo year 400) 0))))

(define (last-day-of-month year month)
  (if (and (= month 2) (leap-year? year))
      29
      (vector-ref '#(31 28 31 30 31 30 31 31 30 31 30 31)
                  (- month 1))))

(define (days-to-first-day-of-month year month)
  (let ((n (vector-ref '#(0 31 59 90 120 151 181 212 243 273 304 334)
                       (- month 1))))
    (if (and (> month 2) (leap-year? year))
        (+ n 1)
        n)))

; Compute the absolute date equivalent to the Gregorian date.  The
; absolute date is relative to january 1st of year 1 (i.e. absolute
; day 1 = january 1st of year 1).

(define (year-month-day->absolute year month day)
  (let ((year-minus-1 (- year 1)))
    (+ day
       (days-to-first-day-of-month year month)
       (* 365 year-minus-1)
       (floor/ year-minus-1 4)
       (- (floor/ year-minus-1 100))
       (floor/ year-minus-1 400))))

; Compute the Gregorian date equivalent to the absolute date.

(define (absolute->year-month-day abs)
  (let* ((year
          (absolute->year abs))
         (prior-days
          (- abs (year-month-day->absolute year 1 1)))
         (correction
          (if (< abs (year-month-day->absolute year 3 1))
              0
              (if (leap-year? year)
                  1
                  2)))
         (month
          (floor/ (+ (* 12 (+ prior-days correction)) 373)
                  367))
         (day
          (+ (- abs (year-month-day->absolute year month 1)) 1)))
    (vector year month day)))

(define (absolute->year abs)
  (let* ((d0 (- abs 1))
         (n400 (floor/ d0 146097))
         (d1   (modulo d0 146097))
         (n100 (floor/ d1 36524))
         (d2   (modulo d1 36524))
         (n4   (floor/ d2 1461))
         (d3   (modulo d2 1461))
         (n1   (floor/ d3 365))
         (d4   (+ (modulo d3 365) 1))
         (year (+ (* 400 n400) (* 100 n100) (* 4 n4) n1)))
    (if (or (= n100 4) (= n1 4))
        year
        (+ year 1))))

(define (utc-time-in-secs->broken-time time-in-secs)

  (define absolute-01-01-1970 719163) ; absolute date of january 1st 1970

  (let* ((time-in-days
          (floor/ time-in-secs 86400))
         (secs-in-day
          (modulo time-in-secs 86400))
         (ymd
          (absolute->year-month-day (+ absolute-01-01-1970 time-in-days)))
         (year
          (vector-ref ymd 0))
         (month
          (vector-ref ymd 1))
         (day
          (vector-ref ymd 2))
         (hours
          (floor/ secs-in-day 3600))
         (minutes
          (floor/ (modulo secs-in-day 3600) 60))
         (seconds
          (modulo secs-in-day 60)))
    (vector year month day hours minutes seconds)))

;(pp (utc-time-in-secs->broken-time
;     (quotient (real-time) 1000000000)))
