#!/usr/bin/env scheme-script
#!r6rs
(import
  (rnrs)
  (rnrs eval)
  (xitomatl generics)
  (xitomatl srfi lightweight-testing))

(define-syntax check-AV-who-msg
  (syntax-rules ()
    [(_ who msg expr)
     (check (guard (ex [else (and (assertion-violation? ex)
                                  (message-condition? ex)
                                  (string=? (condition-message ex) msg)
                                  (who-condition? ex)
                                  (symbol=? 'who (condition-who ex)))])
              expr
              'unexpected-return)
            => #t)]))

(define-syntax check-no-spec
  (syntax-rules ()
    [(_ who expr)
     (check-AV-who-msg who "no specialization" expr)]))

(define-syntax check-SV
  (syntax-rules ()
    [(_ expr)
     (check (guard (ex [else (syntax-violation? ex)])
              (eval 'expr (environment '(rnrs) '(xitomatl generics)))
              'unexpected-return)
            => #t)]))

(define-generic g0)
(check (procedure? g0) => #t)
(check-no-spec g0 (g0))
(specialize g0 '() (lambda () 1))
(check (g0) => 1)
(check-no-spec g0 (g0 'a))
(specialize g0 (list symbol?) symbol->string)
(check (g0 'a) => "a")
(check-no-spec g0 (g0 'a 'b))
(specialize g0 (list symbol? symbol?) symbol=?)
(check (g0 'a 'b) => #f)
(check (g0 'a 'a) => #t)
(check-no-spec g0 (g0 'a 3))
(specialize g0 (list (lambda (x) (or (symbol? x) (string? x))) number?) 
  (lambda (s n) (make-vector n s)))
(check (g0 'a 3) => '#(a a a))
(check (g0 "a" 4) => '#("a" "a" "a" "a"))
(check-no-spec g0 (g0 "a" "3"))
(check-no-spec g0 (g0 #\c))
(check-no-spec g0 (g0 #\a #\b #\c))
;; required arguments and rest arguments list
(specialize g0 (cons* char? (lambda r (for-all char-alphabetic? r)))
  (lambda (c . r) (apply string c r)))
(check (g0 #\1) => "1")
(check (g0 #\1 #\b #\c #\d) => "1bcd")
(check-no-spec g0 (g0 #\1 #\2))
(check-no-spec g0 (g0 #\1 #\2 #\3 #\4))
;; no required, rest arguments list
(specialize g0 (lambda args #t) (lambda args (reverse args)))
;; specializations have precedence according to their order of being added
(check (g0) => 1)
(check (g0 1 2 3) => '(3 2 1))
(check (g0 's) => "s")
(check (g0 's 's) => #t)
(check (g0 "x" 'y) => '(y "x"))
(check (g0 "x" 2) => '#("x" "x"))
(check (g0 #\1 #\2 #\3 #\4) => '(#\4 #\3 #\2 #\1))
(check (g0 #\1) => "1")
(check (g0 #\1 #\z) => "1z")
(check (g0 '(x)) => '((x)))
(check (g0 's 3 's #\c) => '(#\c s 3 s))
;; misc
(check-AV-who-msg make-generic "not a symbol" (make-generic "oops"))
(define g1 (make-generic))
(check-AV-who-msg specialize "not a generic" 
  (specialize 'oops '() values))
(check-AV-who-msg specialize "not a valid specialization predicates list" 
  (specialize g1 'oops values))
(check-AV-who-msg specialize "not a valid specialization predicates list" 
  (specialize g1 (list 'oops) values))
(check-AV-who-msg specialize "not a valid specialization predicates list" 
  (specialize g1 (cons* char? number? 'oops) values))
(check-AV-who-msg specialize "not a procedure"
  (specialize g1 (list number?) 'oops))
;; define-generic specializations
(define-generic g2
  [() 1]
  [([x symbol?])
   (symbol->string x)]
  [([x symbol?] [y symbol?])
   (symbol=? x y)]
  [([x (lambda (x) (or (symbol? x) (string? x)))] [y number?])
   (make-vector y x)]
  [([x char?] . #(y (lambda r (for-all char-alphabetic? r))))
   (apply string x y)]
  [#(x (lambda a #t))
   (reverse x)])
(check (g2) => 1)
(check (g2 'a) => "a")
(check (g2 'a 'b) => #f)
(check (g2 'a 'a) => #t)
(check (g2 'a 3) => '#(a a a))
(check (g2 "a" 4) => '#("a" "a" "a" "a"))
(check (g2 #\1) => "1")
(check (g2 #\1 #\b #\c #\d) => "1bcd")
(check (g2 1 2 3) => '(3 2 1))
(check (g2 's) => "s")
(check (g2 's 's) => #t)
(check (g2 "x" 'y) => '(y "x"))
(check (g2 "x" 2) => '#("x" "x"))
(check (g2 #\1 #\2 #\3 #\4) => '(#\4 #\3 #\2 #\1))
(check (g2 #\1) => "1")
(check (g2 #\1 #\z) => "1z")
(check (g2 '(x)) => '((x)))
(check (g2 's 3 's #\c) => '(#\c s 3 s))
(check-AV-who-msg specialize "not a valid specialization predicates list" 
  (let ()
    (define-generic g1 
      [([a 'oops]) values])
    g1))
(check-AV-who-msg specialize "not a valid specialization predicates list" 
  (let ()
    (define-generic g1 
      [#(a 'oops) values])
    g1))
(check-AV-who-msg specialize "not a valid specialization predicates list" 
  (let ()
    (define-generic g1 
      [([z char?] . #(a 'oops)) values])
    g1))
(check-SV (let ()
            (define-generic g1 
              [(oops) values])
            g1))
(check-SV (let ()
            (define-generic g1 
              [(oops oops2) values])
            g1))
(check-SV (let ()
            (define-generic g1 
              [oops values])
            g1))
(check-SV (let ()
            (define-generic g1 
              [#(oops) values])
            g1))
(check-SV (let ()
            (define-generic g1 
              [([1 char?]) values])
            g1))
(check-SV (let ()
            (define-generic g1 
              [([a null?] . #(1 char?)) values])
            g1))
(check-SV (let ()
            (define-generic g1 
              [#(1 char?) values])
            g1))
(check-SV (let ()
            (define-generic g1 
              [(#(x char?)) values])
            g1))


(check-report)