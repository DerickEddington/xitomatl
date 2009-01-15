;;; Copyright (c) 2008 Derick Eddington
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;;
;;; Except as contained in this notice, the name(s) of the above copyright
;;; holders shall not be used in advertising or otherwise to promote the sale,
;;; use or other dealings in this Software without prior written authorization.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

#!r6rs
(import
  (rnrs)
  (xitomatl predicates)
  (srfi :78 lightweight-testing)
  (only (xitomatl exceptions) catch))

(define-syntax check-AV
  (syntax-rules ()
    [(_ expr)
     (check (catch ex ([else (assertion-violation? ex)])
              expr
              'unexpected-return)
            => #T)]))

;; non-negative-integer?
(check (non-negative-integer? "no") => #F)
(check (non-negative-integer? 1/2) => #F)
(check (non-negative-integer? -2) => #F)
(check (non-negative-integer? 0) => #T)
(check (non-negative-integer? 321) => #T)
(check (non-negative-integer? 7.0) => #T)
;; exact-non-negative-integer?
(check (exact-non-negative-integer? #\z) => #F)
(check (exact-non-negative-integer? 0.0) => #F)
(check (exact-non-negative-integer? 45.0) => #F)
(check (exact-non-negative-integer? -34) => #F)
(check (exact-non-negative-integer? 0) => #T)
(check (exact-non-negative-integer? 89348) => #T)
;; positive-integer?
(check (positive-integer? '(nope)) => #F)
(check (positive-integer? 0) => #F)
(check (positive-integer? -34) => #F)
(check (positive-integer? 789/23) => #F)
(check (positive-integer? 85.0) => #T)
(check (positive-integer? 3916237) => #T)
;; exact-positive-integer?
(check (exact-positive-integer? '#()) => #F)
(check (exact-positive-integer? 21.0) => #F)
(check (exact-positive-integer? -9) => #F)
(check (exact-positive-integer? 3.21) => #F)
(check (exact-positive-integer? 1) => #T)
(check (exact-positive-integer? 567/1) => #T)
;; exact-integer?
(check (exact-integer? '()) => #F)
(check (exact-integer? 1.0) => #F)
(check (exact-integer? 3/2) => #F)
(check (exact-integer? 0) => #T)
(check (exact-integer? -76) => #T)
(check (exact-integer? 238) => #T)
;; list-of?
(check ((list-of? string?) '()) => #T)
(check ((list-of? string?) 'foo) => #F)
(check ((list-of? number?) '(-45/6 23.71)) => #T)
(check ((list-of? (lambda (x) #F)) '(hehe)) => #F)
(check ((list-of? char?) '(#\a #\b "c" #\d)) => #F)
;; pairwise?
(define pw< (pairwise? <))
(check (pw<) => #T)
(check (pw< 42) => #T)
(check (pw< -.4e2 -4/5 0 (* 2 (greatest-fixnum))) => #T)
(check (pw< 0 -1 1) => #F)
(check (list-sort pw< '(56 -5/2 0.0 -1234.)) => (list-sort < '(56 -5/2 0.0 -1234.)))
(define pw>= (pairwise? >=))
(check (pw>= 5.0 5 49/10 4.8 1 1 0) => #T)
(check (pw>= 1 2) => #F)
(check (list-sort pw>= '(56 -5/2 -1234.)) => (list-sort >= '(56 -5/2 -1234.)))
(define-record-type thing (fields a))
(define pw-thing=? (pairwise? equal?
                    (lambda (x)
                      (if (thing? x)
                        (thing-a x)
                        (assertion-violation 'pw-thing=? "invalid" x)))))
(check (pw-thing=?) => #T)
(check (pw-thing=? (make-thing 1)) => #T)
(check (pw-thing=?) => #T)
(check (apply pw-thing=? (map make-thing '(o o o))) => #T)
(check (apply pw-thing=? (map make-thing '(o o o z))) => #F)
(check-AV (pw-thing=? 'oops))
(check-AV (pw-thing=? (make-thing 1) 'oops))
(let ((a '()))
  (define pw (pairwise? (lambda (x y) #T)
                        (lambda (x)
                          (set! a (append a (list x)))
                          x)))
  (pw 'a 'b 'c 'd 'e 'f 'g)
  ;; user's proc is called only once per element, in sequence
  (check a => '(a b c d e f g)))
;; symbol<?
(check (symbol<?) => #T)
(check (symbol<? 'foo) => #T)
(check (symbol<? 'blah 'asdf) => #F)
(check (symbol<? 'f 'h) => #T)
(check (symbol<? 'aaa 'aab 'ab 'ba 'br 'ha 'zdsf) => #T)
(check-AV (symbol<? "oops"))
(check-AV (symbol<? 'foo ''oops 'bar))
;; name=?
(check (name=?) => #T)
(check (name=? "foo") => #T)
(check (name=? #'foo 'foo) => #T)
(check (name=? #'foo 'foo "foo" #'foo "foo" 'foo) => #T)
(check (name=? #'foo 'bar) => #F)
(check (name=? "foo" #'foo 'foo 'bar) => #F)
(check-AV (name=? 1))
(check-AV (name=? 'foo ''oops 'bar))
;; non-empty-string?
(check (non-empty-string? 'nope) => #F)
(check (non-empty-string? "") => #F)
(check (non-empty-string? "a") => #T)
(check (non-empty-string? "blah asdf zab") => #T)
;; char-line-ending?
(check (char-line-ending? 'nope) => #F)
(check (char-line-ending? #\a) => #F)
(check (char-line-ending? #\xa) => #T)
(check (char-line-ending? #\xd) => #T)
(check (char-line-ending? #\x85) => #T)
(check (char-line-ending? #\x2028) => #T)
;; library-name?
(check (library-name? 1) => #F)
(check (library-name? '()) => #F)
(check (library-name? '("foo")) => #F)
(check (library-name? '(1)) => #F)
(check (library-name? '((1))) => #F)
(check (library-name? (list 'foo (string->symbol "") 'zab)) => #F)
(check (library-name? '(foo)) => #T)
(check (library-name? '(foo bar)) => #T)
(check (library-name? '(foo bar ())) => #T)
(check (library-name? '(foo bar zab blah asdf hoho)) => #T)
(check (library-name? '(foo bar "zab" blah asdf hoho)) => #F)
(check (library-name? '(foo (1))) => #T)
(check (library-name? '(foo bar)) => #T)
(check (library-name? '(foo bar (1))) => #T)
(check (library-name? '(foo bar (3 4))) => #T)
(check (library-name? '(foo bar zab (0 4 3 2 1 9 8))) => #T)
(check (library-name? '(foo bar (3 oops 4))) => #F)
(check (library-name? '(foo bar (3 4.1))) => #F)
(check (library-name? '(foo bar (3 -4 5))) => #F)
(check (library-name? '(foo bar (3 4) oops)) => #F)
;; library-name-symbol?
(check (library-name-symbol? 'a) => #T)
(check (library-name-symbol? 'abc) => #T)
(check (library-name-symbol? (string->symbol "")) => #F)
(check (library-name-symbol? "str") => #F)
(check (library-name-symbol? 1) => #F)
;; library-version?
(check (library-version? '()) => #T)
(check (library-version? '(1)) => #T)
(check (library-version? '(3 4)) => #T)
(check (library-version? '(0 4 3 2 1 9 8)) => #T)
(check (library-version? '(3 oops 4)) => #F)
(check (library-version? '(3 4.1)) => #F)
(check (library-version? '(3 -4 5)) => #F)
;; library-name<?
(check (library-name<?) => #T)
(check (library-name<? '(foo)) => #T)
(check (library-name<? '(bar) '(foo)) => #T)
(check (library-name<? '(foo) '(bar)) => #F)
(check (library-name<? '(foo) '(foo)) => #F)
(check (library-name<? '(foo) '(fooo)) => #T)
(check (library-name<? '(fooo) '(foo)) => #F)
(check (library-name<? '(foo) '(foo bar)) => #T)
(check (library-name<? '(foo bar) '(foo)) => #F)
(check (library-name<? '(foo bar) '(foo bar)) => #F)
(check (library-name<? '(foo bar) '(foo bar zab)) => #T)
(check (library-name<? '(foo bar zab) '(foo bar)) => #F)
(check (library-name<? '(foo bar zab) '(foo bar zab)) => #F)
(check (library-name<? '(foo bar zab) '(foo bar zaba)) => #T)
(check (library-name<? '(foo bar zaba) '(foo bar zab)) => #F)
(check (library-name<? '(bar foo) '(foo)) => #T)
(check (library-name<? '(foo) '(bar foo)) => #F)
(check (library-name<? '(foo) '(foo ())) => #T)
(check (library-name<? '(foo ()) '(foo)) => #F)
(check (library-name<? '(foo) '(foo (0))) => #T)
(check (library-name<? '(foo (0)) '(foo)) => #F)
(check (library-name<? '(foo (1)) '(foo (2))) => #T)
(check (library-name<? '(foo (2)) '(foo (1))) => #F)
(check (library-name<? '(foo (1)) '(foo (1))) => #F)
(check (library-name<? '(foo (2 3)) '(foo (3))) => #T)
(check (library-name<? '(foo (3)) '(foo (2 3))) => #F)
(check (library-name<? '(foo (1)) '(foo (1 0))) => #T)
(check (library-name<? '(foo (1 0)) '(foo (1))) => #F)
(check (library-name<? '(foo (1 2 3)) '(foo (1 2 3 0))) => #T)
(check (library-name<? '(foo (1 2 3 0)) '(foo (1 2 3))) => #F)
(check (library-name<? '(foo (3)) '(foo bar (2))) => #T)
(check (library-name<? '(foo bar (2)) '(foo (3))) => #F)
(check (library-name<? '(foo bar zab) '(foo bar zab (0 1 3))) => #T)
(check (library-name<? '(foo bar zab (0 1 3)) '(foo bar zab)) => #F)
(check (library-name<? '(foo bar zab (0 1 2)) '(foo bar zab (0 1 3))) => #T)
(check (library-name<? '(foo bar zab (0 1 3)) '(foo bar zab (0 1 2))) => #F)
(check (library-name<? '(foo bar zab (0 1 2)) '(foo bar zab (0 1 2))) => #F)
(check (library-name<? '(foo bar zab (0 1)) '(foo bar zab (0 1 2))) => #T)
(check (library-name<? '(foo bar zab (0 1 2)) '(foo bar zab (0 1))) => #F)
(check (library-name<? '(bar zab foo (1 2 3)) '(foo bar (0 1))) => #T)
(check (library-name<? '(foo bar (0 1)) '(bar zab foo (1 2 3))) => #F)
(check (library-name<? '(bar foo) '(foo (1 9)) '(foo (2)) '(fooo)) => #T)
(check (library-name<? '(bar foo) '(foo (1 9)) '(foo (2)) '(foo) '(zab)) => #F)
(check (list-sort library-name<?
                  '((foo bar) (aaaa aaa aa a) (z (13)) (z (1 2)) (bar foo)
                    (foo) (bar blah zab asdf (0)) (foo (0)) (foo bar (4 1))))
       => '((aaaa aaa aa a) (bar blah zab asdf (0)) (bar foo) (foo)
            (foo (0)) (foo bar) (foo bar (4 1)) (z (1 2)) (z (13))))
(check (library-name<? '(bar rab arb) '(foo (1 2 3)) '(foo (2)) '(foo arb)) => #T)
(check (library-name<? '(foo (2)) '(rab) '(foo arb)) => #F)
(check-AV (library-name<? '(foo bar) 'oops))
(check-AV (library-name<? '(foo "oops") '(zab)))
;; library-version<?
(check (library-version<? '() '(0)) => #T)
(check (library-version<? '(0) '()) => #F)
(check (library-version<? '() '()) => #F)
(check (library-version<? '(1 98 45) '(2 97)) => #T)
(check (library-version<? '(2 97) '(1 98 45)) => #F)
(check (library-version<? '(1 2 3) '(1 2 3 0)) => #T)
(check (library-version<? '(1 2 3 0) '(1 2 3)) => #F)
(check (library-version<? '(1 2 3 4) '(1 2 4 3)) => #T)
(check (library-version<? '(1 2 4 3) '(1 2 3 4)) => #F)
(check (library-version<? '() '(1 2 3) '(9 9) '(10)) => #T)
(check (library-version<? '() '(10) '(9 9)) => #F)
(check-AV (library-version<? '(1 2 3) 'oops))
(check-AV (library-version<? '(1 2 1.0) '(0 5)))


(check-report)