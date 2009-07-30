#!r6rs
;; Copyright (c) 2009 Derick Eddington.  All rights reserved.  Licensed under an
;; MIT-style license.  My license is in the file named LICENSE from the original
;; collection this file is distributed with.  If this file is redistributed with
;; some other collection, my license must also be included.

(import
  (rnrs)
  (xitomatl records)
  (srfi :78 lightweight-testing))

(define-record-type A (fields a (mutable b) c))
(define-record-type B (parent A) (fields (mutable d) e))
(define-record-type C (parent B))
(define-record-type D (parent C) (fields f (mutable g) (mutable h)))
(define a (make-A 1 2 3))
(define b (make-B 1 2 3 4 5))
(define c (make-C 1 2 3 4 5))
(define d (make-D 1 2 3 4 5 6 7 8))
(check (record-type-fields (record-rtd a)) => '(a b c))
(check (record-type-fields (record-rtd b)) => '(a b c d e))
(check (record-type-fields (record-rtd c)) => '(a b c d e))
(check (record-type-fields (record-rtd d)) => '(a b c d e f g h))
(define A-accessors (record-type-accessors (record-rtd a)))
(define B-accessors (record-type-accessors (record-rtd b)))
(define C-accessors (record-type-accessors (record-rtd c)))
(define D-accessors (record-type-accessors (record-rtd d)))
(check (length A-accessors) => 3)
(check (for-all procedure? A-accessors) => #T)
(check (length B-accessors) => 5)
(check (for-all procedure? B-accessors) => #T)
(check (length C-accessors) => 5)
(check (for-all procedure? C-accessors) => #T)
(check (length D-accessors) => 8)
(check (for-all procedure? D-accessors) => #T)
(check (map (lambda (get) (get a)) A-accessors) => '(1 2 3))
(check (map (lambda (get) (get b)) B-accessors) => '(1 2 3 4 5))
(check (map (lambda (get) (get c)) C-accessors) => '(1 2 3 4 5))
(check (map (lambda (get) (get d)) D-accessors) => '(1 2 3 4 5 6 7 8))
(define A-mutators (record-type-mutators (record-rtd a)))
(define B-mutators (record-type-mutators (record-rtd b)))
(define C-mutators (record-type-mutators (record-rtd c)))
(define D-mutators (record-type-mutators (record-rtd d)))
(check (length A-mutators) => 3)
(check (length B-mutators) => 5)
(check (length C-mutators) => 5)
(check (length D-mutators) => 8)
(let ((what (lambda (m) 
              (cond ((procedure? m) 'p) 
                    ((not m) #F)
                    (else 'bad)))))
  (check (map what A-mutators) => '(#F p #F))
  (check (map what B-mutators) => '(#F p #F p #F))
  (check (map what C-mutators) => '(#F p #F p #F))
  (check (map what D-mutators) => '(#F p #F p #F #F p p)))
(for-each (lambda (setter!) (setter! a 'new)) (filter values A-mutators))
(check (map (lambda (get) (get a)) A-accessors) => '(1 new 3))
(for-each (lambda (setter!) (setter! b 'new)) (filter values B-mutators))
(check (map (lambda (get) (get b)) B-accessors) => '(1 new 3 new 5))
(for-each (lambda (setter!) (setter! c 'new)) (filter values C-mutators))
(check (map (lambda (get) (get c)) C-accessors) => '(1 new 3 new 5))
(for-each (lambda (setter!) (setter! d 'new)) (filter values D-mutators))
(check (map (lambda (get) (get d)) D-accessors) => '(1 new 3 new 5 6 new new))


(check-report)
