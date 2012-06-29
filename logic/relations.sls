#!r6rs
;; Copyright 2012 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl logic relations)
  (export
    succeed fail
    in all some concat size right-of left-of next-to middle
    less greater plus minus)
  (import
    (rnrs base)
    (rnrs lists)
    (xitomatl logic))

  ; Some impure cheating for efficiency :)


  (define (succeed know) (list know))
  (define (fail know) '())


  ;;;; Lists ------------------------------------------------------------------

  (define (in x l)
    (vars (a r)
      (conj (== (cons a r) l)
            (disj (== x a)
                  (in x r)))))

  (define (all x l)
    (disj (== l '())
          (vars (r)
            (conj (== l (cons x r))
                  (all x r)))))

  (define (some x l)
    (vars (a r)
      (conj (== l (cons a r))
            (disj (== a x)
                  (conj (neg (== a x))
                        (some x r))))))

  (define (concat l1 l2 l3)
    (disj (conj (== l1 '()) (== l2 l3))
          (vars (h t m)
            (conj (== (cons h t) l1)
                  (== (cons h m) l3)
                  (concat t l2 m)))))

  (define (size l x)
    (escape/deref (l x)
      (assert (or (var? l) (null? l) (pair? l)))
      (assert (var-or-nat? x))
      (cond ((var? l)
             (if (var? x)
               (let enum-lx ((a '()) (s 0))
                 (disj (conj (== l a) (== x s))
                       (enum-lx (cons ? a) (+ 1 s))))
               (let make-l ((a '()) (i x))
                 (if (positive? i)
                   (make-l (cons ? a) (- i 1))
                   (== l a)))))
            ((null? l)
             (== x 0))
            (else ; pair
             (let loop ((l l) (c 0))
               (cond ((pair? l)
                      (loop (cdr l) (+ 1 c)))
                     ((var? x)
                      (vars (x*)
                        (conj (size l x*)
                              (minus x c x*))))
                     (else
                      (let ((x (- x c)))
                        (if (negative? x) fail (size l x))))))))))

  (define (right-of x y l)
    (disj (== (cons* y x ?) l)
          (vars (r)
            (conj (== (cons ? r) l)
                  (right-of x y r)))))

  (define (left-of x y l) (right-of y x l))

  (define (next-to x y l) (disj (left-of x y l) (right-of x y l)))

  (define (middle x l)
    (let recur ((l l) (suffix '()))
      (vars (a r)
        (conj (== (cons a r) l)
              (disj (conj (== x a)
                          (== suffix r))
                    (recur r (cons ? suffix)))))))


  ;;;; Natural Numbers --------------------------------------------------------

  (define (natural? x) (and (integer? x) (not (negative? x))))
  (define (var-or-nat? x) (or (var? x) (natural? x)))


  (define (less x y)
    (escape/deref (x y)
      (define (enum-x y)
        (let rec ((xi 0))
          (if (< xi y)
            (disj (== x xi)
                  (rec (+ 1 xi)))
            fail)))
      (assert (for-all var-or-nat? (list x y)))
      (cond ((number? x)
             (if (number? y)
               (if (< x y) succeed fail)
               (let enum-y ((yi (+ 1 x)))
                 (disj (== y yi)
                       (enum-y (+ 1 yi))))))
            ((number? y)
             (enum-x y))
            (else
             (if (eq? x y) ; same var?
               fail
               (let enum-xy ((yi 1))
                 (disj (conj (== y yi)
                             (enum-x yi))
                       (enum-xy (+ 1 yi)))))))))


  (define (greater x y) (less y x))


  (define (plus x y z)
    (escape/deref (x y z)
      (define (diff v n)
        (if (<= n z) (== v (- z n)) fail))
      (define (enum-v v n)
        (if (or (not (eq? z v)) ; same var?
                (zero? n))
          (let rec ((i 0))
            (disj (conj (== v i) (== z (+ i n)))
                  (rec (+ 1 i))))
          fail)) ; TODO: test tail-call space-safety of the goal-call
      (define (enum-xy z)
        (let rec ((xi 0) (yi z))
          (if (<= xi z)
            (disj (conj (== x xi) (== y yi))
                  (rec (+ 1 xi) (- yi 1)))
            fail)))
      (assert (for-all var-or-nat? (list x y z)))
      (cond ((number? x)
             (cond ((number? y) (== z (+ x y)))
                   ((number? z) (diff y x))
                   (else (enum-v y x))))
            ((number? y)
             (cond ((number? z) (diff x y))
                   (else (enum-v x y))))
            ((number? z)
             (enum-xy z))
            (else
             (if (and (eq? x y) (eq? y z)) ; same var?
               (conj (== x 0))
               (let enum-xyz ((i 0))
                 (disj (conj (== z i)
                             (enum-xy i))
                       (enum-xyz (+ 1 i)))))))))


  (define (minus x y z) (plus z y x))

)
