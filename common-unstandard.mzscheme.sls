#!r6rs
(library (xitomatl common-unstandard)
  (export
    add1 sub1
    format printf fprintf pretty-print
    gensym
    make-list last-pair
    ;; TODO: add to as needed/appropriate
    )
  (import
    (rnrs)
    (only (scheme) add1 sub1 format printf fprintf gensym void)
    (only (scheme pretty) pretty-print)
    (only (xitomatl predicates) exact-non-negative-integer?))
  
  (define make-list
    (case-lambda 
      [(n) (make-list n (void))]
      [(n v)
       (unless (exact-non-negative-integer? n)
         (assertion-violation 'make-list "not a valid length" n))
       (let loop ([n n] [r '()])
         (if (= 0 n)
           r
           (loop (sub1 n) (cons v r))))]))
  
  (define (last-pair x)
    (unless (pair? x)
      (assertion-violation 'last-pair "not a pair" x))
    (let loop ([x x])
      (if (pair? (cdr x))
        (loop (cdr x))
        x)))
)