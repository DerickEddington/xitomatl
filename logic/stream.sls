#!r6rs
;; Copyright 2012 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl logic stream)
  (export
    delay force
    stream?
    fixed-stream
    stream-map
    stream-map-while
    stream-append
    stream-flatten
    stream->list)
  (import
    (rnrs base)
    (rnrs records syntactic))

  (define-record-type promise (fields delayed))

  (define-syntax delay
    (syntax-rules ()
      ((_ expr)
       (make-promise (lambda () expr)))))

  (define (force x)
    (if (promise? x)
      ((promise-delayed x))
      x))

  (define (stream? x)
    (or (promise? x)
        (null? x)
        (and (pair? x)
             (stream? (cdr x)))))

  (define-syntax fixed-stream
    (syntax-rules ()
      ((_) '())
      ((_ expr0 expr ...)
       (delay (cons expr0 (fixed-stream expr ...))))))

  (define (list->stream l)
    (if (null? l) '()
        (delay (cons (car l) (list->stream (cdr l))))))

  (define (stream->list s n)
    (if (positive? n)
      (let ((s (force s)))
        (if (null? s)
          '()
          (cons (car s) (stream->list (cdr s) (- n 1)))))
      '()))

  (define (stream-map-while proc s)
    (delay (let ((p (force s)))
             (if (null? p) '()
                 (let-values (((? x) (proc (car p))))
                   (if ?
                     (cons x (stream-map-while proc (cdr p)))
                     (list x)))))))

  (define (stream-map proc s)
    (stream-map-while (lambda (x) (values #T (proc x))) s))

  (define (stream-append s1 s2)
    (delay (let ((p (force s1)))
             (if (null? p)
               (force s2)
               (cons (car p)
                     (stream-append (cdr p) s2))))))

  (define (stream-flatten stream-of-stream)
    (delay (let ((p (force stream-of-stream)))
             (if (null? p) '()
                 (force (stream-append (car p)
                                       (stream-flatten (cdr p))))))))

)
