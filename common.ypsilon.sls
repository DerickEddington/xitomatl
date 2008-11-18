(library (xitomatl common)
  (export
    add1 sub1
    format printf fprintf pretty-print
    gensym
    time
    with-output-to-string
    ;; TODO: add to as needed/appropriate
    )
  (import
    (rnrs)
    (only (core) format gensym microsecond set-current-output-port!)
    (prefix (only (core) pretty-print) ypsilon:)
    (only (time) time))
  
  (define (add1 x) (+ x 1))

  (define (sub1 x) (- x 1))
  
  (define (fprintf port fmt-str . fmt-args)
    (put-string port (apply format fmt-str fmt-args)))
  
  (define (printf fmt-str . fmt-args)
    (apply fprintf (current-output-port) fmt-str fmt-args))
  
  (define pretty-print
    (case-lambda
      [(x)
       (pretty-print x (current-output-port))]
      [(x p)
       (ypsilon:pretty-print x p)
       (newline p)]))

  (define (with-output-to-string thunk)
    (let-values ([(sop get) (open-string-output-port)])
      (let ((temp #f))
        (dynamic-wind
         (lambda ()
           (set! temp (current-output-port))
           (set-current-output-port! sop))
         (lambda () (thunk))
         (lambda ()
           (set-current-output-port! temp)))
        (get))))
)
