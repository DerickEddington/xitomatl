#!r6rs
;; Copyright 2012 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

; TODO: Tests that demonstrate goal-creating expressions being evaluated more
; than once.

; TODO: Investigate redesigning so that goal-creating expressions are evaluated
; only once.

; TODO: Investigate how and when, maybe the default, to have interleaving of
; solutions of sub-goals.  Currently disj yields all the solutions of a sub-goal
; before those of following sub-goals, but this is problematic especially when a
; sub-goal yields infinite amount of solutions.

; TODO: Statistics collection of execution complexity, disabled by default.

(library (xitomatl logic)
  (export
    ? vars var?
    solve solve*
    == conj disj #;disj/while #;disj/until neg
    escape/deref debug-vars
    deref deref*)
  (import
    (rnrs base)
    (rnrs control)
    (rnrs lists)
    (rnrs io simple)
    (rnrs records syntactic)
    (rnrs hashtables)
    (xitomatl common)
    (xitomatl logic stream))


  (define-record-type var (fields name))

  (define-syntax vars
    (syntax-rules ()
      ((_ (v ...) . body)
       (let ((v (make-var 'v)) ...) . body))))

  (define ? (make-var '?))
  (define (maybe-? v)
    (cond ((pair? v) (cons (maybe-? (car v)) (maybe-? (cdr v))))
          ((eq? ? v) (make-var '?))
          (else v)))

  (define (empty-knowledge) '())

  (define (extend v x know)
    (assert (var? v))
    (cons (cons v x) know))

  (define (deref x know)
    (cond ((and (var? x) (assq x know)) => (lambda (p) (deref (cdr p) know)))
          (else x)))

  (define (unify a b know)
    (let ((a (deref a know)) (b (deref b know)))
      (cond ((eq? a b) know)
            ((var? a) (extend (maybe-? a) (maybe-? b) know))
            ((var? b) (extend (maybe-? b) (maybe-? a) know))
            ((and (pair? a) (pair? b))
             (let ((k (unify (car a) (car b) know)))
               (and k (unify (cdr a) (cdr b) k))))
            ((equal? a b) know)
            (else #F))))


  ;;;; Core relations ---------------------------------------------------------

  (define (== a b)
    (lambda (know)
      (delay (cond ((unify a b know) => list)
                   (else '())))))

  (define (debug-vars . vars)
    (lambda (know)
      (delay
        (begin (display "--- debug vars ----------------------------------------------\n")
               (for-each (lambda (v)
                           (when (var? v)
                             (let ((d (deref* v know)))
                               (if (eq? v d)
                                 (printf "~a not bound\n" (var-name v))
                                 (printf "~a = ~s\n" (var-name v) d)))))
                         vars)
               (display "-------------------------------------------------------------\n")
               (list know)))))


  (define-syntax define-goal-streamer
    (syntax-rules ()
      ((_ id id*)
       (define-syntax id
         (syntax-rules ()
           ((_ goals (... ...))
            (id* (fixed-stream goals (... ...)))))))))


  (define-goal-streamer conj conj*)

  (define (conj* stream-of-goals)
    ; TODO: Maybe better to use some stream-fold passing knowledge-stream as seed?
    (lambda (know)
      (let ((g (force stream-of-goals)))
        (if (null? g)
          (list know) ; success
          (stream-flatten
           (stream-map (conj* (cdr g))
                       ((car g) know)))))))


  (define (disj/control* pred stream-of-goals)
    (lambda (know)
      (stream-flatten
       (stream-map-while (lambda (g)
                           (let ((ks (force (g know))))
                             (values (pred ks) ks)))
                         stream-of-goals))))

  (define-goal-streamer disj disj*)

  (define (disj* stream-of-goals)
    (disj/control* (lambda (ks) #T) stream-of-goals))

#;(define-goal-streamer disj/while disj/while*) ; TODO: How to describe?
#;(define (disj/while* stream-of-goals)
    (disj/control* (lambda (ks) (not (null? ks))) stream-of-goals))

#;(define-goal-streamer disj/until disj/until*) ; Short-circuit Or
#;(define (disj/until* stream-of-goals)
    (disj/control* null? stream-of-goals))


  (define (neg* goal-promise)
    (lambda (know)
      (let* ((goal (force goal-promise))
             (ks (goal know)))
        (if (null? (force ks))
          (list know) ; success
          '()))))     ; failure

  (define-syntax neg
    (syntax-rules ()
      ((_ goal)
       (neg* (delay goal)))))


  (define-syntax escape/deref
    ; TODO? Maybe this can participate in the reducing of goal-procedure creation?
    (syntax-rules ()
      ((_ (v ...) . body)
       (lambda (know)
         ((let ((v (deref v know)) ...) . body) ; Returns a goal to apply.
          know)))))


  ;;;; Solving ----------------------------------------------------------------

  (define (deref* x know)
    ; Replace embedded variables with their values.
    (let ((x (deref x know)))
      (if (pair? x)
        (cons (deref* (car x) know)
              (deref* (cdr x) know))
        x)))

  (define (reify-var-names know)
    ; Create a mapping of logic variables in a knowledge to reified names.
    (define (rn v i)
      (string->symbol (string-append (symbol->string (var-name v)) "." (number->string i))))
    (define ht (make-eq-hashtable))
    (let recur ((t know) (i 0))
      (cond ((pair? t)
             (recur (car t) (recur (cdr t) i)))
            ((and (var? t)
                  (not (hashtable-contains? ht t)))
             (hashtable-set! ht t (rn t i))
             (+ 1 i))
            (else i)))
    ht)

  (define (solve** limit for goal)
    ; Start the initial goal, and run until the amount of solutions reaches the
    ; limit.
    (define knows (stream->list (goal (empty-knowledge)) limit))
    (if (eq? 'raw for)
      knows
      (map (lambda (know)
             ; Make the returned solutions more aesthetic.
             (define rm (reify-var-names know))
             (define (r t)
               (cond ((pair? t)
                      (cons (r (car t)) (r (cdr t))))
                     ((var? t)
                      (or (and (or (eq? 'show for)
                                   (not (memq t for)))
                               (hashtable-ref rm t #F))
                          (var-name t)))
                     (else t)))
             `(solution .
                ,(map (lambda (p) `(,(r (car p)) = ,(r (cdr p))))
                      (if (eq? 'show for)
                        ; Return everything in the knowledge, with reified variable names,
                        ; without replacing embedded variables with their values.
                        know
                        ; Return only what is requested, with reified variable names, with
                        ; values replaced for embedded variables.
                        (fold-right (lambda (f a)
                                      (let ((f* (deref* f know)))
                                        (if (eq? f f*) a
                                            (cons (cons f f*) a))))
                                    '() for)))))
           knows)))

  (define-syntax solve*
    (syntax-rules (show raw)
      ((_ limit (for ...) goal)
       (vars (for ...)
         (solve** limit (list for ...) goal)))
      ((_ limit show goal)
       (solve** limit 'show goal))
      ((_ limit raw goal)
       (solve** limit 'raw goal))))

  (define-syntax solve
    (syntax-rules ()
      ((_ . r)
       (solve* +inf.0 . r))))

)
