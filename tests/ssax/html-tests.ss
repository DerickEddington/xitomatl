#!/usr/bin/env scheme-script
#!r6rs
(import
  (except (rnrs) assert)
  (rename (xitomatl ssax html)
          (sxml->html SXML->HTML)
          (string->good-html string->goodHTML))
  (xitomatl ssax tree-trans)
  (xitomatl include)
  (xitomatl srfi lightweight-testing)
  (xitomatl ssax private-5-1 output)
  (xitomatl ssax private-5-1 misc))

(define-syntax assert
  (syntax-rules ()
    [(_ expr ...)
     (begin (check expr => #t) ...)]))

(include/resolve ("xitomatl" "tests" "ssax") "vSXML-to-HTML.scm")

(check-report)