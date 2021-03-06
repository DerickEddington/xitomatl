#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(import
  (rnrs)
  (xitomatl sxml-tools xpath-context--xlink)
  (xitomatl include)
  (xitomatl sxml-tools sxpathlib)
  (xitomatl tests sxml-tools xtest-harness)
  (xitomatl tests sxml-tools xtest-lib)
  (xitomatl tests sxml-tools define-macro)
  (xitomatl ssax private-5-1 output)
  (rename (only (xitomatl common) pretty-print)
          (pretty-print pp)))

(define (myenv:error . args)
  (apply assertion-violation 'sxp:error "test failed" args))

(include/resolve ("xitomatl" "tests" "sxml-tools") "vcontext.scm")
