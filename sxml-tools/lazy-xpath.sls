;; Copyright (c) 2009 Derick Eddington
;;
;; Permission is hereby granted, free of charge, to any person obtaining a
;; copy of this software and associated documentation files (the "Software"),
;; to deal in the Software without restriction, including without limitation
;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;; and/or sell copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; Except as contained in this notice, the name(s) of the above copyright
;; holders shall not be used in advertising or otherwise to promote the sale,
;; use or other dealings in this Software without prior written authorization.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;; DEALINGS IN THE SOFTWARE.

#!r6rs
(library (xitomatl sxml-tools lazy-xpath (2008 06 27))
  (export
    lazy:or
    lazy:promise?
    lazy:null?
    lazy:map
    lazy:filter
    lazy:car
    lazy:cdr
    lazy:length
    lazy:result->list
    lazy:node->sxml
    lazy:reach-root
    lazy:contextset->nodeset
    lazy:recover-contextset
    lazy:find-proper-context
    lazy:output-siblings
    lazy:find-foll-siblings
    lazy:find-prec-siblings
    lazy:ancestor
    lazy:ancestor-or-self
    lazy:attribute
    lazy:child
    lazy:descendant
    lazy:descendant-or-self
    lazy:following
    lazy:following-sibling
    lazy:namespace
    lazy:parent
    lazy:preceding
    lazy:preceding-sibling
    lazy:self
    lazy:axis-consume-nodeset
    lazy:string
    lazy:boolean
    lazy:number
    lazy:string-value
    lazy:equality-cmp
    lazy:equal?
    lazy:not-equal?
    lazy:relational-cmp
    lazy:core-last
    lazy:core-position
    lazy:core-count
    lazy:core-id
    lazy:core-local-name
    lazy:core-namespace-uri
    lazy:core-name
    lazy:core-string
    lazy:core-concat
    lazy:core-starts-with
    lazy:core-contains
    lazy:core-substring-before
    lazy:core-substring-after
    lazy:core-substring
    lazy:core-string-length
    lazy:core-normalize-space
    lazy:core-translate
    lazy:core-boolean
    lazy:core-not
    lazy:core-true
    lazy:core-false
    lazy:core-lang
    lazy:core-number
    lazy:core-sum
    lazy:core-floor
    lazy:core-ceiling
    lazy:core-round
    lazy:ast-axis-specifier
    lazy:ast-location-path
    lazy:ast-absolute-location-path
    lazy:ast-relative-location-path
    lazy:ast-step
    lazy:ast-step-list
    lazy:ast-predicate
    lazy:ast-predicate-list
    lazy:ast-expr
    lazy:ast-or-expr
    lazy:ast-and-expr
    lazy:ast-equality-expr
    lazy:ast-relational-expr
    lazy:ast-additive-expr
    lazy:ast-multiplicative-expr
    lazy:ast-union-expr
    lazy:ast-path-expr
    lazy:ast-filter-expr
    lazy:ast-variable-reference
    lazy:ast-literal
    lazy:ast-number
    lazy:ast-function-call
    lazy:ast-function-arguments
    lazy:api-helper
    lazy:txpath
    lazy:xpath-expr
    lazy:sxpath)
  (import
    (rnrs)
    (except (rnrs r5rs) delay force)
    (prefix (only (rnrs r5rs) delay force) r5rs:)
    (xitomatl include)
    (rename (except (srfi :13 strings) string-copy string->list string-titlecase
                    string-upcase string-downcase string-hash string-for-each)
            (string-index-right string-rindex))
    (srfi :2 and-let*)
    (xitomatl sxml-tools sxpathlib)
    (xitomatl sxml-tools sxpath-ext)
    (xitomatl sxml-tools xpath-parser)
    (xitomatl sxml-tools xpath-context--xlink)
    (xitomatl sxml-tools txpath)
    (xitomatl sxml-tools xpath-ast)
    (xitomatl ssax private-5-1 misc)
    (xitomatl ssax private-5-1 util)
    (xitomatl ssax private-5-1 output))

  (define-record-type lazy:promise (fields p))

  (define-syntax delay
    (syntax-rules ()
      [(_ expr)
       (make-lazy:promise (r5rs:delay expr))]))

  (define (force p)
    (r5rs:force (lazy:promise-p p)))

  (include/resolve ("xitomatl" "sxml-tools") "lazy-xpath.scm")
)
