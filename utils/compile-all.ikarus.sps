#!r6rs
;; Automatically generated by utils/make-compile-all.sps
;; Do: ikarus --compile-dependencies compile-all.ikarus.sps
(import
  (only (xitomatl AS-match))
  (only (xitomatl IU-match))
  (only (xitomatl IU-match extras))
  (only (xitomatl IU-match macro-helpers))
  (only (xitomatl R6RS-bindings spec))
  (only (xitomatl R6RS-bindings utils))
  (only (xitomatl R6RS-lexer))
  (only (xitomatl R6RS-lexical-transformations))
  (only (xitomatl alists))
  (only (xitomatl box))
  (only (xitomatl bytevectors))
  (only (xitomatl common))
  (only (xitomatl conditionals))
  (only (xitomatl conditions))
  (only (xitomatl conditions print-condition))
  (only (xitomatl control))
  (only (xitomatl coroutines))
  (only (xitomatl curry))
  (only (xitomatl datum-find))
  (only (xitomatl debug))
  (only (xitomatl define))
  (only (xitomatl define define-values))
  (only (xitomatl define extras))
  (only (xitomatl delimited-control))
  (only (xitomatl engines))
  (only (xitomatl enumerators))
  (only (xitomatl environments))
  (only (xitomatl environments compat))
  (only (xitomatl exceptions))
  (only (xitomatl feature-cond))
  (only (xitomatl file-system base))
  (only (xitomatl file-system base compat))
  (only (xitomatl file-system paths))
  (only (xitomatl file-system value-file))
  (only (xitomatl fmt (0 6)))
  (only (xitomatl fmt base (0 6)))
  (only (xitomatl fmt c (0 6)))
  (only (xitomatl fmt color (0 6)))
  (only (xitomatl fmt column (0 6)))
  (only (xitomatl fmt let-optionals*))
  (only (xitomatl fmt pretty (0 6)))
  (only (xitomatl fmt srfi-33))
  (only (xitomatl fmt unicode (0 6)))
  (only (xitomatl fuego))
  (only (xitomatl gcc-ast))
  (only (xitomatl generics))
  (only (xitomatl htmlprag (0 16)))
  (only (xitomatl include))
  (only (xitomatl include compat))
  (only (xitomatl indexes))
  (only (xitomatl irregex (0 7 5)))
  (only (xitomatl irregex counting))
  (only (xitomatl irregex extras))
  (only (xitomatl irregex-tool))
  (only (xitomatl keywords))
  (only (xitomatl keywords expand-time))
  (only (xitomatl keywords expand-time process-options))
  (only (xitomatl keywords parser))
  (only (xitomatl lang))
  (only (xitomatl lexer))
  (only (xitomatl library-utils))
  (only (xitomatl lists))
  (only (xitomatl lists compat))
  (only (xitomatl macro-utils))
  (only (xitomatl macro-utils fib))
  (only (xitomatl macro-utils fib ctxt))
  (only (xitomatl macro-utils fib p-ctxt))
  (only (xitomatl match (1 2)))
  (only (xitomatl numeral-system balanced-nonary))
  (only (xitomatl persistence base))
  (only (xitomatl persistence transcoded-serializing))
  (only (xitomatl ports))
  (only (xitomatl ports compat))
  (only (xitomatl predicates))
  (only (xitomatl profiler meta))
  (only (xitomatl profiler srfi-time))
  (only (xitomatl queue))
  (only (xitomatl records))
  (only (xitomatl regexp))
  (only (xitomatl repl))
  (only (xitomatl repl compat))
  (only (xitomatl rnrs-profiled))
  (only (xitomatl rnrs-restricted))
  (only (xitomatl ssax html (5 1)))
  (only (xitomatl ssax multi-parser))
  (only (xitomatl ssax parsing (5 1)))
  (only (xitomatl ssax private-5-1 define-opt))
  (only (xitomatl ssax private-5-1 error))
  (only (xitomatl ssax private-5-1 input-parse))
  (only (xitomatl ssax private-5-1 look-for-str))
  (only (xitomatl ssax private-5-1 misc))
  (only (xitomatl ssax private-5-1 output))
  (only (xitomatl ssax private-5-1 to-html))
  (only (xitomatl ssax private-5-1 to-html-ext))
  (only (xitomatl ssax private-5-1 util))
  (only (xitomatl ssax raise))
  (only (xitomatl ssax sxpath (5 1)))
  (only (xitomatl ssax tree-trans (5 1)))
  (only (xitomatl stack-lang))
  (only (xitomatl stack-lang core))
  (only (xitomatl stack-lang unsafe))
  (only (xitomatl strings))
  (only (xitomatl strings compat))
  (only (xitomatl sxml-match (1 0)))
  (only (xitomatl sxml-match void))
  (only (xitomatl sxml-tools ddo-axes (2008 6 27)))
  (only (xitomatl sxml-tools ddo-txpath (2008 6 27)))
  (only (xitomatl sxml-tools fragments (2008 6 27)))
  (only (xitomatl sxml-tools guides (2008 6 27)))
  (only (xitomatl sxml-tools lazy-xpath (2008 6 27)))
  (only (xitomatl sxml-tools modif (2008 6 27)))
  (only (xitomatl sxml-tools serializer (2008 6 27)))
  (only (xitomatl sxml-tools stx-engine (2008 6 27)))
  (only (xitomatl sxml-tools sxml-tools (2008 6 27)))
  (only (xitomatl sxml-tools sxpath (2008 6 27)))
  (only (xitomatl sxml-tools sxpath-ext (2008 6 27)))
  (only (xitomatl sxml-tools sxpath-plus (2008 6 27)))
  (only (xitomatl sxml-tools sxpathlib (2008 6 27)))
  (only (xitomatl sxml-tools txpath (2008 6 27)))
  (only (xitomatl sxml-tools xlink-parser (2008 6 27)))
  (only (xitomatl sxml-tools xpath-ast (2008 6 27)))
  (only (xitomatl sxml-tools xpath-context--xlink (2008 6 27)))
  (only (xitomatl sxml-tools xpath-parser (2008 6 27)))
  (only (xitomatl vectors))
  (only (xitomatl zipper base))
  (only (xitomatl zipper trees))
)
