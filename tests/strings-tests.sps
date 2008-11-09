#!r6rs
(import
  (rnrs)
  (xitomatl strings)
  (xitomatl srfi lightweight-testing))

(check (string-intersperse '() "")
       => "")
(check (string-intersperse '() ":")
       => "")
(check (string-intersperse '() ":+")
       => "")
(check (string-intersperse '() ":+=")
       => "")
(check (string-intersperse '() ":+=-")
       => "")
(check (string-intersperse '("a") "")
       => "a")
(check (string-intersperse '("a") ":")
       => "a")
(check (string-intersperse '("a") ":+")
       => "a")
(check (string-intersperse '("a") ":+=")
       => "a")
(check (string-intersperse '("a") ":+=-")
       => "a")
(check (string-intersperse '("a" "b") "")
       => "ab")
(check (string-intersperse '("a" "b") ":")
       => "a:b")
(check (string-intersperse '("a" "b") ":+")
       => "a:+b")
(check (string-intersperse '("a" "b") ":+=")
       => "a:+=b")
(check (string-intersperse '("a" "b") ":+=-")
       => "a:+=-b")
(check (string-intersperse '("a" "b" "c") "")
       => "abc")
(check (string-intersperse '("a" "b" "c") ":")
       => "a:b:c")
(check (string-intersperse '("a" "b" "c") ":+")
       => "a:+b:+c")
(check (string-intersperse '("a" "b" "c") ":+=")
       => "a:+=b:+=c")
(check (string-intersperse '("a" "b" "c") ":+=-")
       => "a:+=-b:+=-c")
(check (string-intersperse '("a" "b" "c" "d" "e" "f") "") 
       => "abcdef")
(check (string-intersperse '("a" "b" "c" "d" "e" "f") ":") 
       => "a:b:c:d:e:f")
(check (string-intersperse '("a" "b" "c" "d" "e" "f") ":+") 
       => "a:+b:+c:+d:+e:+f")
(check (string-intersperse '("a" "b" "c" "d" "e" "f") ":+=") 
       => "a:+=b:+=c:+=d:+=e:+=f")
(check (string-intersperse '("a" "b" "c" "d" "e" "f") ":+=-") 
       => "a:+=-b:+=-c:+=-d:+=-e:+=-f")
(check (string-intersperse '("a1" "b22" "c333" "d4444" "e55555" "f666666") "") 
       => "a1b22c333d4444e55555f666666")
(check (string-intersperse '("a1" "b22" "c333" "d4444" "e55555" "f666666") ":") 
       => "a1:b22:c333:d4444:e55555:f666666")
(check (string-intersperse '("a1" "b22" "c333" "d4444" "e55555" "f666666") ":+") 
       => "a1:+b22:+c333:+d4444:+e55555:+f666666")
(check (string-intersperse '("a1" "b22" "c333" "d4444" "e55555" "f666666") ":+=") 
       => "a1:+=b22:+=c333:+=d4444:+=e55555:+=f666666")
(check (string-intersperse '("a1" "b22" "c333" "d4444" "e55555" "f666666") ":+=-") 
       => "a1:+=-b22:+=-c333:+=-d4444:+=-e55555:+=-f666666")

(check (for-all char-whitespace? (string->list whitespace)) 
       => #t)
(let ([ws (let loop ([i 0] [a '()])
            (if (<= i #x10ffff)
              (loop (+ 1 i)
                    (if (<= #xd800 i #xdfff)
                      a
                      (let ([c (integer->char i)])
                        (if (char-whitespace? c)
                          (cons c a)
                          a))))
              (reverse a)))]
      [sorted (list-sort char<? (string->list whitespace))])
  (check (equal? ws sorted)
         => #t)
  (check (string=? (apply string ws) (apply string sorted))
         => #t))

(check (string-split "")
       => '())
(check (string-split "a")
       => '("a"))
(check (string-split "ab")
       => '("ab"))
(check (string-split "abc")
       => '("abc"))
(check (string-split "abcdef")
       => '("abcdef"))
(check (string-split " a")
       => '("a"))
(check (string-split "a ")
       => '("a"))
(check (string-split " a ")
       => '("a"))
(check (string-split "a b")
       => '("a" "b"))
(check (string-split " a b")
       => '("a" "b"))
(check (string-split "a b ")
       => '("a" "b"))
(check (string-split " a b ")
       => '("a" "b"))
(check (string-split "\x2001;a\x1680;b\x3000;")
       => '("a" "b"))
(check (string-split "a     b")
       => '("a" "b"))
(check (string-split "   a     b")
       => '("a" "b"))
(check (string-split "a     b    ")
       => '("a" "b"))
(check (string-split "   a     b    ")
       => '("a" "b"))
(check (string-split "a b c")
       => '("a" "b" "c"))
(check (string-split " a b c")
       => '("a" "b" "c"))
(check (string-split "a b c ")
       => '("a" "b" "c"))
(check (string-split " a b c ")
       => '("a" "b" "c"))
(check (string-split "a     b       c")
       => '("a" "b" "c"))
(check (string-split "             a     b       c")
       => '("a" "b" "c"))
(check (string-split "a     b       c                           ")
       => '("a" "b" "c"))
(check (string-split "              a               b       c             ")
       => '("a" "b" "c"))
(check (string-split "a b c d e f")
       => '("a" "b" "c" "d" "e" "f"))
(check (string-split " a b c d e f")
       => '("a" "b" "c" "d" "e" "f"))
(check (string-split "a b c d e f ")
       => '("a" "b" "c" "d" "e" "f"))
(check (string-split "a     b c      d e f")
       => '("a" "b" "c" "d" "e" "f"))
(check (string-split " a     b c      d e f")
       => '("a" "b" "c" "d" "e" "f"))
(check (string-split "a     b c      d e f ")
       => '("a" "b" "c" "d" "e" "f"))
(check (string-split " a     b c      d e f ")
       => '("a" "b" "c" "d" "e" "f"))
(check (string-split "    a     b c                 d e  f                  ")
       => '("a" "b" "c" "d" "e" "f"))

(check (string-split "\t\n\r")
       => '())
(check (string-split "\t\n\r \t\n\r")
       => '())
(check (string-split "\t\n\r " "\t\n\r " #t)
       => '("" "" "" "" ""))
(check (string-split "\na\r")
       => '("a"))
(check (string-split "\n\tab\t\r\t\n\n")
       => '("ab"))
(check (string-split "\n\t\t\rabc\n\r")
       => '("abc"))
(check (string-split "\n\x205F;\v\x202F;\rabc\f\xA0;" whitespace #t)
       => '("" "" "" "" "" "abc" "" ""))
(check (string-split "\rabcdef\r")
       => '("abcdef"))
(check (string-split " \r\n\ta")
       => '("a"))
(check (string-split "a\r \n \t ")
       => '("a"))
(check (string-split " \t \n a \r")
       => '("a"))
(check (string-split "a\nb")
       => '("a" "b"))
(check (string-split " a\r\nb")
       => '("a" "b"))
(check (string-split "a\n\rb ")
       => '("a" "b"))
(check (string-split " a\tb\r\n")
       => '("a" "b"))
(check (string-split "a   \n \n b\n")
       => '("a" "b"))
(check (string-split " \r  a  \n   b")
       => '("a" "b"))
(check (string-split "a  \n   b  \r \t \r\n \n\r ")
       => '("a" "b"))
(check (string-split "\t\r   \na  \r\n\t   b  \t\r \n\r ")
       => '("a" "b"))
(check (string-split "a\nb\rc")
       => '("a" "b" "c"))
(check (string-split "\ta\nb\rc\t")
       => '("a" "b" "c"))
(check (string-split "\ra\n\rb\r\nc")
       => '("a" "b" "c"))
(check (string-split "\n\ra\tb\nc\r")
       => '("a" "b" "c"))
(check (string-split "a\n\n\n\n\n\n\nb\r\r\r\r\rc\t\t\t\t\t\t")
       => '("a" "b" "c"))
(check (string-split " \n \n \n \n a \r \rb\t\t \tc")
       => '("a" "b" "c"))
(check (string-split "a\n   b\n\t\t\rc    \n\n \r   \t           ")
       => '("a" "b" "c"))
(check (string-split "              \na             b\r     c\t           \r")
       => '("a" "b" "c"))
(check (string-split "a\rb\nc\td\te\nf")
       => '("a" "b" "c" "d" "e" "f"))
(check (string-split "\na\nb\nc\nd\ne\nf")
       => '("a" "b" "c" "d" "e" "f"))
(check (string-split "a\tb\tc\td\te\tf\t")
       => '("a" "b" "c" "d" "e" "f"))
(check (string-split "a  \t b c    \rd\n\te\rf\r")
       => '("a" "b" "c" "d" "e" "f"))
(check (string-split "\n a  \r \t b c \t \r \n        d e  f\r\t\n\t\n\r\r\n\t")
       => '("a" "b" "c" "d" "e" "f"))
(check (string-split "\n a  \r \t b c \t \r \n        d e  f\r\t\n\t\n\r\r\n\t" "\t\n\r " #t)
       => '("" "" "a" "" "" "" "" "" "b" "c" "" "" "" "" "" "" "" "" "" "" "" "" "" "d" "e" "" "f" "" "" "" "" "" "" "" "" ""))

(check (string-split "" ":")
       => '())
(check (string-split "a" ":")
       => '("a"))
(check (string-split "ab" ":")
       => '("ab"))
(check (string-split "abc" ":")
       => '("abc"))
(check (string-split "abcdef" ":")
       => '("abcdef"))
(check (string-split ":" ":")
       => '())
(check (string-split "::" ":")
       => '())
(check (string-split ":a" ":")
       => '("a"))
(check (string-split "a:" ":")
       => '("a"))
(check (string-split ":a:" ":")
       => '("a"))
(check (string-split "a:b" ":")
       => '("a" "b"))
(check (string-split ":a:b" ":")
       => '("a" "b"))
(check (string-split "a:b:" ":")
       => '("a" "b"))
(check (string-split ":a:b:" ":")
       => '("a" "b"))
(check (string-split "a:::::b" ":")
       => '("a" "b"))
(check (string-split ":::::" ":")
       => '())
(check (string-split ":::a:::::b" ":")
       => '("a" "b"))
(check (string-split "a:::::b::::" ":")
       => '("a" "b"))
(check (string-split ":::a:::::b::::" ":")
       => '("a" "b"))
(check (string-split "a:b:c" ":")
       => '("a" "b" "c"))
(check (string-split ":a:b:c" ":")
       => '("a" "b" "c"))
(check (string-split "a:b:c:" ":")
       => '("a" "b" "c"))
(check (string-split ":a:b:c:" ":")
       => '("a" "b" "c"))
(check (string-split "a:::::b:::::::c" ":")
       => '("a" "b" "c"))
(check (string-split ":::::::::::::a:::::b:::::::c" ":")
       => '("a" "b" "c"))
(check (string-split "a:::::b:::::::c:::::::::::::::::::::::::::" ":")
       => '("a" "b" "c"))
(check (string-split "::::::::::::::a:::::::::::::::b:::::::c:::::::::::::" ":")
       => '("a" "b" "c"))
(check (string-split "a:b:c:d:e:f" ":")
       => '("a" "b" "c" "d" "e" "f"))
(check (string-split ":a:b:c:d:e:f" ":")
       => '("a" "b" "c" "d" "e" "f"))
(check (string-split "a:b:c:d:e:f:" ":")
       => '("a" "b" "c" "d" "e" "f"))
(check (string-split "a:::::b:c::::::d:e:f" ":")
       => '("a" "b" "c" "d" "e" "f"))
(check (string-split ":a:::::b:c::::::d:e:f" ":")
       => '("a" "b" "c" "d" "e" "f"))
(check (string-split "a:::::b:c::::::d:e:f:" ":")
       => '("a" "b" "c" "d" "e" "f"))
(check (string-split ":a:::::b:c::::::d:e:f:" ":")
       => '("a" "b" "c" "d" "e" "f"))
(check (string-split "::::a:::::b:c:::::::::::::::::d:e::f::::::::::::::::::" ":")
       => '("a" "b" "c" "d" "e" "f"))

(check (string-split "" ":")
       => '())
(check (string-split "a" ":")
       => '("a"))
(check (string-split "ab" ":")
       => '("ab"))
(check (string-split "abc" ":")
       => '("abc"))
(check (string-split "abcdef" ":")
       => '("abcdef"))
(check (string-split " a" ":")
       => '(" a"))
(check (string-split "a " ":")
       => '("a "))
(check (string-split " a " ":")
       => '(" a "))
(check (string-split "a b" ":")
       => '("a b"))
(check (string-split " a b" ":")
       => '(" a b"))
(check (string-split "a b " ":")
       => '("a b "))
(check (string-split " a b " ":")
       => '(" a b "))
(check (string-split "a     b" ":")
       => '("a     b"))
(check (string-split "   a     b" ":")
       => '("   a     b"))
(check (string-split "a     b    "  ":")
       => '("a     b    "))
(check (string-split "   a     b    " ":")
       => '("   a     b    "))
(check (string-split "a b c" ":")
       => '("a b c"))
(check (string-split " a b c" ":")
       => '(" a b c"))
(check (string-split "a b c " ":")
       => '("a b c "))
(check (string-split " a b c " ":")
       => '(" a b c "))
(check (string-split "a     b       c" ":")
       => '("a     b       c"))
(check (string-split "             a     b       c" ":")
       => '("             a     b       c"))
(check (string-split "a     b       c                           " ":")
       => '("a     b       c                           "))
(check (string-split "              a               b       c             " ":")
       => '("              a               b       c             "))
(check (string-split "a b c d e f" ":")
       => '("a b c d e f"))
(check (string-split " a b c d e f" ":")
       => '(" a b c d e f"))
(check (string-split "a b c d e f " ":")
       => '("a b c d e f "))
(check (string-split "a     b c      d e f" ":")
       => '("a     b c      d e f"))
(check (string-split " a     b c      d e f" ":")
       => '(" a     b c      d e f"))
(check (string-split "a     b c      d e f " ":")
       => '("a     b c      d e f "))
(check (string-split " a     b c      d e f " ":")
       => '(" a     b c      d e f "))
(check (string-split "    a     b c                 d e  f                  " ":")
       => '("    a     b c                 d e  f                  "))


(check (string-split "" ":" #t)
       => '(""))
(check (string-split ":" ":" #t)
       => '("" ""))
(check (string-split "a" ":" #t)
       => '("a"))
(check (string-split "ab" ":" #t)
       => '("ab"))
(check (string-split "abc" ":" #t)
       => '("abc"))
(check (string-split "abcdef" ":" #t)
       => '("abcdef"))
(check (string-split ":a" ":" #t)
       => '("" "a"))
(check (string-split "a:" ":" #t)
       => '("a" ""))
(check (string-split ":a:" ":" #t)
       => '("" "a" ""))
(check (string-split "a:b" ":" #t)
       => '("a" "b"))
(check (string-split ":a:b" ":" #t)
       => '("" "a" "b"))
(check (string-split "a:b:" ":" #t)
       => '("a" "b" ""))
(check (string-split ":a:b:" ":" #t)
       => '("" "a" "b" ""))
(check (string-split "a:::::b" ":" #t)
       => '("a" "" "" "" "" "b"))
(check (string-split ":::::" ":" #t)
       => '("" "" "" "" "" ""))
(check (string-split ":::a:::::b" ":" #t)
       => '("" "" "" "a" "" "" "" "" "b"))
(check (string-split "a:::::b::::" ":" #t)
       => '("a" "" "" "" "" "b" "" "" "" ""))
(check (string-split ":::a:::b:::::" ":" #t)
       => '("" "" "" "a" "" "" "b" "" "" "" "" ""))
(check (string-split "a:b:c" ":" #t)
       => '("a" "b" "c"))
(check (string-split ":a:b:c" ":" #t)
       => '("" "a" "b" "c"))
(check (string-split "a:b:c:" ":" #t)
       => '("a" "b" "c" ""))
(check (string-split ":a:b:c:" ":" #t)
       => '("" "a" "b" "c" ""))
(check (string-split "a:::::b:::::::c" ":" #t)
       => '("a" "" "" "" "" "b" "" "" "" "" "" "" "c"))
(check (string-split ":::::::::::::a::b:::::::c" ":" #t)
       => '( "" "" "" "" "" "" "" "" "" "" "" "" "" "a" "" "b" "" "" "" "" "" "" "c"))
(check (string-split "a::::b::::::::c:::::::::::::::::::::::::::" ":" #t)
       => '("a" "" "" "" "b" "" "" "" "" "" "" "" "c" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" ""))
(check (string-split "a:b:c:d:e:f" ":" #t)
       => '("a" "b" "c" "d" "e" "f"))
(check (string-split ":a:b:c:d:e:f" ":" #t)
       => '("" "a" "b" "c" "d" "e" "f"))
(check (string-split "a:b:c:d:e:f:" ":" #t)
       => '("a" "b" "c" "d" "e" "f" ""))
(check (string-split "a:::::b:c::::::d:e:f" ":" #t)
       => '("a" "" "" "" "" "b" "c" "" "" "" "" "" "d" "e" "f"))
(check (string-split ":a:::b:c::::::d:e:f" ":" #t)
       => '("" "a" "" "" "b" "c" "" "" "" "" "" "d" "e" "f"))
(check (string-split "::::a:b:c:::d:e:f:" ":" #t)
       => '("" "" "" "" "a" "b" "c" "" "" "d" "e" "f" ""))
(check (string-split "::a:::::b:c:::::::::::::::::d:e::f::::::::::::::::::" ":" #t)
       => '("" "" "a" "" "" "" "" "b" "c" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "d" "e" "" "f" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" ""))

(check (string-end=? "" "") => #t)
(check (string-end=? "foo bar" "") => #t)
(check (string-end=? "foo bar" "r") => #t)
(check (string-end=? "foo bar" "ar") => #t)
(check (string-end=? "foo bar" "o bar") => #t)
(check (string-end=? "foo bar" "a") => #f)
(check (string-end=? "foo bar" "ba") => #f)
(check (string-end=? "foo bar" "λ") => #f)

(define s0 (string-copy "This is a sentence."))
(define s1 (make-string 10))
(string-copy! s0 3 s1 0 10)
(check s1 => "s is a sen")
(string-copy! s0 11 s1 1 6)
(check s1 => "sentencsen")
(string-copy! s0 2 s0 13 4)
(check s0 => "This is a senis ie.")
(string-copy! s0 5 s0 0 10)
(check s0 => "is a senissenis ie.")

(check-report)
