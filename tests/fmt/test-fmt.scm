
#;(cond-expand
 (chicken (use test fmt))
 (gauche
  (use gauche.test)
  (use text.fmt)
  (define test-begin test-start)
  (define orig-test (with-module gauche.test test))
  (define-syntax test
    (syntax-rules ()
      ((test name expected expr)
       (orig-test name expected (lambda () expr)))
      ((test expected expr)
       (orig-test (let ((s (with-output-to-string (lambda () (write 'expr)))))
                    (substring s 0 (min 60 (string-length s))))
                  expected
                  (lambda () expr)))
      )))
 (else))

(test-begin "fmt")

;; basic data types

(test "hi" (fmt #f "hi"))
(test "\"hi\"" (fmt #f (wrt "hi")))
(test "\"hi \\\"bob\\\"\"" (fmt #f (wrt "hi \"bob\"")))
(test "\"hello\\nworld\"" (fmt #f (wrt "hello\nworld")))
(test "ABC" (fmt #f (upcase "abc")))
(test "abc" (fmt #f (downcase "ABC")))
(test "Abc" (fmt #f (titlecase "abc")))

(test "abc     def" (fmt #f "abc" (tab-to) "def"))
(test "abc  def" (fmt #f "abc" (tab-to 5) "def"))
(test "abcdef" (fmt #f "abc" (tab-to 3) "def"))

(test "-1" (fmt #f -1))
(test "0" (fmt #f 0))
(test "1" (fmt #f 1))
(test "10" (fmt #f 10))
(test "100" (fmt #f 100))
;; (test "1e+15" (fmt #f 1e+15))
;; (test "1e+23" (fmt #f 1e+23))
;; (test "1.2e+23" (fmt #f 1.2e+23))
;; (test "1e-5" (fmt #f 1e-5))
;; (test "1e-6" (fmt #f 1e-6))
;; (test "1e-7" (fmt #f 1e-7))
;; (test "2e-6" (fmt #f 2e-6))
(test "57005" (fmt #f #xDEAD))
(test "#xDEAD" (fmt #f (radix 16 #xDEAD)))
(test "#xDEAD1234" (fmt #f (radix 16 #xDEAD) 1234))
(test "#xDE.AD" (fmt #f (radix 16 (exact->inexact (/ #xDEAD #x100)))))
(test "#xD.EAD" (fmt #f (radix 16 (exact->inexact (/ #xDEAD #x1000)))))
(test "#x0.DEAD" (fmt #f (radix 16 (exact->inexact (/ #xDEAD #x10000)))))
(test "1G" (fmt #f (radix 17 (num 33))))
(test "1G" (fmt #f (num 33 17)))

(test "3.14159" (fmt #f 3.14159))
(test "3.14" (fmt #f (fix 2 3.14159)))
(test "3.14" (fmt #f (fix 2 3.14)))
(test "3.00" (fmt #f (fix 2 3.)))

(test "(#x11 #x22 #x33)" (fmt #f (radix 16 '(#x11 #x22 #x33))))

(test "299,792,458" (fmt #f (num 299792458 10 #f #f #t)))
(test "299,792,458" (fmt #f (num/comma 299792458)))
(test "299.792.458" (fmt #f (comma-char #\. (num/comma 299792458))))
(test "299.792.458,0" (fmt #f (comma-char #\. (num/comma 299792458.0))))

(test "1.23" (fmt #f (fix 2 (num/fit 4 1.2345))))
(test "1.00" (fmt #f (fix 2 (num/fit 4 1))))
(test "#.##" (fmt #f (fix 2 (num/fit 4 12.345))))

(test "3.9Ki" (fmt #f (num/si 3986)))
(test "4k" (fmt #f (num/si 3986 1000)))
(test "608" (fmt #f (num/si 608)))
(test "3G" (fmt #f (num/si 12345.12355 16)))

;; padding/trimming

(test "abc  " (fmt #f (pad 5 "abc")))
(test "  abc" (fmt #f (pad/left 5 "abc")))
(test " abc " (fmt #f (pad/both 5 "abc")))
(test "abcde" (fmt #f (pad 5 "abcde")))
(test "abcdef" (fmt #f (pad 5 "abcdef")))

(test "abc" (fmt #f (trim 3 "abcde")))
(test "abc" (fmt #f (trim/length 3 "abcde")))
(test "abc" (fmt #f (trim/length 3 "abc\nde")))
(test "cde" (fmt #f (trim/left 3 "abcde")))
(test "bcd" (fmt #f (trim/both 3 "abcde")))

(test "prefix: abc" (fmt #f "prefix: " (trim 3 "abcde")))
(test "prefix: abc" (fmt #f "prefix: " (trim/length 3 "abcde")))
(test "prefix: abc" (fmt #f "prefix: " (trim/length 3 "abc\nde")))
(test "prefix: cde" (fmt #f "prefix: " (trim/left 3 "abcde")))
(test "prefix: bcd" (fmt #f "prefix: " (trim/both 3 "abcde")))

(test "abcde" (fmt #f (ellipses "..." (trim 5 "abcde"))))
(test "ab..." (fmt #f (ellipses "..." (trim 5 "abcdef"))))
(test "abc..." (fmt #f (ellipses "..." (trim 6 "abcdefg"))))
(test "abcde" (fmt #f (ellipses "..." (trim/left 5 "abcde"))))
(test "...ef" (fmt #f (ellipses "..." (trim/left 5 "abcdef"))))
(test "...efg" (fmt #f (ellipses "..." (trim/left 6 "abcdefg"))))
(test "abcdefg" (fmt #f (ellipses "..." (trim/both 7 "abcdefg"))))
(test "...d..." (fmt #f (ellipses "..." (trim/both 7 "abcdefgh"))))
(test "...e..." (fmt #f (ellipses "..." (trim/both 7 "abcdefghi"))))

(test "abc  " (fmt #f (fit 5 "abc")))
(test "  abc" (fmt #f (fit/left 5 "abc")))
(test " abc " (fmt #f (fit/both 5 "abc")))
(test "abcde" (fmt #f (fit 5 "abcde")))
(test "abcde" (fmt #f (fit/left 5 "abcde")))
(test "abcde" (fmt #f (fit/both 5 "abcde")))
(test "abcde" (fmt #f (fit 5 "abcdefgh")))
(test "defgh" (fmt #f (fit/left 5 "abcdefgh")))
(test "cdefg" (fmt #f (fit/both 5 "abcdefgh")))

(test "prefix: abc  " (fmt #f "prefix: " (fit 5 "abc")))
(test "prefix:   abc" (fmt #f "prefix: " (fit/left 5 "abc")))
(test "prefix:  abc " (fmt #f "prefix: " (fit/both 5 "abc")))
(test "prefix: abcde" (fmt #f "prefix: " (fit 5 "abcde")))
(test "prefix: abcde" (fmt #f "prefix: " (fit/left 5 "abcde")))
(test "prefix: abcde" (fmt #f "prefix: " (fit/both 5 "abcde")))
(test "prefix: abcde" (fmt #f "prefix: " (fit 5 "abcdefgh")))
(test "prefix: defgh" (fmt #f "prefix: " (fit/left 5 "abcdefgh")))
(test "prefix: cdefg" (fmt #f "prefix: " (fit/both 5 "abcdefgh")))

(test "abc\n123\n" (fmt #f (join/suffix (cut trim 3 <>) (string-split "abcdef\n123456\n" "\n") nl)))

;; utilities

(test "1 2 3" (fmt #f (join dsp '(1 2 3) " ")))

;; shared structures

(test "#0=(1 . #0#)"
    (fmt #f (wrt (let ((ones (list 1))) (set-cdr! ones ones) ones))))
(test "(0 . #0=(1 . #0#))"
    (fmt #f (wrt (let ((ones (list 1)))
                   (set-cdr! ones ones)
                   (cons 0 ones)))))
(test "(sym . #0=(sym . #0#))"
    (fmt #f (wrt (let ((syms (list 'sym)))
                   (set-cdr! syms syms)
                   (cons 'sym syms)))))
(test "(#0=(1 . #0#) #1=(2 . #1#))"
    (fmt #f (wrt (let ((ones (list 1))
                       (twos (list 2)))
                   (set-cdr! ones ones)
                   (set-cdr! twos twos)
                   (list ones twos)))))

;; without shared detection

(test "(1 1 1 1 1"
    (fmt #f (trim/length
             10
             (wrt/unshared
              (let ((ones (list 1))) (set-cdr! ones ones) ones)))))

(test "(1 1 1 1 1 "
    (fmt #f (trim/length
             11
             (wrt/unshared
              (let ((ones (list 1))) (set-cdr! ones ones) ones)))))

;; pretty printing

(define-macro (test-pretty str)
  (let ((sexp (with-input-from-string str read)))
    `(test ,str (fmt #f (pretty ',sexp)))))

(test-pretty "(foo bar)\n")

(test-pretty
"(abracadabra xylophone
             bananarama
             yellowstonepark
             cryptoanalysis
             zebramania
             delightful
             wubbleflubbery)\n")

(test-pretty
 "#(0  1  2  3  4  5  6  7  8  9  10 11 12 13 14 15 16 17 18 19 20 21 22 23 24
   25 26 27 28 29 30 31 32 33 34 35 36 37)\n")

(test-pretty
 "(0  1  2  3  4  5  6  7  8  9  10 11 12 13 14 15 16 17 18 19 20 21 22 23 24
  25 26 27 28 29 30 31 32 33 34 35 36 37)\n")

(test-pretty
 "(define (fold kons knil ls)
  (define (loop ls acc)
    (if (null? ls) acc (loop (cdr ls) (kons (car ls) acc))))
  (loop ls knil))\n")

(test-pretty
"(do ((vec (make-vector 5)) (i 0 (+ i 1))) ((= i 5) vec) (vector-set! vec i i))\n")

(test-pretty
"(do ((vec (make-vector 5)) (i 0 (+ i 1))) ((= i 5) vec)
  (vector-set! vec i 'supercalifrajalisticexpialidocious))\n")

(test-pretty
"(do ((my-vector (make-vector 5)) (index 0 (+ index 1)))
    ((= index 5) my-vector)
  (vector-set! my-vector index index))\n")

(test-pretty
 "(define (fold kons knil ls)
  (let loop ((ls ls) (acc knil))
    (if (null? ls) acc (loop (cdr ls) (kons (car ls) acc)))))\n")

(test-pretty
 "(define (file->sexp-list pathname)
  (call-with-input-file pathname
    (lambda (port)
      (let loop ((res '()))
        (let ((line (read port)))
          (if (eof-object? line) (reverse res) (loop (cons line res))))))))\n")

(test "(let ((ones '#0=(1 . #0#))) ones)\n"
    (fmt #f (pretty (let ((ones (list 1))) (set-cdr! ones ones) `(let ((ones ',ones)) ones)))))

'(test
"(let ((zeros '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
      (ones '#0=(1 . #0#)))
  (append zeros ones))\n"
    (fmt #f (pretty
             (let ((ones (list 1)))
               (set-cdr! ones ones)
               `(let ((zeros '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
                      (ones ',ones))
                  (append zeros ones))))))

;; slashify

(test "\"note\",\"very simple\",\"csv\",\"writer\",\"\"\"yay!\"\"\""
    (fmt #f (join (lambda (x) (cat "\"" (slashified x #\" #f) "\""))
                  '("note" "very simple" "csv" "writer" "\"yay!\"")
                  ",")))

(test "note,\"very simple\",csv,writer,\"\"\"yay!\"\"\""
    (fmt #f (join (cut maybe-slashified <> char-whitespace? #\" #f)
                  '("note" "very simple" "csv" "writer" "\"yay!\"")
                  ",")))

;; columnar formatting

(test "abc\ndef\n" (fmt #f (fmt-columns (list dsp "abc\ndef\n"))))
(test "abc123\ndef456\n" (fmt #f (fmt-columns (list dsp "abc\ndef\n") (list dsp "123\n456\n"))))
(test "abc123\ndef456\n" (fmt #f (fmt-columns (list dsp "abc\ndef\n") (list dsp "123\n456"))))
(test "abc123\ndef456\n" (fmt #f (fmt-columns (list dsp "abc\ndef") (list dsp "123\n456\n"))))
(test "abc123\ndef456\nghi789\n"
    (fmt #f (fmt-columns (list dsp "abc\ndef\nghi\n") (list dsp "123\n456\n789\n"))))
(test "abc123wuv\ndef456xyz\n"
    (fmt #f (fmt-columns (list dsp "abc\ndef\n") (list dsp "123\n456\n") (list dsp "wuv\nxyz\n"))))
(test "abc  123\ndef  456\n"
    (fmt #f (fmt-columns (list (cut pad/right 5 <>) "abc\ndef\n") (list dsp "123\n456\n"))))
(test "ABC  123\nDEF  456\n"
    (fmt #f (fmt-columns (list (compose upcase (cut pad/right 5 <>)) "abc\ndef\n")
                         (list dsp "123\n456\n"))))
(test "ABC  123\nDEF  456\n"
    (fmt #f (fmt-columns (list (compose (cut pad/right 5 <>) upcase) "abc\ndef\n")
                         (list dsp "123\n456\n"))))

(test "hello\nworld\n" (fmt #f (with-width 8 (wrap-lines "hello world"))))

(test "The fundamental list iterator.
Applies KONS to each element of
LS and the result of the previous
application, beginning with KNIL.
With KONS as CONS and KNIL as '(),
equivalent to REVERSE.
"
    (fmt #f (with-width 36 (wrap-lines "The fundamental list iterator.  Applies KONS to each element of LS and the result of the previous application, beginning with KNIL.  With KONS as CONS and KNIL as '(), equivalent to REVERSE."))))

(test
"The   fundamental   list   iterator.
Applies  KONS  to  each  element  of
LS  and  the  result of the previous
application,  beginning  with  KNIL.
With  KONS  as CONS and KNIL as '(),
equivalent to REVERSE.
"
    (fmt #f (with-width 36 (justify "The fundamental list iterator.  Applies KONS to each element of LS and the result of the previous application, beginning with KNIL.  With KONS as CONS and KNIL as '(), equivalent to REVERSE."))))

(test
"(define (fold kons knil ls)          ; The fundamental list iterator.
  (let lp ((ls ls) (acc knil))       ; Applies KONS to each element of
    (if (null? ls)                   ; LS and the result of the previous
        acc                          ; application, beginning with KNIL.
        (lp (cdr ls)                 ; With KONS as CONS and KNIL as '(),
            (kons (car ls) acc)))))  ; equivalent to REVERSE.
"
    (fmt #f (fmt-columns
             (list
              (cut pad/right 36 <>)
              (with-width 36
                (pretty '(define (fold kons knil ls)
                           (let lp ((ls ls) (acc knil))
                             (if (null? ls)
                                 acc
                                 (lp (cdr ls)
                                     (kons (car ls) acc))))))))
             (list
              (cut cat " ; " <>)
              (with-width 36
                (wrap-lines "The fundamental list iterator.  Applies KONS to each element of LS and the result of the previous application, beginning with KNIL.  With KONS as CONS and KNIL as '(), equivalent to REVERSE."))))))

(test
"(define (fold kons knil ls)          ; The fundamental list iterator.
  (let lp ((ls ls) (acc knil))       ; Applies KONS to each element of
    (if (null? ls)                   ; LS and the result of the previous
        acc                          ; application, beginning with KNIL.
        (lp (cdr ls)                 ; With KONS as CONS and KNIL as '(),
            (kons (car ls) acc)))))  ; equivalent to REVERSE.
"
    (fmt #f (with-width 76
              (columnar
               (pretty '(define (fold kons knil ls)
                          (let lp ((ls ls) (acc knil))
                            (if (null? ls)
                                acc
                                (lp (cdr ls)
                                    (kons (car ls) acc))))))
               " ; "
               (wrap-lines "The fundamental list iterator.  Applies KONS to each element of LS and the result of the previous application, beginning with KNIL.  With KONS as CONS and KNIL as '(), equivalent to REVERSE.")))))


;; misc extras
#|
(define (string-hide-passwords str)
  (string-substitute (regexp "(pass(?:w(?:or)?d)?\\s?[:=>]\\s+)\\S+" #t)
                     "\\1******"
                     str
                     #t))

(define hide-passwords
  (make-string-fmt-transformer string-hide-passwords))

(define (string-mangle-email str)
  (string-substitute
   (regexp "\\b([-+.\\w]+)@((?:[-+\\w]+\\.)+[a-z]{2,4})\\b" #t)
   "\\1 _at_ \\2"
   str
   #t))

(define mangle-email
  (make-string-fmt-transformer string-mangle-email))
|#
(test-end)

