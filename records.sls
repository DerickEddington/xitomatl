;;; Copyright (c) 2008 Derick Eddington
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;;
;;; Except as contained in this notice, the name(s) of the above copyright
;;; holders shall not be used in advertising or otherwise to promote the sale,
;;; use or other dealings in this Software without prior written authorization.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

#!r6rs
(library (xitomatl records)
  (export
    record-type-accessors
    record-type-mutators)
  (import
    (rnrs)
    (only (xitomatl define) define/?)
    (only (xitomatl srfi vectors) vector-concatenate))
  
  (define (record-type-field-procs rtd field-proc)
    (let loop ([rtd rtd] [procs '()])
      (if rtd
        (loop (record-type-parent rtd)
              (cons (let ([len (vector-length (record-type-field-names rtd))])
                      (let loop ([i (- len 1)] [v (make-vector len)])
                        (cond [(negative? i) v]
                              [else (vector-set! v i (field-proc rtd i))
                                    (loop (- i 1) v)])))
                    procs))
        (vector-concatenate procs))))
  
  (define/? (record-type-accessors [rtd record-type-descriptor?])
    (record-type-field-procs rtd record-accessor))
  
  (define/? (record-type-mutators [rtd record-type-descriptor?])
    (record-type-field-procs rtd (lambda (rtd i)
                                   (and (record-field-mutable? rtd i)
                                        (record-mutator rtd i)))))
  
)
