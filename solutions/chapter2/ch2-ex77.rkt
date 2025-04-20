#lang racket/base

(require akari-sicp/lib/numeric-tower)

(require racket/pretty)

(install-polar-package)
(install-rectangular-package)
(install-complex-package)

(define z (make-complex-from-real-imag 3 4))

(module+ test
  (require akari-sicp/lib/testing
           akari-sicp/lib/common)

  (run-tests
   (describe "exercise 2.77"
     (it "should raise error"
       (expect [(magnitude z) =!> #rx"no method for these types"]))
     (it "should works"
       (put 'real-part '(complex) real-part)
       (put 'imag-part '(complex) imag-part)
       (put 'magnitude '(complex) magnitude)
       (put 'angle '(complex) angle)

       (expect
        [(magnitude z) ~> (sqrt (+ (square (real-part z))
                                   (square (imag-part z))))])))))
