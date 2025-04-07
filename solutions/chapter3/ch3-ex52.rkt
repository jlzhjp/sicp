#lang racket/base

(module+ test
  (require rackunit/text-ui
           akari-sicp/lib/testing
           akari-sicp/lib/stream
           "ch3-ex50.rkt")

  (run-tests
   (describe "Exercise 3.52"
     (it "with memo optimization"
       (define sum 0)
       (define (accum x)
         (set! sum (+ x sum))
         sum)

       (define seq
         (stream-map accum
                     (stream-enumerate-interval 1 20)))
       ;; 1, 3, 6, 10, 15, 21, 28, 36, 45, 55,
       ;; 66, 78, 91, 105, 120, 136, 153, 171, 190, 210

       (expect [sum => 1 "the first element should be evaluted after initialization"])

       (define y (stream-filter even? seq))

       (expect [sum => 6 "the first even number is 6"])

       (define z
         (stream-filter (lambda (x) (= (remainder x 5) 0))
                        seq))

       (expect [sum => 10 "the first number divisible by 5 is 10"])

       (expect [(stream-ref y 7) => 136 "the 7th even number is 136"]
               [sum => 136])

       (expect [(display-stream z) =$> '("10" "15" "45" "55" "105" "120" "190" "210")]))

     (it "without memo optimization"
       (parameterize ([stream-memo? #f])
         (define sum 0)
         (define (accum x) (set! sum (+ x sum)) sum)

         ;      1
         ; sum: 0
         ; seq: 1
         (define seq
           (stream-map accum
                       (stream-enumerate-interval 1 20)))

         (expect [sum => 1])

         ;        2 3
         ; sum:   1 3
         ; seq: 1 3 6
         ;   y: _ _ 6
         (define y (stream-filter even? seq))

         (expect [sum => 6])

         (define z
           (stream-filter (lambda (x) (= (remainder x 5) 0))
                          seq))

         ;        2  3  4 ...
         ; sum:   6  8 11 ...
         ; seq: 1 8 11 15 ...
         ;   z: _ _  _ 15 ...
         (expect [sum => 15])

         ;         4  5  6  7  8  9 10 11 12  13  14  15  16  17
         ; sum:   15 19 24 30 37 45 54 64 75  87 100 114 129 145
         ; seq:   19 24 30 37 45 54 64 75 87 100 114 129 145 162
         ;   y: 6  _ 24 30  _ _  54 64  _  _ 100 114   _   _ 162
         (expect [(stream-ref y 7) => 162]
                 [sum => 162])

         ;           5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20
         ; sum:    162 167 173 180 188 197 207 218 230 243 257 272 288 305 323 342
         ; seq:    167 173 180 188 197 207 218 230 243 257 272 288 305 323 342 362
         ;   z: 15   _   _ 180   _   _   _   _ 230   _   _   _   _ 305   _   _   _
         (expect [(display-stream z) =$> '("15" "180" "230" "305")]))))))