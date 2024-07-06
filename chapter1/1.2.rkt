#lang sicp

(define (first-denomination kinds-of-coins)
  (cond [(= kinds-of-coins 1) 1]
        [(= kinds-of-coins 2) 5]
        [(= kinds-of-coins 3) 10]
        [(= kinds-of-coins 4) 25]
        [(= kinds-of-coins 5) 50]))

(define (cc amount kinds-of-coins)
  (cond [(= amount 0) 1]
        [(or (< amount 0) (= kinds-of-coins 0)) 0]
        [else (+ (cc amount (- kinds-of-coins 1))
                 (cc (- amount (first-denomination kinds-of-coins)) kinds-of-coins))]))

(define (count-change amount)
  (cc amount 5))

(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

(define (expt* b n)
  (define (expt-iter b counter product)
    (if (= counter 0)
        product
        (expt-iter b (- counter 1) (* b product))))
  (expt-iter b n 1))

(define (square x) (* x x))

(define (fast-expt b n)
  (cond [(= n 0) 1]
        [(even? n) (square (fast-expt b (/ n 2)))]
        [else (* b (fast-expt b (- n 1)))]))

(define (fast-expt* b n)
  (define (fast-expt-iter a b n)
    (cond [(= n 0) a]
          [(even? n) (fast-expt-iter a (square b) (/ n 2))]
          [else (fast-expt-iter (* a b) b (- n 1))]))

  (fast-expt-iter 1 b n))

(define (mul a b)
  (if (= b 0)
      0
      (+ a (mul a (- b 1)))))

(define (double x) (* 2 x))
(define (halve x) (/ x 2))

(define (fast-mul a b)
  (cond [(= b 1) a]
        [(even? b) (double (fast-mul a (halve b)))]
        [else (+ a (fast-mul a (- b 1)))]))

(define (fast-mul* a b)
  (define (fast-mul-iter a b c)
    (cond [(= b 0) c]
          [(even? b) (fast-mul-iter (double a) (halve b) c)]
          [else (fast-mul-iter a (- b 1) (+ c a))]))
  (fast-mul-iter a b 0))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (fib n)
  (define (fib-iter a b p q count)
    (cond [(= count 0) b]
          [(even? count) (fib-iter a
                                   b
                                   (+ (square p) (square q))
                                   (+ (square q) (* 2 p q))
                                   (/ count 2))]
          [else (fib-iter (+ (* b q) (* a q) (* a p))
                          (+ (* b p) (* a q))
                          p
                          q
                          (- count 1))]))
  (fib-iter 1 0 0 1 n))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (divides? a  b)
  (= (remainder b a) 0))

(define (find-divisor n test-divisor)
  (cond [(> (square test-divisor) n) n]
        [(divides? test-divisor n) test-divisor]
        [else (find-divisor n (+ test-divisor 1))]))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (expmod base exp m)
  (cond [(= exp 0) 1]
        [(even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m)]
        [else
         (remainder (* base (expmod base (- exp 1) m))
                    m)]))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond [(= times 0) true]
        [(fermat-test n) (fast-prime? n (- times 1))]
        [else false]))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time)) #f))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time)
  #t)

(define (search-for-primes n)
  (if (even? n)
      (search-for-primes (+ n 1))
      (if (not (timed-prime-test n))
          (search-for-primes (+ n 2)))) )

; (search-for-primes 100000)
; (search-for-primes 1000000)
; (search-for-primes 10000000)

(define (full-fermat-test n)
  (define (full-fermat-test-iter a)
    (cond [(= a 1) #t]
          [(not (= (expmod a n n) (remainder a n))) #f]
          [else (full-fermat-test-iter (- a 1))]))
  (full-fermat-test-iter (- n 1)))

(define (run-full-fermat-test-on-carmichael-numbers)
  (display (full-fermat-test 561))
  (newline)
  (display (full-fermat-test 1105))
  (newline)
  (display (full-fermat-test 1729))
  (newline)
  (display (full-fermat-test 2465))
  (newline)
  (display (full-fermat-test 2821))
  (newline)
  (display (full-fermat-test 6601))
  (newline))
