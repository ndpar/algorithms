#lang racket

(define (square x)
  (* x x))

; Exercise 1.16, p.46
(define (expt x n)
  (define (iter a x n)
    (cond ((= n 0) a)
          ((even? n) (iter a (* x x) (/ n 2)))
          (else (iter (* a x) x (- n 1)))))
  (iter 1 x n))

; Exercise 1.17, p.46
(define (double n) (+ n n))
(define (halve n) (quotient n 2))

(define (recur-mult m n)
  (cond ((= n 0) 0)
        ((= n 1) m)
        ((even? n) (recur-mult (double m) (halve n)))
        (else (+ m (recur-mult m (- n 1))))))

; Exercise 1.18, p.47
(define (russian m n)
  (define (iter a m n)
    (cond ((= n 0) a)
          ((even? n) (iter a (double m) (halve n)))
          (else (iter (+ a m) m (- n 1)))))
  (iter 0 m n))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (define (next td)
    (if (= td 2) 3 (+ td 2)))
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= (smallest-divisor n) n))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

; Exercise 1.22, p.54
(define (search-for-primes n count)
  (cond ((= count 0) (newline) (display "done"))
        ((even? n) (search-for-primes (+ n 1) count))
        ((fast-prime? n 3) (timed-prime-test n)
                           (search-for-primes (+ n 1) (- count 1)))
        (else (search-for-primes (+ n 1) count))))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (current-inexact-milliseconds)))

(define (start-prime-test n start-time)
  (cond ((prime? n)
      (report-prime (- (current-inexact-milliseconds) start-time)))))

(define (report-prime elapsed-time)
  (display " ** ")
  (display elapsed-time))
