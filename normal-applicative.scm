#lang racket

; Exercise 1.5, p.21
; to check if language is lazy
; run (bitdiddle-test (bitdiddle-p))
(define (bitdiddle-p) (bitdiddle-p))
(define (bitdiddle-test x y)
  (if (= x 0) 0 y))

; proof of 'if' being spepcial form
; run (if-test 0 0)
(define (if-test x y)
  (if (= x 0) 0 (/ x y)))

; Exercise 1.6, p.25
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

; cond-implementation of 'if' is not special form
; run (new-if-test 0 0)
(define (new-if-test x y)
  (new-if (= x 0) 0 (/ x y)))
