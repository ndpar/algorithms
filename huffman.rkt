#lang racket

;; -------------------------------------------------------------------
;; Huffman Encoding Trees
;; Adapted from SICP, chapter 2.3.4
;; https://github.com/CompSciCabal/SMRTYPRTY/blob/master/sicp/chapter-2.3/ndpar-2.3.4.scm
;; -------------------------------------------------------------------

;; Tree representation

(struct leaf (symbol weight) #:transparent)

(struct tree (left right symbols weight) #:transparent)

;; Generic procedures

(define (make-tree left right)
  (tree left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (symbols node)
  (if (leaf? node)
      (list (leaf-symbol node))
      (tree-symbols node)))

(define (weight node)
  (if (leaf? node)
      (leaf-weight node)
      (tree-weight node)))

;; Building Huffman trees

(require (prefix-in h: data/heap))

(define ((& f g) x y) ; J compose
  (f (g x) (g y)))

(define-syntax-rule (while test body ...)
  (let loop () (when test body ... (loop))))

(define (leaves->huffman-tree leaves)
  (define (pop! heap)
    (let ([min (h:heap-min heap)]) (h:heap-remove-min! heap) min))
  (let ([heap (h:make-heap (& <= weight))])
    (h:heap-add-all! heap leaves)
    (while (< 1 (h:heap-count heap))
      (h:heap-add! heap (make-tree (pop! heap) (pop! heap))))
    (h:heap-min heap)))

(define (map-apply f lists)
  (map (curry apply f) lists))

(define (make-huffman-tree frequencies)
  (leaves->huffman-tree (map-apply leaf frequencies)))

(define sample-tree
  (make-huffman-tree '((A 4) (B 2) (D 1) (C 1))))

(module+ test
  (require rackunit)
  (check-equal?
   sample-tree
   (tree (leaf 'A 4) (tree (leaf 'B 2) (tree (leaf 'D 1) (leaf 'C 1) '(D C) 2) '(B D C) 4) '(A B D C) 8)))

;; Decoding messages

(define (decode tree bits)
  (define (choose-branch bit branch)
    (cond [(= 0 bit) (tree-left branch)]
          [(= 1 bit) (tree-right branch)]
          [else (error "Bad bit" bit)]))
  (let iter ([bits bits] [current-branch tree])
    (if (null? bits)
        '()
        (let ([next-branch (choose-branch (car bits) current-branch)])
          (if (leaf? next-branch)
              (cons (leaf-symbol next-branch)
                    (iter (cdr bits) tree))
              (iter (cdr bits) next-branch))))))

;; Encoding messages

(define (encode tree message)
  (append-map (curry encode-symbol tree) message))

(define (encode-symbol tree symbol)
  (let iter ([result '()] [tree tree])
    (if (leaf? tree)
        (if (eq? symbol (leaf-symbol tree))
            (reverse result)
            (error "Unknown symbol" symbol))
        ; If the symbol is not in the left branch
        ; it has to be in the right one by tree construction
        (if (member symbol (symbols (tree-left tree)))
            (iter (cons 0 result) (tree-left tree))
            (iter (cons 1 result) (tree-right tree))))))

;; Tests

(define sample-message '(A D A B B C A))
(define sample-bits '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(module+ test
  (check-equal? (decode sample-tree sample-bits) sample-message)
  (check-equal? (encode sample-tree sample-message) sample-bits))
