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

(define (node<=? x y)
  (<= (weight x) (weight y)))

(define-syntax-rule (while test body ...)
  (let loop () (when test body ... (loop))))

(define (leaf-vector->huffman-tree leaves)
  (define (pop! heap)
    (let ([min (h:heap-min heap)]) (h:heap-remove-min! heap) min))
  (let ([heap (h:vector->heap node<=? leaves)])
    (while (< 1 (h:heap-count heap))
      (h:heap-add! heap (make-tree (pop! heap) (pop! heap))))
    (h:heap-min heap)))

(define sample-tree
  (leaf-vector->huffman-tree (vector (leaf 'A 4) (leaf 'B 2) (leaf 'C 1) (leaf 'D 1) )))

(module+ test
  (require rackunit)
  (check-equal?
   sample-tree
   (tree (leaf 'A 4) (tree (leaf 'B 2) (tree (leaf 'D 1) (leaf 'C 1) '(D C) 2) '(B D C) 4) '(A B D C) 8)))

;; Decoding messages

(define (decode bits tree)
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

(define (encode-symbol symbol tree)
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

(define (flatmap f seq)
  (apply append (map f seq)))

(define (encode message tree)
  (flatmap (λ (s) (encode-symbol s tree)) message))

;; Tests

(define sample-message '(A D A B B C A))
(define sample-cipher '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(module+ test
  (check-equal?
   (decode sample-cipher sample-tree)
   sample-message))

(module+ test
  (check-equal?
   (encode sample-message sample-tree)
   sample-cipher))
