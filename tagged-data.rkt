#lang racket

(provide enumerate-tag
         get-index
         get-value
         get-enum-tag
         get-enum-value
         tag-as
         get-tag
         replicate)

; data -> tagged data
(define (enumerate-tag tag xs)
  (define (range start stop)
  (if (> start stop)
      '()
      (cons start (range (+ 1 start) stop))))
  (define (zip x y) (map cons x y))
  (zip
   (map (curry cons tag) (range 0 (- (length xs) 1))) xs))

; tagged-data -> Int
(define (get-index enum-item)
  (cdr (get-tag enum-item)))

; tagged-data -> Symbol
(define (get-enum-tag enum-item)
  (car (get-tag enum-item)))

; tagged-data a -> a
(define get-enum-value cdr)

; symbol -> a -> (symbol, a)
(define (tag-as tag item)
  (cons tag item))

; forall a. (symbol, a) -> symbol
(define get-tag car)

; forall a. (symbol, a) -> a
(define get-value cdr)

; Int -> a -> [a]
(define (replicate n a)
  (if (= n 0)
      '()
      (cons a (replicate (- n 1) a))))
