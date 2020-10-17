#lang racket

(require "tagged-data.rkt")
(require "vector.rkt")

(provide 2darray->list
         list->2darray
         empty-2darray
         is-2darray?
         fold-2d
         map-2d
         enum-2d
         get-enum-2d-value
         get-enum-2d-index
         2darray-ref
         2darray-vector-ref
         get-dimensions-2darray
         rotate-2darray)
         

; 2darray -> (Int, Int)
(define (get-dimensions-2darray 2darray)
  (car (get-value 2darray)))


; 2darray -> list
(define (2darray->list 2darray)
 (if (is-2darray? 2darray)
     (cdr (get-value 2darray))
     (raise-type-error '2darray->string "2darray" 2darray)))

; Int -> Int -> 2darray
(define (empty-2darray n m)
  ; returns an empty n x m 2darray
  (list->2darray (replicate n (replicate m 'Empty))))

; tagged data -> bool 
(define (is-2darray? 2darray)
  (eq? (get-tag 2darray) '2darray))

; [[a]] -> 2darray a 
(define (list->2darray contents)
  (let ((n-rows (length contents))
        (n-cols (length (car contents)))
        (row-lengths (map length contents)))
    (if (andmap (curry = n-cols) row-lengths)
        (tag-as '2darray (cons (make-vector n-rows n-cols) contents))
        (raise-arguments-error 'list->2darray
                               "array has mismatched row lengths"
                               "first row" n-cols
                               "other rows" (cdr row-lengths)))))

; 2darray a -> 2darray ((Int,Int), a)
(define (enum-2d 2darray)
  ; enum-2d takes a 2darray and with each item pairs to it a tuple with its matrix indices
  (let* ([as-list   (2darray->list 2darray)]
         [with-cols (map (curry enumerate-tag 'col) as-list)]
         [with-rows (enumerate-tag 'row with-cols)])
    (list->2darray
     (map
      (lambda (row)
        (let ([i (get-index row)])
          (map
           (lambda (cell)
             (let ([j (get-index cell)])
               (tag-as (make-vector i j) (get-enum-value cell))))
           (get-enum-value row))))
      with-rows))))

; 2darray -> 2darray
(define (rotate-2darray 2darray)
  ; Rotates a 2darray 90 degrees
  (let* ([old-dims (get-dimensions-2darray 2darray)]
         [n        (get-y old-dims)]
         [m        (get-x old-dims)]
         [blank    (enum-2d (empty-2darray n m))])
    (map-2d
     (lambda (item)
       (let* ([indices (get-tag item)]
              [i       (get-x indices)]
              [j       (get-y indices)])
         (2darray-ref  2darray (- m j 1) i)))
     blank)))

; (vector, a) -> a
(define get-enum-2d-value get-enum-value)

; (vector, a) -> vector
(define get-enum-2d-index car)

; 2darray -> 2darray
(define (map-2d fn 2darray)
  ; map-2d maps a function across a 2darray
  (let ([as-list   (2darray->list 2darray)])
    (list->2darray (map (curry map fn) as-list))))

; fold-2d folds a 2darray
(define (fold-2d proc init 2darray)
  (define flat1 ; flatten a list of lists to depth-1 only
    (curry foldl append '()))
  (foldl proc init (flat1 (2darray->list 2darray))))

; 2darray a > Int -> Int -> a
(define (2darray-ref 2darray i j)
  (let* ([dims    (get-dimensions-2darray 2darray)]
         [i-max (- (get-x dims) 1)]
         [j-max (- (get-y dims) 1)])
    (cond [(> i i-max) (raise-range-error '2darray-ref "2darray" "x " i 2darray 0 i-max)]
          [(> j j-max) (raise-range-error '2darray-ref "2darray" "y " j 2darray 0 j-max)]
          [else (list-ref (list-ref (cddr 2darray) i) j)]
          )))

; 2darray a -> vector -> a
(define (2darray-vector-ref 2darray v)
  (let ([i (get-x v)]
        [j (get-y v)])
    (2darray-ref 2darray i j)))