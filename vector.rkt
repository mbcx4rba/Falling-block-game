#lang racket
(require "tagged-data.rkt")

(provide add-vector
         make-vector
         get-x
         get-y)


; number -> number -> vector 
(define (make-vector x y)
  (tag-as 'Vector (cons x y)))

; vector -> vector -> vector
(define (add-vector v1 v2)
  (let ([x1 (get-x v1)]
        [y1 (get-y v1)]
        [x2 (get-x v2)]
        [y2 (get-y v2)])
    (make-vector (+ x1 x2) (+ y1 y2))))
    
; tagged data -> Bool
(define (is-vector? v)
  (eq? (get-tag v) 'Vector))

; vector -> number
(define (get-x v)
  (if (is-vector? v)
      (car (get-value v))
      (raise-type-error 'get-x "Vector" v)))

; vector -> number
(define (get-y v)
  (if (is-vector? v)
      (cdr (get-value v))
      (raise-type-error 'get-y "Vector" v)))