#lang racket
(require "tagged-data.rkt")
(require "2darray.rkt")
(require "vector.rkt")

(provide get-dimensions
         get-mask
         tetramino-ref
         get-colour
         rotate-tetramino
         tetraminos
         get-random-tetramino
         )

; colour -> [[symbol]] -> tetramino
(define (make-tetramino colour 2darray-mask)
  (tag-as 'tetramino
          (cons colour 2darray-mask)))

; tetramino -> [[symbol]]
(define (get-mask tetramino)
  (if (is-tetramino? tetramino)
      (cdr (get-value tetramino))
      (raise-type-error 'get-mask "tetramino" tetramino)))

; 2darray -> 2darray
(define (bool-mask symbols)
  (define (replacer symbol)
    (cond [(eq? symbol 'X) #t]
          [(eq? symbol '_) #f]))
  (list->2darray (map (curry map replacer) symbols)))

; 2darray/tetramino -> (Int, Int)
(define (get-dimensions x)
  (cond [(is-2darray? x) (get-dimensions-2darray x)]
        [(is-tetramino? x) (get-dimensions-2darray (get-mask x))]
        [else (raise-type-error 'get-dimensions "2darray/tetramino" x)]))

; tetraminio -> Bool
(define (tetramino-ref tetramino i j)
  (2darray-ref (get-mask tetramino) i j))

; tetramino -> colour
(define (get-colour tetramino)
  (if (is-tetramino? tetramino)
      (car (get-value tetramino))
      (raise-type-error 'get-colour "tetramino" tetramino)))

; tetramino/cell/row -> bool
(define (is-tetramino? tetramino)
  (eq? (get-tag tetramino) 'tetramino))

; tetramino -> tetramino
(define (rotate-tetramino tetramino)
  ; Rotates a tetramino 90 degrees
  (let ([colour       (get-colour tetramino)]
        [mask-2darray (get-mask tetramino)])
  (make-tetramino colour (rotate-2darray mask-2darray))))
  
; [symbol]
(define tetraminos '(O L J S Z T I))

; symbol -> tetramino
(define (get-tetramino name)
  (define O (make-tetramino 'Yellow (bool-mask '((X X)
                                                 (X X)))))
  
  (define L (make-tetramino 'Orange (bool-mask '((_ _ X)
                                                 (X X X)))))
  
  (define J (make-tetramino 'Blue   (bool-mask '((X X X)
                                                 (_ _ X)))))
  
  (define S (make-tetramino 'Green  (bool-mask '((_ X X)
                                                 (X X _)))))

  (define Z (make-tetramino 'Red    (bool-mask '((X X _)
                                                 (_ X X)))))
  
  (define T (make-tetramino 'Purple (bool-mask '((X X X)
                                                 (_ X _)))))
  
  (define I (make-tetramino 'Cyan   (bool-mask '((X X X X)))))
  
  (cond ((eq? name 'O) O)   ; square
        ((eq? name 'L) L)   ; L
        ((eq? name 'J) J)   ; reverse L
        ((eq? name 'S) S)   ; zig zag
        ((eq? name 'Z) Z)   ; zag zig
        ((eq? name 'T) T)   ; T piece
        ((eq? name 'I) I))) ; long straight piece

; void -> tetramino
(define (get-random-tetramino)
  ; [a] -> a
  (define (random-choice mylist)
    ; Choose random element from list
    (list-ref mylist (random 0 (length mylist))))
  (get-tetramino (random-choice tetraminos)))
  


