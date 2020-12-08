#lang racket
(require 2htdp/image)
(require "2darray.rkt")
(require "tagged-data.rkt")
(require "tetramino.rkt")
(require "vector.rkt")

(provide can-move-to?
         place-tetramino
         clear-full-rows
         board->image
         empty-board)

; global constants

; Int
(define board-width 10)

; Int
(define board-height 20)

; Int
; grid size in pixels
(define grid-size 30)

; cell -> symbol
(define cell-colour get-value)

; cell / row -> bool
(define (is-row? row)
  (eq? (get-enum-tag row) 'row))

; cell -> bool
(define (is-empty? cell)
  (eq? 'Empty
       (get-enum-value cell)))

; vector -> colour -> cell
(define (make-cell loc colour)
  (cons loc colour))

; board -> string
(define (board->string board)
  ; cell -> char
  (define (cell->char cell)
    (cond [(is-empty? cell) #\_]
          [else #\#]))
  (string-join (map list->string (2darray->list (map-2d cell->char board))) "\n"))

; board -> void (with side effect)
(define (show-board board)
  (display (board->string board)))

; board (enumerated 2darray)
(define empty-board
  (enum-2d (empty-2darray board-height board-width)))

; place-tetramino :: (int,int) -> tetramino -> board -> board
(define (place-tetramino loc tetramino board)
  ; no checking if we can place here, just place the piece
  ; loc should be a vector consisting of two ints in the following coordinate system
  ; +----> (., y)
  ; |
  ; |
  ; V (x,.) where 0,0 is the top left of the board
  ; tetraminos are arrays with a certain extent and rotation
  (define (in-range? min max x)
    (and (<= min x) (> max x)))
  (let* ((new-2darray (get-mask tetramino))
         (new-dims    (get-dimensions  new-2darray))
         (i-min       (get-x loc))
         (i-max       (+ i-min (get-x new-dims)))
         (j-min       (get-y loc))
         (j-max       (+ j-min (get-y new-dims)))
         (new-colour  (get-colour  tetramino)))
    (map-2d (lambda (cell)
              (let* ([loc (get-tag cell)]
                     [i   (get-x loc)]
                     [j   (get-y loc)])
                (if (and (in-range? i-min i-max i)
                         (in-range? j-min j-max j)
                         (tetramino-ref tetramino
                                        (- i i-min)
                                        (- j j-min)))
                    ; cell occupied by tetramino in play, so fill the cell
                    (make-cell loc new-colour)
                    ; cell not occupied by tetramino in play, so leave unchanged
                    cell)))
            board)))


; tetramino -> (Int,Int) -> board -> Bool
(define (can-move-to? tetramino loc board)
  ; Returns false if a tetramino collides with either
  ;    1. Another tetramino
  ;    2. The "floor" of the board
  ;    3. The "walls" of the board
  (let* ([tetra-dims   (get-dimensions tetramino)]
         [tetra-height (get-y tetra-dims)]
         [tetra-width  (get-x tetra-dims)]
         [tetra-min-x  (get-x loc)]
         [tetra-min-y  (get-y loc)]
         [tetra-max-x  (+ tetra-min-x tetra-width)]
         [tetra-max-y  (+ tetra-min-y tetra-height)])
    
    (andmap-2d (lambda (cell)
                 (let ([occupied? (get-enum-2d-value cell)])
                   (or (not occupied?)
                       (let* ([cell-loc (add-vector loc (get-enum-2d-index cell))]
                              [cell-x   (get-x cell-loc)]
                              [cell-y   (get-y cell-loc)])

                         ; we have to check if the cell is on the board first, or else
                         ; there will be an error when we try to check what is the contents
                         ; of a cell outside the bounds of the board
                         (and (>= cell-x 0)
                              (< cell-x board-height) ; check if in floor
                              (>= cell-y 0)            ; check if in walls
                              (< cell-y board-width)
                              ; check if there is already a square in this cell on the board
                              (is-empty? (2darray-ref board cell-x cell-y)))))))
               ; Doesn't really make much sense to run enum-2d every tick. - extra overhead.
               ; Include the enumeration in the representation of the tetramino mask
               (enum-2d (get-mask tetramino)))))

; board -> image
(define (board->image board)
  ; cell -> image -> image
  (define (draw-cell cell scene)
    (if (is-empty? cell)
        scene ; nothing to do
        (let* ([indices (get-tag cell)]
               [x-coord (+ (/ grid-size 2)
                           (* (get-x indices) grid-size))]
               [y-coord (+ (/ grid-size 2)
                           (* (get-y indices) grid-size))]
               [colour  (symbol->string (get-enum-2d-value cell))])
          (place-image (square grid-size "solid" colour)
                       y-coord
                       x-coord
                       scene))))
  (let* ([image-h (* board-height grid-size)]
         [image-w (* board-width  grid-size)]
         [canvas (empty-scene image-w image-h)])
    (fold-2d draw-cell canvas board)))

; board -> (board, Int)
(define (clear-full-rows board)
  ; Takes a board, then clears full rows and returns the new board
  ; along with the points gained due to the clears
  ; Int -> Int -> board -> board

  (define (clear-rows i-min i-max board)
    ; remove rows between i-min and i-max
    (define (move-cell index-vector cell)
      (make-cell index-vector (get-enum-2d-value cell)))
           (let ([i-range (- i-max i-min)])
             (map-2d (lambda (cell)
                       (let* ([indices (get-enum-2d-index cell)]
                              [this-i  (get-x indices)]
                              [this-j  (get-y indices)])
                         (cond [(<  this-i i-range) (make-cell indices 'Empty)]
                               [(<  this-i i-max)
                                (move-cell indices (2darray-ref board (- this-i i-range) this-j))]
                               [else
                                cell
                                ])))
                     board)))
  ; board -> [(Int,Int)]
  (define (full-rows board)
    ; Find all filled rows on a board, return a list of tuples
    ; with the indices of the first and last rows in a block of
    ; filled rows
    (define is-full? pair?)
    (define (build-pair-list index row-full? xs)
      (define (partial-pair second-index this-row-full?)
        (if this-row-full?
            partial-pair
            (cons index second-index)))
      (cond [(null? xs) (if row-full?
                            (list partial-pair)
                            '())]
            [(is-full? (car xs)) (if row-full?
                                     (cons partial-pair xs)
                                     xs)]
            [else (cons ((car xs) index row-full?) (cdr xs))]))
  
    (let* ([rows-full (enumerate-tag 'row (map
                                           (curry andmap (compose not is-empty?))
                                           (2darray->list board)))]
           [my-list (foldl (lambda (row pair-list)
                             (let ([i         (get-index row)]
                                   [row-full? (get-value row)])
                               (build-pair-list i row-full? pair-list)))
                           '()
                           rows-full)])
      (cond [(null? my-list) '()]
                          [(is-full? (car my-list)) my-list]
                          [else (cons ((car my-list) board-height #f)
                                      (cdr my-list))])))
            

  (foldl
   (lambda (index-pair state)
     (let* ([current-board    (car state)]
            [current-score    (cdr state)]
            [i-min            (car  index-pair)]
            [i-max            (cdr index-pair)]
            [num-rows-cleared (- i-max i-min)])
       (cons (clear-rows i-min i-max current-board)
             (+ num-rows-cleared current-score))))
   (cons board 0)
   (full-rows board)))