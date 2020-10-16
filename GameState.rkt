#lang racket
(require 2htdp/image)
(require 2htdp/universe)

; global constants

; Int
(define board-width 10)

; Int
(define board-height 20)

; Int
; grid size in pixels
(define grid-size 35)


; (a -> Bool) -> [a] -> Bool
(define (all? fn my-list)
  (foldl (lambda (x bs) (and bs (fn x))) #t my-list)) ; should use andmap

; data -> tagged data
(define (enumerate-tag tag xs)
  (define (range start stop)
  (if (> start stop)
      '()
      (cons start (range (+ 1 start) stop))))
  (define (zip x y) (map cons x y))
  (zip
   (map (curry cons tag) (range 0 (- (length xs) 1))) xs))

; tagged data -> Int
(define (get-index enum-item)
  (cdr (get-tag enum-item)))

; tagged data -> Symbol
(define (get-enum-tag enum-item)
  (car (get-tag enum-item)))

(define get-enum-value cdr)

; Int -> a -> [a]
(define (replicate n a)
  (if (= n 0)
      '()
      (cons a (replicate (- n 1) a))))

; symbol -> a -> (symbol, a)
(define (tag-as tag item)
  (cons tag item))

; forall a. (symbol, a) -> symbol
(define get-tag car)

; forall a. (symbol, a) -> a
(define get-value cdr)

; cell -> symbol
(define cell-colour get-value)

; number -> number -> vector 
(define (make-vector x y)
  (tag-as 'Vector (cons x y)))

; 2darray -> list
(define (2darray->list 2darray)
 (if (is-2darray? 2darray)
     (cdr (get-value 2darray))
     (raise-type-error '2darray->string "2darray" 2darray)))

; Int -> Int -> 2darray
(define (empty-2darray n m)
  ; returns an empty n x m 2darray
  (make-2darray (replicate n (replicate m 'Empty))))

; tagged data -> bool 
(define (is-2darray? 2darray)
  (eq? (get-tag 2darray) '2darray))

; [[a]] -> 2darray a 
(define (make-2darray contents)
  (let ((n-rows (length contents))
        (n-cols (length (car contents)))
        (row-lengths (map length contents)))
    (if (all? (curry = n-cols) row-lengths)
        (tag-as '2darray (cons (make-vector n-rows n-cols) contents))
        (raise-arguments-error 'make-2darray
                               "array has mismatched row lengths"
                               "first row" n-cols
                               "other rows" (cdr row-lengths)))))

; 2darray a -> 2darray ((Int,Int), a)
(define (enum-2d 2darray)
  ; enum-2d takes a 2darray and with each item pairs to it a tuple with its matrix indices
  (let* ([as-list   (2darray->list 2darray)]
         [with-cols (map (curry enumerate-tag 'col) as-list)]
         [with-rows (enumerate-tag 'row with-cols)])
    (make-2darray
     (map
      (lambda (row)
        (let ([i (get-index row)])
          (map
           (lambda (cell)
             (let ([j (get-index cell)])
               (tag-as (make-vector i j) (get-enum-value cell))))
           (get-enum-value row))))
      with-rows))))

; (vector, a) -> a
(define get-enum-2d-value get-enum-value)

; (vector, a) -> vector
(define get-enum-2d-index car)

; 2darray -> 2darray
(define (map-2d fn 2darray)
  ; map-2d maps a function across a 2darray
  (let ([as-list   (2darray->list 2darray)])
    (make-2darray (map (curry map fn) as-list))))

; fold-2d folds a 2darray
(define (fold-2d proc init 2darray)
  (define flat1 ; flatten a list of lists to depth-1 only
    (curry foldl append '()))
  (foldl proc init (flat1 (2darray->list 2darray))))

; board (enumerated 2darray)
(define empty-board
  (enum-2d (empty-2darray board-height board-width)))

; 2darray a -> (Int, Int)
(define (get-dimensions x)
  (cond [(is-2darray? x) (car (get-value x))]
        [(is-tetramino? x) (get-dimensions (get-mask x))]
        [else (raise-type-error 'get-dimensions "2darray/tetramino" x)]))

; cell / row -> bool
(define (is-row? row)
  (eq? (get-enum-tag row) 'row))

; board -> [row]
(define get-rows cddr)

; row -> [cell]
(define get-cells identity)

; cell -> bool
(define (is-empty? cell)
  (eq? 'Empty
       (get-enum-value cell)))

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

; 2darray a > Int -> Int -> a
(define (2darray-ref 2darray i j)
  (let* ([dims    (get-dimensions 2darray)]
         [i-max (- (get-x dims) 1)]
         [j-max (- (get-y dims) 1)])
    (cond [(> i i-max) (raise-range-error '2darray-ref "2darray" "x " i 2darray 0 i-max)]
          [(> j j-max) (tetramino-ref (get-tetramino 'T) 1 0) (raise-range-error '2darray-ref "2darray" "y " j 2darray 0 j-max)]
          [else (list-ref (list-ref (cddr 2darray) i) j)]
          )))

; 2darray a -> vector -> a
(define (2darray-vector-ref 2darray v)
  (let ([i (get-x v)]
        [j (get-y v)])
    (2darray-ref 2darray i j)))

; tetraminio -> Bool
(define (tetramino-ref tetramino i j)
  (2darray-ref (get-mask tetramino) i j))

; 2darray -> 2darray
(define (bool-mask symbols)
  (define (replacer symbol)
    (cond [(eq? symbol 'X) #t]
          [(eq? symbol '_) #f]))
  (make-2darray (map (curry map replacer) symbols)))

; colour -> [[symbol]] -> tetramino
(define (make-tetramino colour 2darray-mask)

  (tag-as 'tetramino
          (cons colour 2darray-mask)))

; tetramino -> [[symbol]]
(define (get-mask tetramino)
  (if (is-tetramino? tetramino)
      (cdr (get-value tetramino))
      (raise-type-error 'get-mask "tetramino" tetramino)))

; tetramino -> colour
(define (get-colour tetramino)
  (if (is-tetramino? tetramino)
      (car (get-value tetramino))
      (raise-type-error 'get-colour "tetramino" tetramino)))

; tetramino/cell/row -> bool
(define (is-tetramino? tetramino)
  (eq? (get-tag tetramino) 'tetramino))

; tetramino -> string
(define (tetramino->string tetramino)
  (if (is-tetramino? tetramino)
      (get-mask tetramino)
      (raise-type-error 'tetramino->string "tetramino" tetramino)))
  
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

; vector -> colour -> cell
(define (make-cell loc colour)
  (cons loc colour))

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

; 2darray -> 2darray
(define (rotate-2darray 2darray)
  ; Rotates a 2darray 90 degrees
  (let* ([old-dims (get-dimensions 2darray)]
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

; [a] -> a
(define (random-choice mylist)
  ; Choose random element from list
  (list-ref mylist (random 0 (length mylist))))

; tetramino -> tetramino
(define (rotate-tetramino tetramino)
  ; Rotates a tetramino 90 degrees
  (let ([colour       (get-colour tetramino)]
        [mask-2darray (get-mask tetramino)])
  (make-tetramino colour (rotate-2darray mask-2darray))))

; vector
(define starting-loc (make-vector 0 5))

; game-state -> keypress -> game-state
(define (update-game game-state key)
  ; updates the game-state when a key is pressed
  (let* ([tetramino-in-play  (get-tetramino-in-play game-state)]
         [loc                (get-tetramino-loc     game-state)]
         [board              (get-board             game-state)]
         [player-move (cond [(key=? key "left")  'l]
                            [(key=? key "right") 'r]
                            [(key=? key "up")    'u]
                            [(key=? key "down")  'd]
                            [else 'w])]

         [rotation?         (eq? player-move 'u)]
         [proposed-loc (cond [(eq? player-move 'l) (add-vector loc (make-vector  0 -1))]
                             [(eq? player-move 'r) (add-vector loc (make-vector  0 +1))]
                             [(eq? player-move 'd) (add-vector loc (make-vector +1  0))]
                             [else loc])]

         [proposed-tetramino (if rotation?
                                 (rotate-tetramino tetramino-in-play)
                                 tetramino-in-play)]

         [collision? (not (can-move-to? proposed-tetramino proposed-loc board))]
         [place?     (and (eq? player-move 'd) collision?)]
                         
         [new-board (if place?
                        (car (clear-full-rows (place-tetramino loc
                                         tetramino-in-play
                                         board)))
                        board)]

         [new-loc (cond [place?     starting-loc]
                        [collision? loc]
                        [else proposed-loc])]

         [new-tetramino (cond [place? (get-tetramino (random-choice tetraminos))]
                              [collision? tetramino-in-play]
                              [else proposed-tetramino])])
    (make-game-state new-tetramino
                     new-loc
                     new-board)))

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
         [tetra-max-y  (+ tetra-min-y tetra-height)]
         [board-dims   (get-dimensions board)]
         [board-height (get-y board-dims)]
         [board-width  (get-x board-dims)]
         [in-floor?    (or (< tetra-min-y 0)
                           (> tetra-max-y board-height))]
         [in-walls?    (or (< tetra-min-x 0)
                           (> tetra-max-x board-width))]
         [overlaps-non-empty?
            (and (not in-floor?)
                 (not in-walls?)
                 (fold-2d (lambda (x y) (and x y)) ; should use andmap
                          #t
                          (map-2d (lambda (cell)
                                    (let (; check whether the tetramino actually occupies this cell
                                          [occupied? (get-enum-2d-value cell)]
                                          ; and get the location of the cell on the board
                                          [cell-loc (add-vector loc (get-enum-2d-index cell))])
                                      (or (not occupied?)
                                          (is-empty? (2darray-vector-ref board cell-loc)))))
                          (enum-2d (get-mask tetramino)))))])
           overlaps-non-empty?))

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

; game-state = [tetramino, vector, board]
; tetramino -> vector -> board -> game-state
(define (make-game-state tetramino-in-play tetramino-loc board)
  (list tetramino-in-play tetramino-loc board))

; game-state -> tetramino
(define get-tetramino-in-play first)

; game-state -> vector
(define get-tetramino-loc second)

; game-state -> board
(define get-board third)

; board -> (board, Int)
(define (clear-full-rows board)
  ; Takes a board, then clears full rows and returns the new board
  ; along with the points gained due to the clears
  ; Int -> Int -> board -> board

  (define (clear-rows i-min i-max board)
    ; remove rows between i-min and i-max
    (define (move-cell index-vector cell)
      (make-cell index-vector (get-enum-2d-value cell)))
    (begin (display "i-min: ")
           (display i-min)
           (newline)
           (display "i-max: ")
           (display i-max)
           (newline)
           (newline)
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
                     board))))
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
                                           (curry all? (compose not is-empty?))
                                           (2darray->list board)))]
           [my-list (foldl (lambda (row pair-list)
                             (let ([i         (get-index row)]
                                   [row-full? (get-value row)])
                               (build-pair-list i row-full? pair-list)))
                           '()
                           rows-full)])
      (let ([return (cond [(null? my-list) '()]
                          [(is-full? (car my-list)) my-list]
                          [else (cons ((car my-list) board-height #f)
                                      (cdr my-list))])])
        (begin (display my-list)
               (display return)
               (newline)
               return))))
            

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
        
         
                  
  

; game-state -> image
(define (draw-game game-state)
  (let ([tetramino-in-play (first  game-state)]
        [loc               (second game-state)]
        [board             (third  game-state)])
  (board->image (place-tetramino loc
                                 tetramino-in-play
                                 board))))


(big-bang
    (make-game-state (get-tetramino (random-choice tetraminos)) ; initial state
                     starting-loc
                     empty-board)
  (to-draw draw-game)    ; redraws the world
  (on-key update-game))  ; process the event of key press


(define test-board
  '(2darray
   (Vector 20 . 10)
   (((Vector 0 . 0) . Empty)
    ((Vector 0 . 1) . Empty)
    ((Vector 0 . 2) . Empty)
    ((Vector 0 . 3) . Empty)
    ((Vector 0 . 4) . Empty)
    ((Vector 0 . 5) . Yellow)
    ((Vector 0 . 6) . Yellow)
    ((Vector 0 . 7) . Purple)
    ((Vector 0 . 8) . Cyan)
    ((Vector 0 . 9) . Blue))
   (((Vector 1 . 0) . Blue)
    ((Vector 1 . 1) . Empty)
    ((Vector 1 . 2) . Empty)
    ((Vector 1 . 3) . Cyan)
    ((Vector 1 . 4) . Green)
    ((Vector 1 . 5) . Yellow)
    ((Vector 1 . 6) . Yellow)
    ((Vector 1 . 7) . Red)
    ((Vector 1 . 8) . Orange)
    ((Vector 1 . 9) . Blue))
   (((Vector 2 . 0) . Blue)
    ((Vector 2 . 1) . Blue)
    ((Vector 2 . 2) . Blue)
    ((Vector 2 . 3) . Cyan)
    ((Vector 2 . 4) . Empty)
    ((Vector 2 . 5) . Red)
    ((Vector 2 . 6) . Red)
    ((Vector 2 . 7) . Empty)
    ((Vector 2 . 8) . Blue)
    ((Vector 2 . 9) . Blue))
   (((Vector 3 . 0) . Purple)
    ((Vector 3 . 1) . Purple)
    ((Vector 3 . 2) . Purple)
    ((Vector 3 . 3) . Cyan)
    ((Vector 3 . 4) . Empty)
    ((Vector 3 . 5) . Blue)
    ((Vector 3 . 6) . Red)
    ((Vector 3 . 7) . Red)
    ((Vector 3 . 8) . Yellow)
    ((Vector 3 . 9) . Yellow))
   (((Vector 4 . 0) . Purple)
    ((Vector 4 . 1) . Purple)
    ((Vector 4 . 2) . Purple)
    ((Vector 4 . 3) . Cyan)
    ((Vector 4 . 4) . Orange)
    ((Vector 4 . 5) . Blue)
    ((Vector 4 . 6) . Blue)
    ((Vector 4 . 7) . Blue)
    ((Vector 4 . 8) . Yellow)
    ((Vector 4 . 9) . Yellow))
   (((Vector 5 . 0) . Purple)
    ((Vector 5 . 1) . Purple)
    ((Vector 5 . 2) . Purple)
    ((Vector 5 . 3) . Purple)
    ((Vector 5 . 4) . Orange)
    ((Vector 5 . 5) . Cyan)
    ((Vector 5 . 6) . Cyan)
    ((Vector 5 . 7) . Cyan)
    ((Vector 5 . 8) . Cyan)
    ((Vector 5 . 9) . Blue))
   (((Vector 6 . 0) . Purple)
    ((Vector 6 . 1) . Purple)
    ((Vector 6 . 2) . Purple)
    ((Vector 6 . 3) . Green)
    ((Vector 6 . 4) . Orange)
    ((Vector 6 . 5) . Orange)
    ((Vector 6 . 6) . Purple)
    ((Vector 6 . 7) . Purple)
    ((Vector 6 . 8) . Purple)
    ((Vector 6 . 9) . Blue))
   (((Vector 7 . 0) . Purple)
    ((Vector 7 . 1) . Purple)
    ((Vector 7 . 2) . Purple)
    ((Vector 7 . 3) . Green)
    ((Vector 7 . 4) . Green)
    ((Vector 7 . 5) . Red)
    ((Vector 7 . 6) . Red)
    ((Vector 7 . 7) . Purple)
    ((Vector 7 . 8) . Blue)
    ((Vector 7 . 9) . Blue))
   (((Vector 8 . 0) . Cyan)
    ((Vector 8 . 1) . Cyan)
    ((Vector 8 . 2) . Cyan)
    ((Vector 8 . 3) . Cyan)
    ((Vector 8 . 4) . Green)
    ((Vector 8 . 5) . Purple)
    ((Vector 8 . 6) . Red)
    ((Vector 8 . 7) . Red)
    ((Vector 8 . 8) . Yellow)
    ((Vector 8 . 9) . Yellow))
   (((Vector 9 . 0) . Blue)
    ((Vector 9 . 1) . Blue)
    ((Vector 9 . 2) . Blue)
    ((Vector 9 . 3) . Blue)
    ((Vector 9 . 4) . Purple)
    ((Vector 9 . 5) . Purple)
    ((Vector 9 . 6) . Purple)
    ((Vector 9 . 7) . Green)
    ((Vector 9 . 8) . Yellow)
    ((Vector 9 . 9) . Yellow))
   (((Vector 10 . 0) . Blue)
    ((Vector 10 . 1) . Blue)
    ((Vector 10 . 2) . Blue)
    ((Vector 10 . 3) . Blue)
    ((Vector 10 . 4) . Green)
    ((Vector 10 . 5) . Orange)
    ((Vector 10 . 6) . Orange)
    ((Vector 10 . 7) . Green)
    ((Vector 10 . 8) . Green)
    ((Vector 10 . 9) . Blue))
   (((Vector 11 . 0) . Purple)
    ((Vector 11 . 1) . Purple)
    ((Vector 11 . 2) . Purple)
    ((Vector 11 . 3) . Green)
    ((Vector 11 . 4) . Green)
    ((Vector 11 . 5) . Green)
    ((Vector 11 . 6) . Orange)
    ((Vector 11 . 7) . Empty)
    ((Vector 11 . 8) . Green)
    ((Vector 11 . 9) . Blue))
   (((Vector 12 . 0) . Orange)
    ((Vector 12 . 1) . Purple)
    ((Vector 12 . 2) . Green)
    ((Vector 12 . 3) . Green)
    ((Vector 12 . 4) . Green)
    ((Vector 12 . 5) . Green)
    ((Vector 12 . 6) . Orange)
    ((Vector 12 . 7) . Empty)
    ((Vector 12 . 8) . Blue)
    ((Vector 12 . 9) . Blue))
   (((Vector 13 . 0) . Orange)
    ((Vector 13 . 1) . Empty)
    ((Vector 13 . 2) . Green)
    ((Vector 13 . 3) . Green)
    ((Vector 13 . 4) . Green)
    ((Vector 13 . 5) . Red)
    ((Vector 13 . 6) . Empty)
    ((Vector 13 . 7) . Empty)
    ((Vector 13 . 8) . Yellow)
    ((Vector 13 . 9) . Yellow))
   (((Vector 14 . 0) . Orange)
    ((Vector 14 . 1) . Orange)
    ((Vector 14 . 2) . Green)
    ((Vector 14 . 3) . Green)
    ((Vector 14 . 4) . Red)
    ((Vector 14 . 5) . Red)
    ((Vector 14 . 6) . Empty)
    ((Vector 14 . 7) . Purple)
    ((Vector 14 . 8) . Yellow)
    ((Vector 14 . 9) . Yellow))
   (((Vector 15 . 0) . Empty)
    ((Vector 15 . 1) . Purple)
    ((Vector 15 . 2) . Green)
    ((Vector 15 . 3) . Green)
    ((Vector 15 . 4) . Red)
    ((Vector 15 . 5) . Red)
    ((Vector 15 . 6) . Empty)
    ((Vector 15 . 7) . Purple)
    ((Vector 15 . 8) . Purple)
    ((Vector 15 . 9) . Purple))
   (((Vector 16 . 0) . Purple)
    ((Vector 16 . 1) . Purple)
    ((Vector 16 . 2) . Purple)
    ((Vector 16 . 3) . Green)
    ((Vector 16 . 4) . Red)
    ((Vector 16 . 5) . Red)
    ((Vector 16 . 6) . Empty)
    ((Vector 16 . 7) . Purple)
    ((Vector 16 . 8) . Purple)
    ((Vector 16 . 9) . Purple))
   (((Vector 17 . 0) . Yellow)
    ((Vector 17 . 1) . Yellow)
    ((Vector 17 . 2) . Yellow)
    ((Vector 17 . 3) . Yellow)
    ((Vector 17 . 4) . Red)
    ((Vector 17 . 5) . Red)
    ((Vector 17 . 6) . Red)
    ((Vector 17 . 7) . Red)
    ((Vector 17 . 8) . Red)
    ((Vector 17 . 9) . Purple))
   (((Vector 18 . 0) . Yellow)
    ((Vector 18 . 1) . Yellow)
    ((Vector 18 . 2) . Yellow)
    ((Vector 18 . 3) . Yellow)
    ((Vector 18 . 4) . Yellow)
    ((Vector 18 . 5) . Yellow)
    ((Vector 18 . 6) . Red)
    ((Vector 18 . 7) . Red)
    ((Vector 18 . 8) . Red)
    ((Vector 18 . 9) . Red))
   (((Vector 19 . 0) . Cyan)
    ((Vector 19 . 1) . Cyan)
    ((Vector 19 . 2) . Cyan)
    ((Vector 19 . 3) . Cyan)
    ((Vector 19 . 4) . Yellow)
    ((Vector 19 . 5) . Yellow)
    ((Vector 19 . 6) . Cyan)
    ((Vector 19 . 7) . Cyan)
    ((Vector 19 . 8) . Cyan)
    ((Vector 19 . 9) . Cyan))))