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
(define starting-loc (make-vector 0 3))

; game-state -> keypress -> game-state
(define (update-game game-state player-move)
  ; updates the game-state when a key is pressed
  (let* ([tetramino-in-play  (get-tetramino-in-play game-state)]
         [loc                (get-tetramino-loc     game-state)]
         [board              (get-board             game-state)]
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
        
         
                  
  

; game-state -> image
(define (draw-game game-state)
  (let ([tetramino-in-play (first  game-state)]
        [loc               (second game-state)]
        [board             (third  game-state)])
  (board->image (place-tetramino loc
                                 tetramino-in-play
                                 board))))


(big-bang
  ; initial state
  (make-game-state (get-tetramino (random-choice tetraminos)) 
                   starting-loc
                   empty-board)
  ; redraws the world
  (to-draw draw-game)    
  (on-tick (lambda (game-state) (update-game game-state 'd))
           0.5)

  ; process the event of key press
  (on-key (lambda (game-state key)
            (let ([player-move (cond [(key=? key "left")  'l]
                            [(key=? key "right") 'r]
                            [(key=? key "up")    'u]
                            [(key=? key "down")  'd]
                            [else 'w])])
              (update-game game-state player-move)))))