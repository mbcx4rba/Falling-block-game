#lang racket
(require 2htdp/image)
(require 2htdp/universe)


;; (a -> Bool) -> [a] -> Bool
(define (all? fn my-list)
  (foldl (lambda (x bs) (and bs (fn x))) #t my-list))

(define (enumerate-tag tag xs)
  (define (range start stop)
  (if (> start stop)
      '()
      (cons start (range (+ 1 start) stop))))
  (define (zip x y) (map cons x y))
  (zip
   (map (curry cons tag) (range 0 (- (length xs) 1))) xs))

(define (get-index enum-item)
  (cdr (get-tag enum-item)))

(define (get-enum-tag enum-item)
  (car (get-tag enum-item)))

(define get-enum-value cdr)

;; Int
(define board-width 10)

;; Int
(define board-height 20)

;; Int -> a -> [a]
(define (replicate n a)
  (if (= n 0)
      '()
      (cons a (replicate (- n 1) a))))

;; symbol -> a -> (symbol, a)
(define (tag-as tag item)
  (cons tag item))

;; forall a. (symbol, a) -> symbol
(define get-tag car)

;; forall a. (symbol, a) -> a
(define get-value cdr)

;; cell -> symbol
(define cell-colour get-value)

;; number -> number -> vector 
(define (make-vector x y)
  (tag-as 'Vector (cons x y)))

;; 2darray -> list
(define (2darray->list 2darray)
 (if (is-2darray? 2darray)
     (cdr (get-value 2darray))
     (raise-type-error '2darray->string "2darray" 2darray)))

;; returns an empty n x m 2darray0
;; Int -> Int -> 2darray
(define (empty-2darray n m)
  (make-2darray (replicate n (replicate m 'Empty))))

;; cell / row / 2darray /tetramino -> bool 
(define (is-2darray? 2darray)
  (eq? (get-tag 2darray) '2darray))

;; [[a]] -> 2darray a 
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

;; enum-2d takes a 2darray and with each item pairs to it a tuple with its matrix indices
;; 2darray a -> 2darray ((Int,Int), a)
(define (enum-2d 2darray)
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

;; (vector, a) -> a
(define get-enum-2d-value get-enum-value)

;; (vector, a) -> vector
(define get-enum-2d-index car)

;; map-2d maps a function across a 2darray
;; 2darray -> 2darray
(define (map-2d fn 2darray)
  (let ([as-list   (2darray->list 2darray)])
    (make-2darray (map (curry map fn) as-list))))

;; fold-2d folds a 2darray
(define (fold-2d proc init 2darray)
  (define flat1 ;; flatten a list of lists to depth-1 only
    (curry foldl append '()))
  (foldl proc init (flat1 (2darray->list 2darray))))

;; board (enumerated 2darray)
(define empty-board
  (enum-2d (empty-2darray board-height board-width)))

;; 2darray a -> (Int, Int)
(define (get-dimensions x)
  (cond [(is-2darray? x) (car (get-value x))]
        [(is-tetramino? x) (get-dimensions (get-mask x))]
        [else (raise-type-error 'get-dimensions "2darray/tetramino" x)]))

;; cell / row -> bool
(define (is-row? row)
  (eq? (get-enum-tag row) 'row))

;; board -> [row]
(define get-rows cddr)

;; row -> [cell]
(define get-cells identity)

;; cell -> bool
(define (is-empty? cell)
  (eq? 'Empty
       (get-enum-value cell)))

;; board -> string
(define (board->string board)
  ;; cell -> char
  (define (cell->char cell)
    (cond [(is-empty? cell) #\_]
          [else #\#]))
  (string-join (map list->string (2darray->list (map-2d cell->char board))) "\n"))

;; board -> void (with side effect)
(define (show-board board)
  (display (board->string board)))

;; 2darray a > Int -> Int -> a
(define (2darray-ref 2darray i j)
  (let* ([dims    (get-dimensions 2darray)]
         [i-max (- (get-x dims) 1)]
         [j-max (- (get-y dims) 1)])
    (cond [(> i i-max) (raise-range-error '2darray-ref "2darray" "x " i 2darray 0 i-max)]
          [(> j j-max) (tetramino-ref (get-tetramino 'T) 1 0) (raise-range-error '2darray-ref "2darray" "y " j 2darray 0 j-max)]
          [else (list-ref (list-ref (cddr 2darray) i) j)]
          )))

(define (2darray-vector-ref 2darray v)
  (let ([i (get-x v)]
        [j (get-y v)])
    (2darray-ref 2darray i j)))

;; tetraminio -> 
(define (tetramino-ref tetramino i j)
  (2darray-ref (get-mask tetramino) i j))
        
(define (bool-mask symbols)
  (define (replacer symbol)
    (cond [(eq? symbol 'X) #t]
          [(eq? symbol '_) #f]))
  (make-2darray (map (curry map replacer) symbols)))

;; colour -> [[symbol]] -> tetramino
(define (make-tetramino colour 2darray-mask)

  (tag-as 'tetramino
          (cons colour 2darray-mask)))

;; tetramino -> [[symbol]]
(define (get-mask tetramino)
  (if (is-tetramino? tetramino)
      (cdr (get-value tetramino))
      (raise-type-error 'get-mask "tetramino" tetramino)))

;; tetramino -> colour
(define (get-colour tetramino)
  (if (is-tetramino? tetramino)
      (car (get-value tetramino))
      (raise-type-error 'get-colour "tetramino" tetramino)))

;; tetramino/cell/row -> bool
(define (is-tetramino? tetramino)
  (eq? (get-tag tetramino) 'tetramino))

;; tetramino -> string
(define (tetramino->string tetramino)
  (if (is-tetramino? tetramino)
      (get-mask tetramino)
      (raise-type-error 'tetramino->string "tetramino" tetramino)))
  
;; [symbol]
(define tetraminos '(O L J S Z T I))

;; symbol -> tetramino
(define (get-tetramino name)
  (define O (make-tetramino 'Yellow (bool-mask '((X X)
                                                 (X X)))))
  
  (define L (make-tetramino 'Orange (bool-mask '((_ _ X)
                                                 (X X X)))))
  
  (define J (make-tetramino 'Blue (bool-mask '((X X X)
                                               (_ _ X)))))
  
  (define S (make-tetramino 'Green (bool-mask '((_ X X)
                                                (X X _)))))

  (define Z (make-tetramino 'Red (bool-mask '((X X _)
                                              (_ X X)))))
  
  (define T (make-tetramino 'Purple (bool-mask '((X X X)
                                                 (_ X _)))))
  
  (define I (make-tetramino 'Cyan (bool-mask '((X X X X)))))
  
  (cond ((eq? name 'O) O)   ;; square
        ((eq? name 'L) L)   ;; L
        ((eq? name 'J) J)   ;; reverse L
        ((eq? name 'S) S)   ;; zig zag
        ((eq? name 'Z) Z)   ;; zag zig
        ((eq? name 'T) T)   ;; T piece
        ((eq? name 'I) I))) ;; long straight piece

;; vector -> vector -> vector
(define (add-vector v1 v2)
  (let ([x1 (get-x v1)]
        [y1 (get-y v1)]
        [x2 (get-x v2)]
        [y2 (get-y v2)])
    (make-vector (+ x1 x2) (+ y1 y2))))
    
(define (is-vector? v)
  (eq? (get-tag v) 'Vector))

(define (get-x v)
  (if (is-vector? v)
      (car (get-value v))
      (raise-type-error 'get-x "Vector" v)))

(define (get-y v)
  (if (is-vector? v)
      (cdr (get-value v))
      (raise-type-error 'get-y "Vector" v)))

;; place-tetramino :: (int,int) -> tetramino -> board -> board
;; no checking if we can place here, just place the piece
;; loc should be a vector consisting of two ints in the following coordinate system
;; +----> (., y)
;; |
;; |
;; V (x,.) where 0,0 is the top left of the board
;; tetraminos are arrays with a certain extent and rotation
(define (place-tetramino loc tetramino board)
  (define (make-cell loc colour)
    (cons loc colour))
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
                    ;; cell occupied by tetramino in play, so fill the cell
                    (make-cell loc new-colour)
                    ;; cell not occupied by tetramino in play, so leave unchanged
                    cell)))
            board)))
;; Rotates a 2darray 90 degrees
;; 2darray -> 2darray
(define (rotate-2darray 2darray)
  ;; the rotated matrix has
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

;; Choose random element from list
;; [a] -> a
(define (random-choice mylist)
  (list-ref mylist (random 0 (length mylist))))

;; Rotates a tetramino 90 degrees
;; tetramino -> tetramino
(define (rotate-tetramino tetramino)
  (let ([colour       (get-colour tetramino)]
        [mask-2darray (get-mask tetramino)])
  (make-tetramino colour (rotate-2darray mask-2darray))))

(define starting-loc (make-vector 0 5))

(define (update-game world-state a-key)
  (define test-collision (const #f)) ;; debug
  (define (update-loc move loc)
    (cond [(eq? move 'l) (add-vector loc (make-vector +1 -1))]
          [(eq? move 'r) (add-vector loc (make-vector +1 +1))]
          [(eq? move 'd) (add-vector loc (make-vector +2  0))]
          [else (add-vector loc (make-vector +1 0))]))
    
    (let* ([player-move (cond [(key=? a-key "left")  'l]
                              [(key=? a-key "right") 'r]
                              [(key=? a-key "up")    'u]
                              [(key=? a-key "down")  'd]
                              [(key=? a-key " ") 'p]
                              [else 'w])]
           [rotation? (eq? player-move 'u)]
           [tetramino-in-play (first  world-state)]
           [loc               (second world-state)]
           [board             (third  world-state)]
           [place? (eq? player-move 'p)]
           [proposed-loc (update-loc player-move loc)]
           [collision? (or place?
                           (not (can-move-to? tetramino-in-play proposed-loc board)))]
           [new-board (if collision?
                        (place-tetramino loc
                                         tetramino-in-play
                                         board)
                        board)]
         [new-loc (if collision?
                      starting-loc
                      proposed-loc)]
         [new-tetramino (cond [collision? (get-tetramino (random-choice tetraminos))]
                              [rotation?  (rotate-tetramino tetramino-in-play)]
                              [else tetramino-in-play])])
    (list new-tetramino
          new-loc
          new-board)))

;; tetramino -> (Int,Int) -> board -> Bool
;; Returns false if a tetramino collides with either
;;    1. Another tetramino
;;    2. The "floor" of the board
;;    3. The "walls" of the board

(define (can-move-to? tetramino loc board)
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
         [in-floor? (or (< tetra-min-y 0)
                        (> tetra-max-y board-height))]
         [in-walls? (or (< tetra-min-x 0)
                        (> tetra-max-x board-width))]
         [overlaps-non-empty?
          (lambda ()  (fold-2d (lambda (x y) (and x y))
                   #t
                   (map-2d
                    (lambda (cell)
                      (let ([cell-loc (add-vector loc
                                                  (get-enum-2d-index cell))])
                        (is-empty? (2darray-vector-ref board cell-loc))))
                   
                   (enum-2d (get-mask tetramino)))))])
      (and (not in-floor?)
           (not in-walls?)
           (overlaps-non-empty?))))
    
;    (not (or (in-floor?) (in-walls?) (overlaps-non-empty?))))) ;; De Morgan's



(define grid-size 25)

;; board -> image
(define (board->image board)
  ;; cell -> char
  (let* (;; grid size in pixels
        [grid-size 25]
        [image-h (* board-height grid-size)]
        [image-w (* board-width  grid-size)]
        [canvas (empty-scene image-w image-h)])
  
  (define (draw-cell cell scene)
    (if (is-empty? cell)
        scene ;; nothing to do
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
  (fold-2d draw-cell canvas board)))

(define (draw-game game-state)
  (let ([tetramino-in-play (first  game-state)]
        [loc               (second game-state)]
        [board             (third  game-state)])
  (board->image (place-tetramino loc
                                 tetramino-in-play
                                 board))))


(big-bang (list (get-tetramino (random-choice tetraminos)) ;; maybe add some abstraction here for game state
                starting-loc
                empty-board)    ; <-- initial state
          (to-draw draw-game)   ; <-- redraws the world
          (on-key update-game)) ; <-- process the event of key press
