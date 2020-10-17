#lang racket
(require 2htdp/universe)
(require "2darray.rkt")
(require "tagged-data.rkt")
(require "vector.rkt")
(require "tetramino.rkt")
(require "board.rkt")
(require "sound-effects.rkt")

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
                        (begin
                          (play-sound place-piece-sound)
                          (car (clear-full-rows (place-tetramino loc
                                         tetramino-in-play
                                         board))))
                        board)]

         [new-loc (cond [place?     starting-loc]
                        [collision? loc]
                        [else proposed-loc])]

         [new-tetramino (cond [place? (get-random-tetramino)]
                              [collision? tetramino-in-play]
                              [else proposed-tetramino])])
    (make-game-state new-tetramino
                     new-loc
                     new-board)))

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
  (make-game-state (get-random-tetramino)
                   (make-vector 0 3)
                   empty-board)
  ; redraws the world
  (to-draw draw-game)    

  ; ticks - block falling every so often
  (on-tick (lambda (game-state) (update-game game-state 'd))
           0.3)


  ; process the event of key press
  (on-key (lambda (game-state key)
            (let ([player-move (cond [(or (key=? key "left")  (key=? key "a")) 'l]
                                     [(or (key=? key "right") (key=? key "d")) 'r]
                                     [(or (key=? key "up")    (key=? key "w")) 'u]
                                     [(or (key=? key "down")  (key=? key "s")) 'd]
                                     [else 'w])])
              (update-game game-state player-move)))))