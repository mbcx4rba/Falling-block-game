#lang racket
(require 2htdp/universe)
(require 2htdp/image)
(require "2darray.rkt")
(require "tagged-data.rkt")
(require "vector.rkt")
(require "tetramino.rkt")
(require "board.rkt")
(require "sound-effects.rkt")

; vector 
(define starting-loc (make-vector 0 3))

; vector -> keypress -> vector
(define (update-loc loc player-move)
  (cond [(eq? player-move 'l) (add-vector loc (make-vector  0 -1))]
        [(eq? player-move 'r) (add-vector loc (make-vector  0 +1))]
        [(eq? player-move 'd) (add-vector loc (make-vector +1  0))]
        [else loc]))

; tetramino -> keypress -> tetramino
(define (update-tetramino tetramino player-move)
  (if (eq? player-move 'u) (rotate-tetramino tetramino) tetramino))

; game-state -> keypress -> game-state (sound may play as a side effect)
(define (update-game old-game-state player-move)
  (define (score-of n place?)
          (cond [(not place?) 0]
                [(= n 0)      1]
                [(= n 1)     10]
                [(= n 2)     25]
                [(= n 3)     40]
                [(= n 4)    100]
                [else         1]))
  ; updates the game-state when a key is pressed
  (match-let* ([old-tetramino      (game-state-tetramino        old-game-state)]
               [old-loc            (game-state-loc              old-game-state)]
               [old-board          (game-state-board            old-game-state)]
               [old-score          (game-state-score            old-game-state)]
               [old-lines-cleared  (game-state-lines-cleared    old-game-state)]
               [proposed-loc       (update-loc       old-loc       player-move)]
               [proposed-tetramino (update-tetramino old-tetramino player-move)]
               
               ; Check if the new location and orientation of the tetramino collides with anything
               [collision? (not (can-move-to? proposed-tetramino proposed-loc old-board))]
               ; If it collides and the keystroke was down, then the tetramino should be placed
               [place?     (and (eq? player-move 'd) collision?)]
               ; 
               [(cons new-board lines-cleared) (if place?
                               (begin
                               (play-sound place-piece-sound)
                               (clear-full-rows (place-tetramino old-loc
                                                                       old-tetramino
                                                                       old-board)))
                               (cons old-board 0))]
               [new-lines-cleared (+ lines-cleared old-lines-cleared)]
               [new-score         (+ old-score  (score-of lines-cleared place?))]
   
               [new-loc (cond [place?     starting-loc]
                              [collision?      old-loc]
                              [else       proposed-loc])]
   
               [new-tetramino (cond [place? (get-random-tetramino)]
                                    [collision?      old-tetramino]
                                    [else       proposed-tetramino])])
              (game-state new-tetramino
                          new-loc
                          new-board
                          new-score
                          new-lines-cleared)))

; game-state = (tetramino, vector, board)
; tetramino -> vector -> board -> game-state
(struct game-state (tetramino loc board score lines-cleared))

; game-state -> image
(define (draw-game game-state)
  (let* ([tetramino          (game-state-tetramino     game-state)]
         [loc                (game-state-loc           game-state)]
         [board              (game-state-board         game-state)]
         [lines-cleared      (game-state-lines-cleared game-state)]
         [score              (game-state-score         game-state)]
         [board-image        (board->image (place-tetramino loc
                                                            tetramino
                                                            board))]
         [score-text         (text (string-append " Score: "
                                                  (number->string score))
                                   18
                                   "black")]
         [lines-cleared-text (text (string-append " Lines cleared: "
                                                  (number->string lines-cleared))
                                   18
                                   "black")])
        (beside/align "top"
                    board-image
                    (above/align "left"
                                 score-text
                                 lines-cleared-text)
                    (rectangle 25 1 "solid" "white"))))




(big-bang
  ; initial state
  (game-state (get-random-tetramino)   ; Initial tetramino
              starting-loc             ; Starting position of said tetramino
              empty-board              ; Initial state of board
              0                        ; Initial number of lines cleared
              0)                       ; Initial score
                
  ; redraws the world
  (to-draw draw-game)    

  ; ticks - block falling every so often
  (on-tick (lambda (old-game-state) (update-game old-game-state 'd))
           0.3)


  ; process the event of key press
  (on-key (lambda (old-game-state key)
            (let ([player-move (cond [(or (key=? key "left")  (key=? key "a")) 'l]
                                     [(or (key=? key "right") (key=? key "d")) 'r]
                                     [(or (key=? key "up")    (key=? key "w")) 'u]
                                     [(or (key=? key "down")  (key=? key "s")) 'd]
                                     [else 'w])])
              (update-game old-game-state player-move)))))
