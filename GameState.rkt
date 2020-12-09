#lang racket
(require 2htdp/universe)
(require 2htdp/image)
(require struct-update)
(require "2darray.rkt")
(require "tagged-data.rkt")
(require "vector.rkt")
(require "tetramino.rkt")
(require "board.rkt")
(require "sound-effects.rkt")-

; vector 
(define starting-loc (make-vector 0 3))

; vector -> keypress -> (vector -> Bool) -> (vector, Bool, Bool)
(define (update-loc loc-0 depressed-keys <can-move-to?>)
  ; Here's an overengineered solution to a relatively simple problem 
  ; ([vector -> (Bool, vector)], vector) -> ([Bool],vector) 
  (define (sequence-steps xs prev-loc)
     ; this is almost like mapM acting on a state monad
     ; Racket does in fact provide methods for monads in
     ; data/functor, so this may be a possible
     ; improvement here.
     (if (null? xs) (cons '() prev-loc)
                    (match-let* ([(cons collision? new-loc) ((car xs) prev-loc)]
                                 [(cons rest final-loc)    (sequence-steps (cdr xs) new-loc)])
                                (cons (cons collision? rest) 
                                            final-loc))))
  
  ; (keypress -> Bool, vector) -> vector -> (Bool, vector)
  (define (next-step key-check movement-vector)
    (lambda (prev-loc)
            (let ([proposed-loc (if (key-check depressed-keys)
                                    (add-vector prev-loc movement-vector)
                                    prev-loc)])
                 (if (<can-move-to?> proposed-loc)
                     (cons #f proposed-loc)
                     (cons #t prev-loc)))))
  
  (match-let* ([down-vector      (make-vector +1  0)]
               [right-vector     (make-vector  0 +1)]
               [left-vector      (make-vector  0 -1)]
               [(cons collisions
                      new-loc)   (sequence-steps (list (next-step depressed-left?  left-vector )
                                                       (next-step depressed-right? right-vector)
                                                       (next-step depressed-down?  down-vector ))
                                                 loc-0)]
               [place? (and (third collisions)
                            (depressed-down? depressed-keys)
                            (not (depressed-up? depressed-keys)))]
               [collision? (ormap identity collisions)])
             (begin
                 (newline)
                 (print collisions)
                 (list new-loc collision? place?))))

; tetramino -> keypress -> tetramino
(define (update-tetramino tetramino depressed-keys)
  (if (depressed-up? depressed-keys)
      (rotate-tetramino tetramino)
      tetramino))

; game-state -> game-state
(define (block-fall old-game-state)
        (let ([old-depressed-keys (game-state-depressed-keys old-game-state)]
              [mid-game-state     (update-game (game-state-depressed-keys-set old-game-state
                                                                              (depressed #f #f #f #t)))])
             (game-state-depressed-keys-set mid-game-state old-depressed-keys)))
              

; game-state -> keypress -> game-state (sound may play as a side effect)
(define (update-game old-game-state)
  (define (score-of n place?)
          (cond [(not place?) 0]
                [(= n 0)      1]
                [(= n 1)     10]
                [(= n 2)     25]
                [(= n 3)     40]
                [(= n 4)    100]
                [else         1]))
  ; updates the game-state when a key is pressed
  (match-let* ([old-tetramino      (game-state-tetramino      old-game-state)]
               [old-loc            (game-state-loc            old-game-state)]
               [old-board          (game-state-board          old-game-state)]
               [old-score          (game-state-score          old-game-state)]
               [old-lines-cleared  (game-state-lines-cleared  old-game-state)]
               [old-depressed-keys (game-state-depressed-keys old-game-state)]
               ; See if we've rotated the tetramino
               [proposed-tetramino (update-tetramino old-tetramino old-depressed-keys)]
               ; See where we're asked to move tetramino to
               [(list proposed-loc
                      collision?
                      place?)      (update-loc old-loc
                                               old-depressed-keys
                                               (curry can-move-to? proposed-tetramino old-board))]
               [(cons new-board lines-cleared) (if place?
                                                   (begin
                                                     (play-sound place-piece-sound)
                                                     (clear-full-rows (place-tetramino old-loc
                                                                                             old-tetramino
                                                                                             old-board)))
                                                   (cons old-board 0))]
               [new-depressed-keys (depressed-up?-set old-depressed-keys #f)]
               [new-lines-cleared  (+ lines-cleared old-lines-cleared)]
               [new-score          (+ old-score  (score-of lines-cleared place?))]
   
               [new-loc (if place? starting-loc
                                   proposed-loc)]
   
               [new-tetramino (cond [place? (get-random-tetramino)]
                                    [collision?      old-tetramino]
                                    [else       proposed-tetramino])])
              (game-state new-tetramino
                          new-loc
                          new-board
                          new-score
                          new-lines-cleared
                          new-depressed-keys)))

; This is invoked every few ticks to drop the block
; It is invoked more often on higher levels, so the
; blocks fall down faster on higher levels
; game-state -> game-state
; (define (drop-block old-game-state)
;         (update-game )

; game-state = (tetramino, vector, board)
; tetramino -> vector -> board -> game-state
(struct game-state (tetramino loc board score lines-cleared depressed-keys))
(define-struct-updaters game-state) ; Provides (functional/immutable) setters for game-state

(define (key-left?  key) (or (key=? key "left")  (key=? key "a")))
(define (key-right? key) (or (key=? key "right") (key=? key "d")))
(define (key-up?    key) (or (key=? key "up")    (key=? key "w")))
(define (key-down?  key) (or (key=? key "down")  (key=? key "s")))

; Struct which keeps track of which keys are currently held down
(struct depressed (left? right? up? down?) #:transparent)
(define-struct-updaters depressed) ; Provides depressed-left?-set, depressed-right?-set, etc.

; keystroke -> game-state -> game-state
(define (press-key old-game-state key-event)
    (let* ([tetramino                  (game-state-tetramino        old-game-state)]
           [loc                        (game-state-loc              old-game-state)]
           [board                      (game-state-board            old-game-state)]
           [score                      (game-state-score            old-game-state)]
           [lines-cleared              (game-state-lines-cleared    old-game-state)]
           [old-depressed-keys         (game-state-depressed-keys   old-game-state)]
           [left  (or (depressed-left?  old-depressed-keys) (key-left?  key-event))]
           [right (or (depressed-right? old-depressed-keys) (key-right? key-event))]
           [up    (or (depressed-up?    old-depressed-keys) (key-up?    key-event))]
           [down  (or (depressed-down?  old-depressed-keys) (key-down?  key-event))])
          (game-state tetramino
                      loc
                      board
                      score
                      lines-cleared
                      (depressed left right up down))))

; keystroke -> game-state -> game-state
(define (release-key old-game-state key-event)
    (let* ([tetramino                             (game-state-tetramino        old-game-state)]
           [loc                                   (game-state-loc              old-game-state)]
           [board                                 (game-state-board            old-game-state)]
           [score                                 (game-state-score            old-game-state)]
           [lines-cleared                         (game-state-lines-cleared    old-game-state)]
           [old-depressed-keys                    (game-state-depressed-keys   old-game-state)]
           [left  (not (implies (depressed-left?  old-depressed-keys) (key-left?  key-event)))]
           [right (not (implies (depressed-right? old-depressed-keys) (key-right? key-event)))]
           [up    (not (implies (depressed-up?    old-depressed-keys) (key-up?    key-event)))]
           [down  (not (implies (depressed-down?  old-depressed-keys) (key-down?  key-event)))])
          (game-state tetramino
                      loc
                      board
                      score
                      lines-cleared
                      (depressed left right up down))))



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
              0                        ; Initial score
              (depressed #f #f #f #f)) ; Keys depressed
                
  ; redraws the world
  (to-draw draw-game)    

  ; ticks - block falling every so often
  (on-tick (compose update-game block-fall) 0.1)

  ; process the event of key press
  (on-key press-key)
  
  ; process the event of key release
  (on-release release-key))
