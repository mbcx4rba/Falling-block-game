#lang racket
(require rsound)

(provide play-sound
         place-piece-sound)

; rsound
(define place-piece-sound
  ; Sound by Mark DiAngelo and released under 
  ; Creative Commons Attribution 3.0 license: 
  ; https://creativecommons.org/licenses/by/3.0/#
  ;
  ; from https://soundbible.com/2067-Blop.html
  (rs-read "blop.wav"))

; rsound -> void
(define (play-sound sound)
  ; plays sound effect as a side-effect
  (begin
   (play sound)
   (void)))
