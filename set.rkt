;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname set) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; SET

;; VARIABLE DEFINITIONS ------------------------------------------------------------------------------

(define solidsquiggle _)
(define soliddiamond _)
(define solidoval _)

(define linedsquiggle _)
(define lineddiamond _)
(define linedoval _)

(define outlinesquiggle _)
(define outlinediamond _)
(define outlineoval _)

;; DATA DEFINITIONS ----------------------------------------------------------------------------------
;; A Shape is one of
;; - 'oval
;; - 'diamond
;; - 'squiggle

;; A Fill is one of
;; - 'solid
;; - 'lined
;; - 'outline

;; A CardColor is one of
;; - 'red
;; - 'green
;; - 'purple

(define-struct card [shape fill color number clicked])
;; A Card is a (make-card Shape Fill CardColor Number Boolean)
(define card1 (make-card 'oval 'solid 'red 2 #false))

(define-struct set [one two three])
;; A Set is a (make-set Card Card Card)

(define-struct world [cards sets])
;; a World is a (make-world [List-of Card] [List-of Set])
;; and represents the state of the game. the game should
;; always have the same 9 cards
;; any item in sets must almost be in cards

;; WORLD SETUP ---------------------------------------------------------------------------------------
;; random-x : Number Symbol Symbol Symbol -> Symbol
;; chooses an input 
(define (pick-n n s1 s2 s3)
  (cond
    [(= n 0) s1]
    [(= n 1) s2]
    [(= n 2) s3]))

;; random-shape : Number -> Shape
;; picks a random shape
(define (random-shape n)
  (pick-n n 'oval 'diamond 'squiggle))

;; random-fill : Number -> Fill
;; picks a random fill 
(define (random-fill n)
  (pick-n n 'solid 'lined 'outline))

;; random-setcolor : Number -> CardColor
;; picks a random set color
(define (random-setcolor n)
  (pick-n n 'red 'green 'purple))

;; random-cards : Number -> [List-of Card]
;; creates a list of n randomly generated cards
(define (random-cards n)
  (local
    [;; random-card : Any -> Card
     (define (random-card _)
       (make-card (random-shape (random 3))
                  (random-fill (random 3))
                  (random-setcolor (random 3))
                  (+ 1 (random 3))
                  #false))]
    (build-list n random-card)))

(define start-world (make-world (random-cards 9) '()))

;; DRAW WORLD ----------------------------------------------------------------------------------------
#;(define (draw-world w)
    ...)
(require 2htdp/image)


;; draw-card : Card -> Image
#;(define (draw-card c)
  (overlay/align "center" "center"
                 (draw-shapes (card-shape c) (card-fill c) (card-color c) (card-number c))
                 (rectangle 200 100 "outline" "black")))

;; draw-shapes : Shape Fill CardColor Number -> Image
#;(define (draw-shapes s f c n)
  ...)

;; draw-shape : Shape Fill CardColor
#|(define (draw-shape s f c)
  (cond
    [(symbol=? s 'oval) (...]
    [(symbol=? s 'diamond) ...]
    [(symbol=? s 'squiggle) ...]))
|#




;; SET CALCULATOR -----------------------------------------------------------------------------------

;; validset? : Card Card Card -> Boolean










