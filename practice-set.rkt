;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname practice-set) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; DATA DEFNITIONS -----------------------------------------------------------------------------------
(require 2htdp/image)
(require 2htdp/universe)

(define WIDTH 100)
(define SPACE 10)

(define-struct object [shape color])
;; an Object is a (make-object Shape ObColor)
(define ob1 (make-object "square" "red"))
(define ob2 (make-object "circle" "blue"))
(define ob3 (make-object "triangle" "green"))

;; A Shape is one of
;; - "square"
;; - "circle"
;; - "triangle"

;; An ObColor is one of
;; - "red"
;; - "blue"
;; - "green"
;; - "darkred"
;; - "darkblue"
;; - "darkgreen"

;; a World is a [List-of Object]
(define w1 (list ob1 ob2 ob3))

;; MAKE WORLD ----------------------------------------------------------------------------------------
;; random-x : Number Symbol Symbol Symbol -> Symbol
;; chooses the nth input (up to 3)
(define (pick-n n s1 s2 s3)
  (cond
    [(= n 0) s1]
    [(= n 1) s2]
    [(= n 2) s3]
    [(> n 2) (error "n must be less than 3")]))

;; random-shape : Number -> Shape
;; picks a random shape
(define (random-shape n)
  (pick-n n "square" "circle" "triangle"))

;; random-color : Number -> ObColor
;; picks a random fill color
(define (random-color n)
  (pick-n n "red" "blue" "green"))

;; make-random-world : Number -> World
;; creates a world with n randomly generated objects
(define (make-random-world n)
  (local
    [;; random-ob : Any -> Object
     (define (random-ob _)
       (make-object (random-shape (random 3))
         (random-color (random 3))))]
    (build-list n random-ob)))

;; DRAW WORLD ----------------------------------------------------------------------------------------

;; draw-world : World -> Image
(define (draw-world w)
  (cond
    [(empty? w) (empty-scene 0 0)]
    [(cons? w) (overlay/xy (draw-ob (first w)) (+ SPACE WIDTH) 0 (draw-world (rest w)))]))

;; draw-ob : Object -> Image
(define (draw-ob ob)
  (cond
    [(string=? (object-shape ob) "square")
     (square WIDTH "solid" (object-color ob))]
    [(string=? (object-shape ob) "circle")
     (circle (/ WIDTH 2) "solid" (object-color ob))]
    [(string=? (object-shape ob) "triangle")
     (triangle WIDTH "solid" (object-color ob))]))

;; ON KEY --------------------------------------------------------------------------------------------

;; change-colors : World KeyEvent -> World
(define (change-colors w ke)
  (cond
    [(string=? ke "r") (make-random-world (length w))]
    [(or (false? (string->number ke)) (> (string->number ke) (length w))) w]
    [else (change-nth (length w) w (string->number ke))]))

;; change-nth : Number World Number -> World
(define (change-nth len w n)
  (cond
    [(empty? w) '()]
    [(cons? w) (if (= n (- len (length (rest w))))
                   (cons (flip (first w)) (change-nth len (rest w) n))
                   (cons (first w) (change-nth len (rest w) n)))]))

;; flip : Object -> Object
(define (flip ob)
  (make-object (object-shape ob)
    (flip-color (object-color ob))))

;; flip-color : ObColor -> ObColor
(define (flip-color c)
  (cond
    [(or (string=? c "red")
         (string=? c "blue")
         (string=? c "green"))
     (string-append "dark" c)]
    [(string=? c "darkred") "red"]
    [(string=? c "darkblue") "blue"]
    [(string=? c "darkgreen") "green"]))

;; ON CLICK ------------------------------------------------------------------------------------------
  
;; reset-colors : World MouseEvent Number Number -> World
(define (reset-colors w x y me)
  (if (string=? "button-down" me)
      (cond
        [(empty? w) '()]
        [(cons? w) (cons (to-normal (first w))
                         (reset-colors (rest w) x y me))])
      w))

;; to-normal : Object -> Object
(define (to-normal ob)
  (cond
    [(string=? "darkred" (object-color ob))
     (make-object (object-shape ob) "red")]
    [(string=? "darkblue" (object-color ob))
     (make-object (object-shape ob) "blue")]
    [(string=? "darkgreen" (object-color ob))
     (make-object (object-shape ob) "green")]
    [else ob]))

;; BIG-BANG ------------------------------------------------------------------------------------------
;; clickgame : World -> World
(define (clickgame w)
  (big-bang w
    [to-draw draw-world]
    [on-key change-colors]
    [on-mouse reset-colors]))

(clickgame (make-random-world 9))