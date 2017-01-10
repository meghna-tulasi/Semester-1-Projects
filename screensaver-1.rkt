;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname screensaver-1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;Screensaver
;;The screensaver consists of two circles that move inside a canvas.
;;if the circle in its normal motion it would hit or go past
;;one side of the canvas
;;if the circles hits the canvas edge, it will bounce off in the tangent direction
;;to the edge.
;;On a key event " ",both the circles pause at positions they are at that moment,
;;until another key event " " is provided.
;;to run the program
;;start with (screensaver sim-speed)

 
(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)
(require 2htdp/image)
(check-location "03" "screensaver-1.rkt")

(provide screensaver
         initial-world
         world-after-tick
         world-after-key-event
         world-circ1
         world-circ2
         world-paused?
         new-circle
         circ-x
         circ-y
         circ-vx
         circ-vy)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;CONSTANTS:
(define CANVAS-WIDTH 400)
(define CANVAS-HEIGHT 300)

(define CANVAS-LEFT-CORNER-X-COORD 0)
(define CANVAS-RIGHT-CORNER-X-COORD CANVAS-WIDTH)

(define CANVAS-TOP-CORNER-Y-COORD 0)
(define CANVAS-BOTTOM-CORNER-Y-COORD CANVAS-HEIGHT)

(define TOP-LEFT-CORNER "topleft")
(define TOP-RIGHT-CORNER "topright")
(define BOTTOM-LEFT-CORNER "bottomleft")
(define BOTTOM-RIGHT-CORNER "bottomright")

(define CIRC-RADIUS1 40)
(define CIRC-RADIUS2 40)

(define DIST-TO-EDGE-FM-CENTRE-X 40)
(define DIST-TO-EDGE-FM-CENTRE-Y 40)
  
(define CIRC1-X 200) (define CIRC1-Y 100)
(define CIRC2-X 200) (define CIRC2-Y 200)

(define CIRC1-VX -12) (define CIRC1-VY 20)
(define CIRC2-VX 23)  (define CIRC2-VY -14)

(define FONT-SIZE 12)
(define COLOR "blue")
(define MODE "outline")
(define CIRCLE (circle CIRC-RADIUS1 MODE COLOR))
(define PAUSED true)
(define UNPAUSED false)

(define RIGHT-EDGE "right")
(define LEFT-EDGE "left")
(define TOP-EDGE "top")
(define DOWN-EDGE "down")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;






;;DATA-DEFINITIONS:



(define-struct world (circ1 circ2 paused?))
;;A World is a
;;(make-world Circle Circle Boolean)
;;INTERPRETATION:
;;circle1 is the first circle inside the world with
;;(200,100) coordinates with radius 40 pixels
;;circle2 is the second circle inside the world with
;;(200,200) coordinates with radius 40 pixels
;;paused?->boolean represents if the world is paused or unpaused

;;Template
;;world-fn:WorldState -> ??
#;(define (world-fn w)
         (...
          (world-circ1 w)
          (world-circ2 w)
          (world-paused? w)))
     

(define-struct circ(x y vx vy))

;; A Circle is a
;; (make-circle NonNegInt NonNegInt Int Int)
;; INTERPRETATION:
;; x represents the x-coordinate of center of circle in computer-graphics co-ordinate system.
;; y represents the y-coordinate of center of circle in computer-graphics co-ordinate system.
;; vx represents the velocity of the circle per tick along x-axis.
;; - A positive value indicates movement towards right, negative value indicates left.
;; vy represents the velocity of the circle per tick along y-axis.
;; - A positive value indicates movement towards down, negative value indicates up.


;;Template
;; circle-fn: Circle -> ??
#;(define (circ-fn c)
     (...
      circ-x c
      circ-y c
      circ-vx c
      circ-vy c))

;; A Edge is a String which is one of
;; "left"
;; "right"
;; "top"
;; "down"
;; INTERPRETATION: represents any one of the 4 edges of canvas
;;template:
;; edge-fn: Edge -> ???
#;(define (edge-fn e)
    (cond
      [(string=? e LEFT-EDGE) ...]
      [(string=? e RIGHT-EDGE) ...]
      [(string=? e TOP-EDGE) ...]
      [(string=? e DOWN-EDGE) ...]))

;; A Corner is a String which is one of
;; "topleft"
;; "topright"
;; "bottomleft"
;; "bottomright"
;; INTERPRETATION: represents any one of the 4 corners of canvas
;;template:
;; corner-fn: Corner -> ???
#;(define (corner-fn c)
    (cond
      [(string=? c TOP-LEFT-CORNER) ...]
      [(string=? c TOP-RIGHT-CORNER) ...]
      [(string=? c BOTTOM-LEFT-CORNER) ...]
      [(string=? c BOTTOM-RIGHT-CORNER) ...]))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;MAIN FUNCTION:
;; screensaver : PosReal -> WorldState
;; GIVEN: the speed of the simulation, in seconds/tick
;; EFFECT: runs the simulation, starting with the initial state as
;; specified in the problem set.
;; RETURNS: the final state of the world
(define (screensaver sim-speed)
    (big-bang (initial-world  "start")
              (on-tick world-after-tick sim-speed)
              (on-key world-after-key-event)
              (on-draw world-to-scene)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;initial-world : Any -> WorldState
;;GIVEN: any value (ignored)
;;RETURNS:the initial world specified in the problem set
;;circle1(200,100) with velocity (-12,20)
;;circle2(200,200) with velocity (23,-14)
;;Initially, world will be paused.
;;EXAMPLES:
;;(initial-world "start") -> (make-world (make-circ 200 100 -12 20)
;;                                       (make-circ 200 200 23 -14) true)
;;DESIGN-STRATEGY: Using Constructor template of World
(define (initial-world any)
  (make-world (make-circ CIRC1-X CIRC1-Y CIRC1-VX CIRC1-VY)
              (make-circ CIRC2-X CIRC2-Y CIRC2-VX CIRC2-VY) PAUSED))
;;TESTS:
(begin-for-test
  (check-equal? (initial-world "start") (make-world (make-circ 200 100 -12 20)
                 (make-circ 200 200 23 -14) #true))
  (check-equal? (world-paused? (initial-world "start")) true
                "World is paused initially"))




;;new-circle :NonNegInt NonNegInt Int Int -> Circle
;;GIVEN: 2 non-negative integers x and y, and 2 integers vx and vy
;;RETURNS: a circle centered at (x,y), which will travel with
;; velocity (vx, vy).
;;EXAMPLES:
;;(new-circle 20 10 3 7) -> (make-circ 20 10 3 7)
;;DESIGN-STRATEGY: Using Constructor template of Circle
(define (new-circle x y vx vy)
  (make-circ x y vx vy))

;;TESTS:
(begin-for-test
  (check-equal? (new-circle 12 15 -23 10) (make-circ 12 15 -23 10)
                "new-circle should create circle at given circle's position."))
                



;; world-after-tick : WorldState -> WorldState
;; GIVEN: the current WorldState with details of both circle
;; and whether it is paused or not.
;; RETURNS: the world state that should follow the given world state
;; after a tick.
(define (world-after-tick w)
  (if (world-paused?  w) w
(update-after-tick w)))      

;;TESTS:
(begin-for-test
  (check-equal?(world-after-tick (make-world
                  (make-circ 50 50 10 10) (make-circ 60 60 10 10) UNPAUSED))
(make-world (make-circ 60 60 10 10) (make-circ 70 70 10 10) #false))

(check-equal?(world-after-tick (make-world
                  (make-circ 50 50 10 10) (make-circ 60 60 10 10) PAUSED))
             (make-world (make-circ 50 50 10 10) (make-circ 60 60 10 10) #true)))

   
;; update-after-tick : WorldState -> WorldState
;; GIVEN: the current WorldState with details of both circles
;; RETURNS: the new World State with updated co-ordinates of both circles
;;along with their velocities.
;; EXAMPLE:(update-after-tick (make-world (make-circle 10 4 -17 5)
;;  (make-circle 10 20 10 10) false)) -> (unpaused-scene-at-20-20-20-30)
;; STRATEGY: Use template fn of world on w
(define (update-after-tick w)
  (make-world (update-circ-pos (world-circ1 w))
              (update-circ-pos (world-circ2 w))
              false))


;; update-circ-pos: Circle -> Circle
;; GIVEN: the current Circle
;; RETURNS: Circle with (x,y) co-ordinates updated according to the velocity
;; EXAMPLES:(update-circ-pos (make-circle 70 70 -10 10)) -> (make-circle 60 80 -10 10)
;; STRATEGY: Divide into cases based on circle's position in canvas
(define (update-circ-pos c)
  (cond
    [(crosses-corner? c) (canvas-corner-crossed c)]
    [(crosses-canvas-edge? (circ-x c) (circ-y c)) (canvas-edge-crossed c)]
    [else (tangent-movement c)]))
;;TESTS:
(begin-for-test
  (check-equal? (update-circ-pos (make-circ 150 160 11 10)) (make-circ 161 170 11 10)
                "Circle attributes are updated properly")
  (check-equal? (update-circ-pos (make-circ 10 290 -10 -10))(make-circ 20 300 10 10)
                 "Circle attributes are updated properly")
  (check-equal? (update-circ-pos (make-circ 410 300 10 10)) (make-circ 400 290 -10 -10)
                 "Circle attributes are updated properly")
  (check-equal? (update-circ-pos (make-circ 12 310 12 12)) (make-circ 0 298 -12 -12))
  
  (check-equal? (update-circ-pos (make-circ 200 -10 12 12)) 
(make-circ 212 -22 12 -12)))
  

;; tangent-movement : Circle -> Circle
;; GIVEN: the current circle object which contains the position & velocity
;; RETURNS: the new circle object with (x,y) co-ordinates updated according to the
;;         velocity appearing as if they follow tangent path to the edge
;; EXAMPLES:(tangent-movement (make-circle 70 70 -10 10)) -> (make-circle 60 80 -10 10)
;; STRATEGY: Use template fn of Circle
(define (tangent-movement c)
  (make-circ (+ (circ-x c) (circ-vx c))
                 (+ (circ-y c) (circ-vy c))
                 (circ-vx c)
                 (circ-vy c)))        

;; crosses-corner? : Circle -> Boolean
;; GIVEN: the current circle object which contains the position & velocity
;; RETURNS: true if the co-ordinates crosses any of the 4 corners
;; EXAMPLES:
;; (crosses-corner? (make-circle 10 10 -10 10)) -> true
;; (crosses-corner? (make-circle 50 10 -10 10)) -> false
;; STRATEGY: Use template fn of Circle 
(define (crosses-corner? r)
  (or (corner? (circ-x r) (circ-y r) TOP-LEFT-CORNER)
      (corner? (circ-x r) (circ-y r) BOTTOM-LEFT-CORNER)
      (corner? (circ-x r) (circ-y r) TOP-RIGHT-CORNER)
      (corner? (circ-x r) (circ-y r) BOTTOM-RIGHT-CORNER)))

;;TESTS:
(begin-for-test
  (check-equal? (crosses-corner? (make-circ 12 34 -12 14)) true
                "Circle should cross top left corner")
  (check-equal? (crosses-corner? (make-circ 390 410 -12 14)) true
                "Circle should cross bottom left corner")
  (check-equal? (crosses-corner? (make-circ 410 10 -12 14)) true
                "Circle should cross top right corner")
  (check-equal? (crosses-corner? (make-circ 410 10 -12 14)) true
                "Circle should cross top right corner")
  (check-equal? (crosses-corner? (make-circ 310 300 -12 14)) false
                "Circle does not cross bottom right corner")
  (check-equal? (crosses-corner? (make-circ 410 280 -12 14))true
                "Circle should cross bottom right corner")
  (check-equal? (crosses-corner? (make-circ 10 310 -12 14))true
                "Circle should cross bottom left corner"))
  
  
   

;; corner? ; x y c -> Boolean
;; GIVEN : Anchor point x,y of the circle and corner against which we need verify crossing
;; RETURNS: true if any part of circle is crossing the corner
;; EXAMPLES:
;; STRATEGY: Using template of Corner on c
(define (corner? x y c)
  (cond
    [(string=? c TOP-LEFT-CORNER) (and (<= (get-left-corner-x x) CANVAS-LEFT-CORNER-X-COORD)
                         (<= (get-top-corner-y y) CANVAS-TOP-CORNER-Y-COORD))]
    [(string=? c BOTTOM-LEFT-CORNER) (and (<= (get-left-corner-x x) CANVAS-LEFT-CORNER-X-COORD)
                         (>= (get-bottom-corner-y y) CANVAS-BOTTOM-CORNER-Y-COORD))]
    [(string=? c TOP-RIGHT-CORNER) (and (>= (get-right-corner-x x) CANVAS-RIGHT-CORNER-X-COORD)
                         (<= (get-top-corner-y y) CANVAS-TOP-CORNER-Y-COORD))]
    [(string=? c BOTTOM-RIGHT-CORNER) (and (>= (get-right-corner-x x) CANVAS-RIGHT-CORNER-X-COORD)
                         (>= (get-bottom-corner-y y) CANVAS-BOTTOM-CORNER-Y-COORD))]))

;; get-right-corner-x : PosInt -> PosInt
;; GIVEN: anchor point x co-ordinate of the circle
;; RETURNS: the x co-ordinate of the circle on the right edge
;; EXAMPLES:
;; STRATEGY: Combine Simple functions
(define (get-right-corner-x x)
  (+ x DIST-TO-EDGE-FM-CENTRE-X))

;; get-left-corner-x : PosInt -> PosInt
;; GIVEN: anchor point x co-ordinate of the circle
;; RETURNS: the x co-ordinate of the circle on the left edge
;; EXAMPLES:
;; STRATEGY: Combine Simple functions
(define (get-left-corner-x x)
  (- x DIST-TO-EDGE-FM-CENTRE-X))
  
;; get-bottom-corner-y : PosInt -> PosInt
;; GIVEN: anchor point y co-ordinate of the circle
;; RETURNS: the y co-ordinate of the circle on the bottom edge
;; EXAMPLES:
;; STRATEGY: Combine Simple functions
(define (get-bottom-corner-y y)
  (+ y DIST-TO-EDGE-FM-CENTRE-Y))

;; get-top-corner-y-coord : PosInt -> PosInt
;; GIVEN: anchor point y co-ordinate of the circle
;; RETURNS: the y co-ordinate of the circle on the top edge
;; EXAMPLES:
;; STRATEGY: Combine Simple functions
(define (get-top-corner-y y)
  (- y DIST-TO-EDGE-FM-CENTRE-Y))


;; crosses-canvas-edge? : Integer Integer -> Boolean
;; GIVEN:x and y coordinates of Circle which has to be checked
         ;;against the canvas boundary
;; RETURNS: true if the circle crosses canvas boundary
;; EXAMPLES:
;; (crosses-canvas-edge? 10 10) -> true
;; (crosses-canvas-edge? 10 290) -> true
;; STRATEGY: Combine Simple functions
(define (crosses-canvas-edge? x y)
  (or (crosses-edge? x y RIGHT-EDGE) 
      (crosses-edge? x y LEFT-EDGE)
      (crosses-edge? x y TOP-EDGE)
      (crosses-edge? x y DOWN-EDGE)))

;;TESTS:
(begin-for-test
  (check-equal? (crosses-canvas-edge? 410 310) true "Should Cross right edge")
  (check-equal? (crosses-canvas-edge? 410 -10) true "Should Cross top edge")
  (check-equal? (crosses-canvas-edge? -10 100) true "Should Cross left edge")
  (check-equal? (crosses-canvas-edge? 10 310) true "Should Cross bottom edge"))
  

;; crosses-edge? : Integer Integer Edge -> Boolean
;; GIVEN: the x,y co-ordinates representing the position of the circle in the scene
;; RETURNS: true if the co-ordinates are beyond the edge boundary
;; EXAMPLES:
;; (crosses-edge? 10 10 "left") => true
;; (crosses-edge? 10 10 "right") => false
;; (crosses-edge? 10 10 "top") => true
;; (crosses-edge? 10 290 "bottom") => true
;; STRATEGY: Use template fn of Edge on e
(define (crosses-edge? x y e)
  (cond
     [(string=? e RIGHT-EDGE) (>= (+ x DIST-TO-EDGE-FM-CENTRE-X) CANVAS-WIDTH)]
     [(string=? e LEFT-EDGE) (<= (- x DIST-TO-EDGE-FM-CENTRE-X) 0)]
     [(string=? e TOP-EDGE) (<= (- y DIST-TO-EDGE-FM-CENTRE-Y) 0)]
     [(string=? e DOWN-EDGE) (>= (+ y DIST-TO-EDGE-FM-CENTRE-Y) CANVAS-HEIGHT)]))

;;canvas-corner-crossed: Circle -> Circle
;; GIVEN: the current circle which contains the position & velocity
;; RETURNS: the circle with (x,y) co-ordinates updated according to the velocity
;; EXAMPLES:(canvas-corner-crossed (make-circle 10 10 -10 10)) ->
                                  ;; (make-circle 20 0 10 -10)
;; STRATEGY: Using template function of Circle
(define (canvas-corner-crossed c)
  (make-circ (+ (circ-x c) (change-sign (circ-vx c)))
                 (+ (circ-y c) (change-sign (circ-vy c)))
                 (change-sign (circ-vx c))
                 (change-sign (circ-vy c))))
;;TESTS:
(begin-for-test
  (check-equal? (canvas-corner-crossed (make-circ 10 10 -10 10))
(make-circ 20 0 10 -10))
  (check-equal?(canvas-corner-crossed (make-circ 380 290 -10 10))
(make-circ 390 280 10 -10))
  (check-equal?(canvas-corner-crossed (make-circ 380 10 10 -10))
(make-circ 370 20 -10 10))
  (check-equal?(canvas-corner-crossed (make-circ 10 290 10 10))
(make-circ 0 280 -10 -10))
  (check-equal?(canvas-corner-crossed (make-circ 10 290 -10 -10))
(make-circ 20 300 10 10)))
  
  
 ;;change-sign: Integer -> Integer
;; GIVEN: Integer
;; RETURNS: toggled value by sign of the given integer
;; EXAMPLES:(sign -1) -> 1
;; STRATEGY: Combine Simple functions
(define (change-sign i)
  (* -1 i))


;; canvas-edge-crossed: Circle -> Circle
;; GIVEN: the circle which has the current position & velocity details
;; RETURNS: new circle with updated co-ordinates & velocity
;; EXAMPLES:(canvas-edge-crossed (make-circle 10 50 -10 10) ->
;;                               (make-circle 20 60 10 10)
;; STRATEGY: Dividing into cases based on the edge crossed
(define (canvas-edge-crossed c)
  (cond
     [(crosses-edge? (circ-x c) (circ-y c) LEFT-EDGE) (edge-crossed c LEFT-EDGE)]
     [(crosses-edge? (circ-x c) (circ-y c) RIGHT-EDGE) (edge-crossed c RIGHT-EDGE)]
     [(crosses-edge? (circ-x c) (circ-y c) TOP-EDGE) (edge-crossed c TOP-EDGE)]
     [(crosses-edge? (circ-x c) (circ-y c) DOWN-EDGE) (edge-crossed c DOWN-EDGE)]))
;;TESTS:
(begin-for-test
  (check-equal? (canvas-edge-crossed (make-circ -10 310 -10 -10))
                (make-circ 0 300 10 -10))
  (check-equal? (canvas-edge-crossed (make-circ 380 -10 -10 10))
(make-circ 390 0 10 10))
(check-equal? (canvas-edge-crossed (make-circ 400 310 10 -10))
                (make-circ 390 300 -10 -10))
(check-equal? (canvas-edge-crossed (make-circ 310 310 -10 -10))
                (make-circ 300 320 -10 10))

  (check-equal? (canvas-edge-crossed (make-circ 10 -1 -10 10))
(make-circ 20 9 10 10)))
  
  

;;edge-crossed: Circle Edge -> Circlele
;; GIVEN: the Circle object and Edge which it has crossed
;; RETURNS: new circle with updated x co-ordinates & velocity
;; EXAMPLES:(edge-crossed (make-circle 10 50 -10 10) -> (make-circle 20 60 10 10)
;; STRATEGY: Using template of Circle
(define (edge-crossed c e)
  (make-circ (+ (circ-x c) (toggle-x-vel (circ-vx c) e))
             (+ (circ-y c) (toggle-y-vel (circ-vy c) e))
             (toggle-x-vel (circ-vx c) e)
             (toggle-y-vel (circ-vy c) e)))
;;TESTS:


;; toggle-x-vel: Integer Edge -> Integer
;; GIVEN: Velocity in x-coordinate and the Edge
;; RETURNS: toggles the velocity if it is left or right edge
;; EXAMPLE:
;; (toggle-x-vel 10 "left") -> -10
;; (toggle-x-vel 10 "right") -> 10
;; STRATEGY:
(define (toggle-x-vel vx e)
  (cond
      [(string=? e LEFT-EDGE) (change-sign vx)]
      [(string=? e RIGHT-EDGE) (change-sign vx)]
     [(string=? e TOP-EDGE) vx]
      [(string=? e DOWN-EDGE) vx]))

;; toggle-y-vel: Integer Edge -> Integer
;; GIVEN: Velocity changes in y-coordinate and the Edge
;; RETURNS: toggles the velocity if it is top or down edge
;; EXAMPLE:
;; (toggle-y-vel 10 "top") -> 10
;; (toggle-y-vel 10 "down") -> -10
;; STRATEGY:
(define (toggle-y-vel vy e)
  (cond
     [(string=? e LEFT-EDGE) vy]
      [(string=? e RIGHT-EDGE) vy]
      [(string=? e TOP-EDGE) (change-sign vy)]
      [(string=? e DOWN-EDGE) (change-sign vy)]))


;; world-after-key-event : WorldState KeyEvent -> WorldState
;;GIVEN:current WorldState and keyevent
;; RETURNS: the WorldState that should follow the given worldstate
;; after the given keyevent
;; EXAMPLES:
;; (world-after-key-event (make-world (make-circle 15 10 23 13) (make-circle 20 15 23 13) UNPAUSED) " ")
;;                          -> (make-world (make-circle 15 10 23 13) (make-circle 20 15 23 13 ) PAUSED)
;;(world-after-key-event (make-world (make-circle 15 10 23 13) (make-circle 20 15 23 13) PAUSED) "\n ")
;;                          -> (make-world (make-circle 15 10 23 13) (make-circle 20 15 23 13 ) PAUSED)
;;
;; STRATEGY: Dividing into cases based on KeyEvent Occured
(define (world-after-key-event w ke)
  (if (pause-key-event? ke)
      (update-scene-after-event w)
      w))

;;TESTS:
(begin-for-test
  (check-equal? (world-after-key-event (make-world (make-circ 110 100 10 10)
   (make-circ 100 200 10 10) UNPAUSED) " ") (make-world (make-circ 110 100 10 10)
                                                        (make-circ 100 200 10 10) #true))

(check-equal? (world-after-key-event (make-world (make-circ 110 100 10 10)
   (make-circ 100 200 10 10) PAUSED) "\n")
(make-world (make-circ 110 100 10 10) (make-circ 100 200 10 10) #true)))



 
;; pause-key-event? : KeyEvent -> Boolean
;; GIVEN: a KeyEvent
;; RETURNS: true iff the KeyEvent represents a pause instruction
;; EXAMPLES:
;; (pause-key-event? " ") -> true
;; (pause-key-event? "\n") -> false
;; STRATEGY: Combine simple functions
(define (pause-key-event? ke)
  (key=? ke " "))


;; update-scene-after-event : WorldState -> WorldState
;; GIVEN: WorldState with details of both circles and
;;        whether it is paused or not.
;; RETURNS: the world state that should follow the
;;          given world state with paused state updated.
;; EXAMPLES:(update-scene-after-event (make-world (make-circle 10 10 10 10) (make-circle 10 20 10 10) UNPAUSED))
;;                          -> (make-world (make-circle 10 10 10 10) (make-circle 10 20 10 10) PAUSED)
;; (update-scene-after-event (make-world (make-circle 10 10 10 10) (make-circle 10 20 10 10) PAUSED))
;;                          -> (make-world (make-circle 10 10 10 10) (make-circle 10 20 10 10) UNPAUSED)
;; STRATEGY: Use template of WorlState on w
(define (update-scene-after-event w)
  (make-world (make-circ (circ-x (world-circ1 w)) (circ-y (world-circ1 w))
                         (circ-vx (world-circ1 w)) (circ-vy (world-circ1 w)))
              (make-circ (circ-x (world-circ2 w)) (circ-y (world-circ2 w))
                         (circ-vx (world-circ2 w)) (circ-vy (world-circ2 w)))
              (not (world-paused? w))))

;; get-circ-text: Int Int -> String
;; GIVEN: velocity details
;; RETURNS: a string in the format "(vx,vy)" - vx, vy are the velocity
;;          of the circle
;; EXAMPLES:(get-circ-text -23 -15) -> "(-23,-15)"
;; STRATEGY: Combine Simple functions
(define (get-circ-text vx vy)
  (string-append "("
                 (number->string vx)
                 ","
                 (number->string vy)
                 ")"))

;;TESTS:
(begin-for-test
  (check-equal? (get-circ-text  -23 -15) "(-23,-15)")
    "Should display given values in form (vx,vy)")

;; create-circle: Integer Integer -> Image
;; GIVEN: 2 Integer values(velocity) which should be part of circle as a text
;; RETURNS: the circle with the given information at the center as a text 
;; EXAMPLES:(create-circle 15 10) -> (circle-with-text-15-10)
;; STRATEGY: Combine simple functions
(define (create-circle vx vy)
  (overlay CIRCLE
           (text (get-circ-text vx vy) FONT-SIZE COLOR)))
;;TESTS:
(define circ-with-text-10-10
       (overlay CIRCLE (text "(10,10)" FONT-SIZE COLOR)))
(begin-for-test
  (check-equal? (create-circle 10 10) circ-with-text-10-10
                "Circle should contain velocity (10,10)"))

;; circ-to-scene: Circle -> Scene
;; GIVEN: A Circle with the (x,y) co-ordinates
;; RETURNS: given world with the circle at (x,y) co-ordinates
;; EXAMPLES:
;; (circ-to-scene (make-circ 12 14 13 15)) -> (circ-scene-at-12-14)
;; STRATEGY: Using template of Circle
(define (circ-to-scene c)
  (place-image (create-circle (circ-vx c) (circ-vy c))
               (circ-x c) (circ-y c) (empty-scene CANVAS-WIDTH CANVAS-HEIGHT)))

;;TESTS:
(define circ-scene-at-10-10
  (place-image (create-circle 10 10) 10 10
                              (empty-scene CANVAS-WIDTH CANVAS-HEIGHT)))
(define circ-scene-at-10-20
  (place-image (create-circle 10 10)
               10 20 (empty-scene CANVAS-WIDTH CANVAS-HEIGHT)))

(begin-for-test
  (check-equal? (circ-to-scene (make-circ 10 10 10 10)) circ-scene-at-10-10))
                




;; world-to-scene : WorldState -> Scene
;; GIVEN: WorldState object which contains the details of 2 circles to represent the world
;; RETURNS: a Scene that portrays the given world with 2 circles drawn at the given co-ordinates.
;; EXAMPLE:(world-to-scene (make-world (make-circ 10 11 12 10) (make-circ 10 20 12 10)
;;                        UNPAUSED)) -> (world-scene-with-2-circles)
;; STRATEGY: Use template for World on w and template for circle on circ1 of w
(define (world-to-scene w)
  (place-image (create-circle (circ-vx (world-circ1 w)) (circ-vy (world-circ1 w)))
               (circ-x (world-circ1 w)) (circ-y (world-circ1 w))
               (circ-to-scene (world-circ2 w))))
;TESTS:
(define world-scene-with-2-circles
  (place-image circ-with-text-10-10 10 10 circ-scene-at-10-20))

(begin-for-test
 (check-equal? (world-to-scene (make-world
                 (make-circ 10 10 10 10) (make-circ 10 20 10 10) UNPAUSED))
                world-scene-with-2-circles "Scene does not match"))



 
















































