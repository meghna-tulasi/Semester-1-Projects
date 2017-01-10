;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname screensaver-2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;Screensaver
;;The screensaver consists of two circles that move inside a canvas.
;;if the circle in its normal motion it would hit or go past
;;one side of the canvas
;;if the circles hits the canvas edge, it will bounce off in the tangent direction
;;to the edge.
;;On a key event " ",both the circles pause at positions they are at that moment,
;;until another key event " " is provided.
;;on mouse-event(button down,drag) the circle selected is dragged to wherever
;;the mouse pointer moves inside the canvas.
;;to run the program
;;start with (screensaver sim-speed)

 
(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)
(require 2htdp/image)
(check-location "03" "screensaver-2.rkt")

(provide
         screensaver
         initial-world
         world-after-tick
         world-after-key-event
         world-after-mouse-event
         circ-after-mouse-event
         world-circ1
         world-circ2
         world-paused?
         new-circle
         circ-selected?
         circ-x
         circ-y
         circ-vx
         circ-vy)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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

(define PAUSED true)
(define UNPAUSED false)
(define SELECTED true)
(define UNSELECTED false)

(define RIGHT-EDGE "right")
(define LEFT-EDGE "left")
(define TOP-EDGE "top")
(define DOWN-EDGE "down")

(define BUTTON-DOWN "button-down")
(define BUTTON-UP "button-up")
(define DRAG "drag")
(define MOVE "move")
(define ENTER "enter")
(define LEAVE "leave")

(define FONT-SIZE 15)
(define UNSELECTED-COLOR "blue")
(define SELECTED-COLOR "red")
(define MODE "outline")
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))
(define MODE1 "solid")
(define SRADIUS 5)
(define SMALL-CIRC (circle SRADIUS MODE1 SELECTED-COLOR))
(define SELECTED-CIRC (circle CIRC-RADIUS1 MODE SELECTED-COLOR))
(define UNSELECTED-CIRC (circle CIRC-RADIUS1 MODE UNSELECTED-COLOR))

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

;;template
;;world-fn:WorldState -> ??
#;(define (world-fn w)
         (...
          (world-circ1 w)
          (world-circ2 w)
          (world-paused? w)))
     

(define-struct circ(x y vx vy selected? dsx dsy))
;; A Circle is a
;; (make-circle NonNegInt NonNegInt Int Int Boolean NonInt NonInt)
;; INTERPRETATION:
;; x represents the x-coordinate of center of circle in computer-graphics co-ordinate system.
;; y represents the y-coordinate of center of circle in computer-graphics co-ordinate system.
;; vx represents the velocity of the circle per tick along x-axis.
;; - A positive value indicates movement towards right, negative value indicates left.
;; vy represents the velocity of the circle per tick along y-axis.
;; - A positive value indicates movement towards down, negative value indicates up.
;;selected? is a boolean representing if the circle is selected by a mouse click or not
;;dsx is the distance of x-coordinate of circle from selection point in the scene
;;dsy is the distance of y-coordinate of circle from selection point in the scene
;;template
;; circle-fn: Circle -> ??
#;(define (circ-fn c)
     (...
      circ-x c
      circ-y c
      circ-vx c
      circ-vy c
      circ-selected?
      circ-dsx
      circ-dsy))



;; A MouseEvent is one of
;; "button-down"
;; "button-up"
;; "drag"
;; "move"
;; "enter"
;; "leave"
;; INTERPRETATION: represents one of the mouse actions
;;template:
;; mouse-event-fn : MouseEvent -> ??
#;(define (mouse-event-fn mev)
    (cond
      [(mouse=? BUTTON-DOWN mev) ...]
      [(mouse=? BUTTON-UP mev) ...]
      [(mouse=? DRAG mev) ...]
      [(mouse=? MOVE mev) ...]
      [(mouse=? ENTER mev) ...]
      [(mouse=? LEAVE mev) ...]))


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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
              (on-mouse world-after-mouse-event)
              (on-draw world-to-scene)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;initial-world : Any -> WorldState
;;GIVEN: any value (ignored)
;;RETURNS:the initial world specified in the problem set
;;circle1(200,100) with velocity (-12,20)
;;circle2(200,200) with velocity (23,-14)
;;Initially, world will be paused.
;;EXAMPLES:
;;(initial-world "start") -> (make-world (make-circ 200 100 -12 20 UNSELECTED 0 0)
;;                                       (make-circ 200 200 23 -14 UNSELECTED 0 0) true)
;;DESIGN-STRATEGY: Using Constructor template of World
(define (initial-world any)
  (make-world (make-circ CIRC1-X CIRC1-Y CIRC1-VX CIRC1-VY UNSELECTED 0 0)
              (make-circ CIRC2-X CIRC2-Y CIRC2-VX CIRC2-VY UNSELECTED 0 0) PAUSED))
;;TESTS:
(begin-for-test
  (check-equal? (initial-world "start") (make-world (make-circ 200 100 -12 20 UNSELECTED 0 0)
                 (make-circ 200 200 23 -14 UNSELECTED 0 0) #true))
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
  (make-circ x y vx vy UNSELECTED 0 0))

;;TESTS:
(begin-for-test
  (check-equal? (new-circle 12 15 -23 10) (make-circ 12 15 -23 10 UNSELECTED 0 0)
                "new-circle should create circle at given circle's position."))
                
;; create-scene-with-selection-circle : Boolean Int Int Scene -> Scene
;; GIVEN: Boolean which represents selection of circle, x,y coordinates of selection & initial scene
;; RETURNS: updated Scene with the selected circle at x,y on top of initial scene
;; EXAMPLES:
;; (create-scene-with-selection-circle true 10 10 EMPTY-CANVAS) -> empty-canvas-with-circle-10-10
;; STRATEGY: Combine simple functions
(define (create-scene-with-selection-circle selected? mx my scene)
  (if selected?
      (place-image (circle SRADIUS MODE SELECTED-COLOR) mx my scene)
      scene))

;; image-color : Boolean -> String
;; GIVEN: Boolean which represents true if the circle is selected
;; RETURNS: red if true else blue
;; EXAMPLES:
;; (image-color true) -> "red"
;; (image-color false) -> "blue"
;; STRATEGY: Combine simple functions
(define (image-color selected?)
  (if selected?
      SELECTED-COLOR
      UNSELECTED-COLOR))

;; circ-image  : Boolean -> Image
;; GIVEN: Boolean which represents true if the circle is selected
;; RETURNS: red circle Image if true else blue
;; EXAMPLES:
;; (circ-image true) -> SELECTED-CIRC
;; (circ-image false) -> UNSELECTED-CIRC
;; STRATEGY: Combine simple functions
(define (circ-image selected?)
  (if selected?
      SELECTED-CIRC
      UNSELECTED-CIRC))

;; world-after-tick : WorldState -> WorldState
;; GIVEN: the current WorldState with details of both circle
;; and whether it is paused or not.
;; RETURNS: the world state that should follow the given world state
;; after a tick.
 ;; STRATEGY: Divide into cases based on paused state                             
(define (world-after-tick w)
  (if (world-paused?  w) w
(update-after-tick w)))      

;;TESTS:

(define unpaused-scene-at-60-60-70-70 (make-world (make-circ 60 60 10 10 UNSELECTED 0 0)
                                                  (make-circ 70 70 10 10 UNSELECTED 0 0)
                                                  UNPAUSED))
(define paused-scene-at-10-10-10-20 (make-world (make-circ 10 10 10 10 UNSELECTED 0 0)
                                                (make-circ 10 20 10 10 UNSELECTED 0 0)
                                                PAUSED))
  
(begin-for-test
  (check-equal? (world-after-tick (make-world
                  (make-circ 50 50 10 10 UNSELECTED 0 0) (make-circ 60 60 10 10 UNSELECTED 0 0)
                  UNPAUSED))
                    unpaused-scene-at-60-60-70-70)
(check-equal?(world-after-tick (make-world
                  (make-circ 10 10 10 10 UNSELECTED 0 0) (make-circ 10 20 10 10 UNSELECTED 0 0)
                  PAUSED))
            paused-scene-at-10-10-10-20))
;; update-after-tick : WorldState -> WorldState
;; GIVEN: the current WorldState with details of both circles
;; RETURNS: the new World State with updated co-ordinates of both circles
;;along with their velocities.
;; EXAMPLE:(update-after-tick (make-world (make-circle 10 4 -17 5 UNSELECTED 0 0)
;;  (make-circle 10 20 10 10 UNSELECTED 0 0) false)) -> (unpaused-scene-at-20-20-20-30)
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
    [(circ-selected? c) c]
    [(crosses-corner? c) (canvas-corner-crossed c)]
    [(crosses-canvas-edge? (circ-x c) (circ-y c)) (canvas-edge-crossed c)]
    [else (tangent-movement c)]))
;;TESTS:
(begin-for-test
  (check-equal? (update-circ-pos (make-circ 150 160 11 10 UNSELECTED 0 0))
   (make-circ 161 170 11 10 UNSELECTED 0 0) "Circle attributes are updated properly")
  (check-equal? (update-circ-pos (make-circ 10 290 -10 -10 UNSELECTED 0 0))
                (make-circ 20 300 10 10 UNSELECTED 0 0)
                 "Circle attributes are updated properly")
  (check-equal? (update-circ-pos (make-circ 410 300 10 10 UNSELECTED 0 0))
                (make-circ 400 290 -10 -10 UNSELECTED 0 0)
                 "Circle attributes are updated properly")
  (check-equal? (update-circ-pos (make-circ 12 310 12 12 SELECTED 0 0))
                (make-circ 12 310 12 12 SELECTED 0 0))
  
  (check-equal? (update-circ-pos (make-circ 200 -10 12 12 UNSELECTED 0 0)) 
(make-circ 212 -22 12 -12 UNSELECTED 0 0)))
  

;; tangent-movement : Circle -> Circle
;; GIVEN: the current circle object which contains the position & velocity
;; RETURNS: the new circle object with (x,y) co-ordinates updated according to the
;;         velocity appearing as if they follow tangent path to the edge
;; EXAMPLES:(tangent-movement (make-circle 70 70 -10 10)) -> (make-circle 60 80 -10 10)
;; STRATEGY: Use template fn of Circle on r
(define (tangent-movement c)
  (make-circ (+ (circ-x c) (circ-vx c))
                 (+ (circ-y c) (circ-vy c))
                 (circ-vx c)
                 (circ-vy c)
                  (circ-selected? c)
             (circ-dsx c)
             (circ-dsy c)))
      

;; crosses-corner? : Circle -> Boolean
;; GIVEN: the current circle object which contains the position & velocity
;; RETURNS: true if the co-ordinates crosses any of the 4 corners
;; EXAMPLES:
;; (crosses-corner? (make-circle 10 10 -10 10)) -> true
;; (crosses-corner? (make-circle 50 10 -10 10)) -> false
;; STRATEGY: Use template fn of Circle on r
(define (crosses-corner? r)
  (or (corner? (circ-x r) (circ-y r) TOP-LEFT-CORNER)
      (corner? (circ-x r) (circ-y r) BOTTOM-LEFT-CORNER)
      (corner? (circ-x r) (circ-y r) TOP-RIGHT-CORNER)
      (corner? (circ-x r) (circ-y r) BOTTOM-RIGHT-CORNER)))

;;TESTS:
(begin-for-test
  (check-equal? (crosses-corner? (make-circ 12 34 -12 14 UNSELECTED 0 0)) true
                "Circle should cross top left corner")
  (check-equal? (crosses-corner? (make-circ 390 410 -12 14 UNSELECTED 0 0)) true
                "Circle should cross bottom left corner")
  (check-equal? (crosses-corner? (make-circ 410 10 -12 14 UNSELECTED 0 0)) true
                "Circle should cross top right corner")
  (check-equal? (crosses-corner? (make-circ 410 10 -12 14 UNSELECTED 0 0)) true
                "Circle should cross top right corner")
  (check-equal? (crosses-corner? (make-circ 310 300 -12 14 UNSELECTED 0 0)) false
                "Circle does not cross bottom right corner")
  (check-equal? (crosses-corner? (make-circ 410 280 -12 14 UNSELECTED 0 0))true
                "Circle should cross bottom right corner")
  (check-equal? (crosses-corner? (make-circ 10 310 -12 14 UNSELECTED 0 0))true
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
                 (change-sign (circ-vy c))
                 (circ-selected? c)
             (circ-dsx c)
             (circ-dsy c)))

;;TESTS:
(begin-for-test
  (check-equal? (canvas-corner-crossed (make-circ 10 10 -10 10 UNSELECTED 0 0))
(make-circ 20 0 10 -10 UNSELECTED 0 0))
  (check-equal?(canvas-corner-crossed (make-circ 380 290 -10 10 UNSELECTED 0 0))
(make-circ 390 280 10 -10 UNSELECTED 0 0))
  (check-equal?(canvas-corner-crossed (make-circ 380 10 10 -10 UNSELECTED 0 0))
(make-circ 370 20 -10 10 UNSELECTED 0 0))
  (check-equal?(canvas-corner-crossed (make-circ 10 290 10 10 UNSELECTED 0 0))
(make-circ 0 280 -10 -10 UNSELECTED 0 0))
  (check-equal?(canvas-corner-crossed (make-circ 10 290 -10 -10 UNSELECTED 0 0))
(make-circ 20 300 10 10 UNSELECTED 0 0)))
  
  
  
  




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
;; STRATEGY: Dividing into cases based on the canvas edge crossed
(define (canvas-edge-crossed c)
  (cond
     [(crosses-edge? (circ-x c) (circ-y c) LEFT-EDGE) (edge-crossed c LEFT-EDGE)]
     [(crosses-edge? (circ-x c) (circ-y c) RIGHT-EDGE) (edge-crossed c RIGHT-EDGE)]
     [(crosses-edge? (circ-x c) (circ-y c) TOP-EDGE) (edge-crossed c TOP-EDGE)]
     [(crosses-edge? (circ-x c) (circ-y c) DOWN-EDGE) (edge-crossed c DOWN-EDGE)]))
;;TESTS:
(begin-for-test
  (check-equal? (canvas-edge-crossed (make-circ -10 310 -10 -10 UNSELECTED 0 0))
                (make-circ 0 300 10 -10 UNSELECTED 0 0))
  (check-equal? (canvas-edge-crossed (make-circ 380 -10 -10 10 UNSELECTED 0 0))
(make-circ 390 0 10 10 UNSELECTED 0 0))
(check-equal? (canvas-edge-crossed (make-circ 400 310 10 -10 UNSELECTED 0 0))
                (make-circ 390 300 -10 -10 UNSELECTED 0 0))
(check-equal? (canvas-edge-crossed (make-circ 310 310 -10 -10 UNSELECTED 0 0))
                (make-circ 300 320 -10 10 UNSELECTED 0 0))

  (check-equal? (canvas-edge-crossed (make-circ 10 -1 -10 10 UNSELECTED 0 0))
(make-circ 20 9 10 10 UNSELECTED 0 0)))
  
  

;;edge-crossed: Circle Edge -> Circlele
;; GIVEN: the Circle object and Edge which it has crossed
;; RETURNS: new circle with updated x co-ordinates & velocity
;; EXAMPLES:(edge-crossed (make-circle 10 50 -10 10) -> (make-circle 20 60 10 10)
;; STRATEGY: Using template of Circle
(define (edge-crossed c e)
  (make-circ (+ (circ-x c) (toggle-x-vel (circ-vx c) e))
             (+ (circ-y c) (toggle-y-vel (circ-vy c) e))
             (toggle-x-vel (circ-vx c) e)
             (toggle-y-vel (circ-vy c) e)
             (circ-selected? c)
             (circ-dsx c)
             (circ-dsy c)))



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
;; (world-after-key-event (make-world (make-circle 15 10 23 13 UNSELECTED 0 0)
;;(make-circle 20 15 23 13 UNSELECTED 0 0) UNPAUSED) " ")
;; -> (make-world (make-circle 15 10 23 13 UNSELECTED 0 0)
;;(make-circle 20 15 23 13 UNSELECTED 0 0) PAUSED)
;;(world-after-key-event (make-world (make-circle 15 10 23 13 UNSELECTED 0 0)
;;(make-circle 20 15 23 13 UNSELECTED 0 0) PAUSED) "\n ")
;; -> (make-world (make-circle 15 10 23 13 UNSELECTED 0 0)
;;(make-circle 20 15 23 13 UNSELECTED 0 0) PAUSED)

;; STRATEGY: Dividing into cases based on KeyEvent Occured
(define (world-after-key-event w ke)
  (if (pause-key-event? ke)
      (update-scene-after-event w)
      w))

;;TESTS:

(define unpaused-scene-at-10-10-10-20
  (make-world
   (make-circ 10 10 10 10 UNSELECTED 0 0)
   (make-circ 10 20 10 10 UNSELECTED 0 0) UNPAUSED))

  
(begin-for-test
  (check-equal? ( world-after-key-event
                  (make-world (make-circ 10 10 10 10 UNSELECTED 0 0)
                              (make-circ 10 20 10 10 UNSELECTED 0 0)
                              UNPAUSED) " ")
                paused-scene-at-10-10-10-20
                "World State is not proper")
  (check-equal? ( world-after-key-event
                  (make-world (make-circ 10 10 10 10 UNSELECTED 0 0)
                              (make-circ 10 20 10 10 UNSELECTED 0 0)
                              PAUSED) " ")
                unpaused-scene-at-10-10-10-20
                "World State is not proper")
   (check-equal? ( world-after-key-event
                  (make-world (make-circ 10 10 10 10 UNSELECTED 0 0)
                              (make-circ 10 20 10 10 UNSELECTED 0 0)
                              PAUSED) "\n")
                paused-scene-at-10-10-10-20
                "World State is not proper"))
  
  
;button-down : Circle Int Int -> Circle
; GIVEN: the Circle object & Mouse coordinates
; RETURNS: Updated Circle object if mouse click is within Circle else input circle
; EXAMPLE:
; (button-down (make-circ 100 100 10 10 UNSELECTED 0 0) 120 120))
;                     -> (make-circ 100 100 10 10 SELECTED 20 20)
; (button-down (make-circ 100 100 10 10 UNSELECTED 0 0) 200 200))
;                     -> (make-circ 100 100 10 10 UNSELECTED 0 0)
; STRATEGY: Use constructor template of Circle
(define (button-down c mx my)
  (if (mouse-event-inside-circ? (circ-x c) (circ-y c) mx my)
      (make-circ (circ-x c) (circ-y c)
                 (circ-vx c) (circ-vy c) SELECTED
                 (- mx (circ-x c)) (- my (circ-y c)))
      c))
   

          
 
;;mouse-event-outside-circ?: Int Int -> Circle 
;;;GIVEN: Mouse coordinates
;;;RETURN:Circle at that mouse coordinates
;;;EXAMPLES
;;;STRATEGY:
;
;(define (mouse-event-outside-circ? x y mx my)
;  (not
;  (and
;   (and ( >= mx (- x DIST-TO-EDGE-FM-CENTRE-X)) ( <= mx (+ x DIST-TO-EDGE-FM-CENTRE-X)))
;   (and ( >= my (- y DIST-TO-EDGE-FM-CENTRE-Y)) ( <= my (+ y DIST-TO-EDGE-FM-CENTRE-Y))))))
;
;  (define (button-down-out c mx my)
;  (if (mouse-event-outside-circ? (circ-x c) (circ-y c) mx my)
;      (circle 5 "solid" "red")
;      c))
;   
;;(and
;;(and ( >= mx (- x DIST-TO-EDGE-FM-CENTRE-X)) ( <= mx (+ x DIST-TO-EDGE-FM-CENTRE-X)))
;;   (and ( >= my (- y DIST-TO-EDGE-FM-CENTRE-Y)) ( <= my (+ y DIST-TO-EDGE-FM-CENTRE-Y))))))
;;



;;drag : Circle Int Int -> Circle
;; GIVEN: the Circle object & Mouse coordinates
;; RETURNS: Updated Circle object if the circle is selected else input circle
;; EXAMPLE:
;; (drag (make-circ 100 100 10 10 UNSELECTED 0 0) 150 150))
;;                     -> (make-circ 130 130 10 10 SELECTED 20 20)
;; STRATEGY: Use constructor template of Circle
(define (drag c mx my)
  (if (circ-selected? c)
      (make-circ (- mx (circ-dsx c)) (- my (circ-dsy c))
                 (circ-vx c) (circ-vy c) SELECTED
                 (circ-dsx c) (circ-dsy c))
      c))
    
      
;; circ-after-mouse-event :  Circle Int Int MouseEvent -> Circle
;; GIVEN: A circle, the x- and y-coordinates of a mouse event, and the
;; mouse event
;; RETURNS: the circle that should follow the given circle after
;; the given mouse event
;; EXAMPLES:
;; (circ-after-mouse-event (make-circ 100 100 10 10 UNSELECTED 0 0) 120 120 BUTTON-DOWN))
;;                     -> (make-circ 100 100 10 10 SELECTED 20 20)
;; (circ-after-mouse-event (make-circ 100 100 10 10 SELECTED 20 20) 200 200 BUTTON-UP))
;;                     -> (make-circ 180 180 10 10 UNSELECTED 0 0)
;; (circ-after-mouse-event (make-circ 100 100 10 10 UNSELECTED 0 0) 150 150 DRAG))
;;                     -> (make-circ 130 130 10 10 SELECTED 20 20)
;; STRATEGY: Use template of MouseEvent on mev
(define (circ-after-mouse-event c mx my mev)
    (cond
      [(mouse=? BUTTON-DOWN mev) (button-down c mx my)]
      [(mouse=? BUTTON-UP mev) (new-circle (circ-x c) (circ-y c) (circ-vx c) (circ-vy c))]
      [(mouse=? DRAG mev) (drag c mx my)]))
      
;;TESTS:
(begin-for-test
  (check-equal? (circ-after-mouse-event (make-circ 100 100 10 10 UNSELECTED 0 0) 120 120 BUTTON-DOWN)
                (make-circ 100 100 10 10 SELECTED 20 20)
                "Circle attributes are not proper after MouseEvent Button Down")
  (check-equal? (circ-after-mouse-event (make-circ 100 100 10 10 UNSELECTED 0 0) 200 200 BUTTON-DOWN)
                (make-circ 100 100 10 10 UNSELECTED 0 0)
                "Circle attributes are not proper after MouseEvent Button Down")
   (check-equal? (circ-after-mouse-event (make-circ 160 160 10 10 SELECTED 20 20) 200 200 BUTTON-UP)
                (make-circ 160 160 10 10 UNSELECTED 0 0)
                "Circle attributes are not proper after MouseEvent Button up")
  (check-equal? (circ-after-mouse-event (make-circ 120 120 10 10 SELECTED 20 20) 150 150 DRAG)
                (make-circ 130 130 10 10 SELECTED 20 20)
                "Circle attributes are not proper after MouseEvent drag")
  (check-equal? (circ-after-mouse-event (make-circ 120 120 10 10 UNSELECTED 0 0) 150 150 DRAG)
                (make-circ 120 120 10 10 UNSELECTED 0 0)
                "Circle attributes are not proper after MouseEvent drag"))


;; mouse-event-inside-circ? : NonNegInt NonNegInt Int Int -> Boolean
;; GIVEN: the circle anchor point co-ordinates & mouse co-ordinates x,y
;; RETURNS: true if the mouse co-ordinates are within the circle
;; EXAMPLES:
;; (mouse-event-inside-circ? 50 50 60 60) -> true
;; (mouse-event-inside-circ? 50 50 200 200) -> false
;; STRATEGY: Combine simple function
(define (mouse-event-inside-circ? x y mx my)
  (and
   (and ( >= mx (- x DIST-TO-EDGE-FM-CENTRE-X)) ( <= mx (+ x DIST-TO-EDGE-FM-CENTRE-X)))
   (and ( >= my (- y DIST-TO-EDGE-FM-CENTRE-Y)) ( <= my (+ y DIST-TO-EDGE-FM-CENTRE-Y)))))

;; START-TESTS
(begin-for-test
  (check-equal? (mouse-event-inside-circ? 50 50 60 60) true "MouseEvent is inside circle")
  (check-equal? (mouse-event-inside-circ? 50 50 200 200) false "MouseEvent is outside circle"))

;;handle-mouse-event? :MouseEvent -> Boolean
;; GIVEN: MouseEvent Occured
;; RETURNS: true if the MouseEvent is one of button-down, button-up & drag , false otherwise
;; EXAMPLES:
;; (handle-mouse-event? BUTTON-DOWN) -> true
;; (handle-mouse-event? MOVE) -> false
;; STRATEGY: Use template of MouseEvent on mev
(define (handle-mouse-event? mev)
  (cond
      [(mouse=? BUTTON-DOWN mev) true]
      [(mouse=? BUTTON-UP mev) true]
      [(mouse=? DRAG mev) true]
      [(mouse=? MOVE mev) false]
      [(mouse=? ENTER mev) false]
      [(mouse=? LEAVE mev) false]))

;; START-TESTS
(begin-for-test
  (check-equal? (handle-mouse-event? BUTTON-DOWN) true "MouseEvent should be handled")
  (check-equal? (handle-mouse-event? BUTTON-UP) true "MouseEvent should be handled")
  (check-equal? (handle-mouse-event? MOVE) false "MouseEvent should not be handled")
  (check-equal? (handle-mouse-event? ENTER) false "MouseEvent should not be handled")
  (check-equal? (handle-mouse-event? LEAVE) false "MouseEvent should not be handled")
  (check-equal? (handle-mouse-event? DRAG) true "MouseEvent should not be handled"))



;; world-after-mouse-event : WorldState Int Int MouseEvent -> WorldState
;; RETURNS: the WorldState that should follow the given worldstate
;; after the given MouseEvent
;; EXAMPLES:
;; (world-after-mouse-event (make-world (make-circ 10 10 10 10 UNSELECTED 0 0)
;;                (make-circ 100 200 10 10 UNSELECTED 0 0) UNPAUSED) 7 7 "button-down")
;;                        -> (make-world (make-circ 10 10 10 10 SELECTED -3 -3)
;;                                       (make-circ 100 200 10 10 UNSELECTED 0 0) UNPAUSED)
;; (world-after-mouse-event (make-world (make-circ 10 10 10 10 SELECTED 0 0)
;;                   (make-circ 100 200 10 10 UNSELECTED 0 0) PAUSED) 7 7 "button-up")
;;                        -> (make-world (make-circ 10 10 10 10 UNSELECTED 0 0)
;;                                       (make-circ 100 200 10 10 UNSELECTED 0 0) PAUSED)
;; STRATEGY: Divide into cases based on MouseEvent Occured
(define (world-after-mouse-event w mx my mev)
  (if (handle-mouse-event? mev)
      (make-world (circ-after-mouse-event (world-circ1 w) mx my mev)
              (circ-after-mouse-event (world-circ2 w) mx my mev)
              (world-paused? w))
      w))

;; START-TESTS
(begin-for-test
  (check-equal? (world-after-mouse-event (make-world (make-circ 10 10 10 10 UNSELECTED 0 0)
                 (make-circ 100 200 10 10 UNSELECTED 0 0) UNPAUSED) 7 7 "button-down")
                (make-world (make-circ 10 10 10 10 SELECTED -3 -3)
                            (make-circ 100 200 10 10 UNSELECTED 0 0) UNPAUSED)
                "WorldState not proper after mouse event")
  (check-equal? (world-after-mouse-event (make-world (make-circ 10 10 10 10 SELECTED -3 -3)
                                          (make-circ 100 200 10 10 UNSELECTED 0 0) UNPAUSED) 7 7 "button-up")
          (make-world (make-circ 10 10 10 10 UNSELECTED 0 0)
                            (make-circ 100 200 10 10 UNSELECTED 0 0) UNPAUSED)
                "WorldState not proper after mouse event")
  (check-equal? (world-after-mouse-event (make-world (make-circ 10 10 10 10 UNSELECTED 0 0)
                                          (make-circ 100 200 10 10 UNSELECTED 0 0) UNPAUSED) 7 7 "enter")
                (make-world (make-circ 10 10 10 10 UNSELECTED 0 0)
                            (make-circ 100 200 10 10 UNSELECTED 0 0) UNPAUSED)
                "WorldState not proper after mouse event"))


;; pause-key-event? : KeyEvent -> Boolean
;;help function for key event
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
;; EXAMPLES:
;;(update-scene-after-event (make-world (make-circle 10 10 10 10 UNSELECTED 0 0)
;;(make-circle 10 20 10 10 UNSELECTED 0 0)) UNPAUSED)) -> (make-world (make-circle 10 10 10 10
;;UNSELECTED 0 0) (make-circle 10 20 10 10 UNSELECTED 0 0) PAUSED)
;; (update-scene-after-event (make-world (make-circle 10 10 10 10 UNSELECTED 0 0)
;;(make-circle 10 20 10 10 UNSELECTED 0 0) PAUSED))
;; -> (make-world (make-circle 10 10 10 10 UNSELECTED 0 0) (make-circle 10 20 10 10 UNSELECTED 0 0)
;;UNPAUSED)
;; STRATEGY: Use template of world on w
(define (update-scene-after-event w)
  (make-world (make-circ (circ-x (world-circ1 w)) (circ-y (world-circ1 w))
                         (circ-vx (world-circ1 w)) (circ-vy (world-circ1 w))
                         UNSELECTED 0 0)
              (make-circ (circ-x (world-circ2 w)) (circ-y (world-circ2 w))
                         (circ-vx (world-circ2 w)) (circ-vy (world-circ2 w))
                         UNSELECTED 0 0)
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

;; create-circle: Integer Integer Boolean -> Image
;; GIVEN: 2 Integer values(velocity) which should be part of circle as a text
;; and a boolean which represents if circle is selected
;; RETURNS: the circle with the given information at the center as a text 
;; EXAMPLES:(create-circle 15 10 true) -> (red-circle-with-text-15-10)
;; STRATEGY: Combine simple functions
(define (create-circle vx vy selected?)
  (overlay (circ-image selected?)
           (text (get-circ-text vx vy) FONT-SIZE (image-color selected?))))
;;TESTS:
(define red-circ-with-text-10-10
       (overlay SELECTED-CIRC (text "(10,10)" FONT-SIZE SELECTED-COLOR)))
(define blue-circ-with-text-10-10
       (overlay UNSELECTED-CIRC (text "(10,10)" FONT-SIZE UNSELECTED-COLOR)))
(define circ-with-text-10-12
       (overlay UNSELECTED-CIRC (text "(10,12)" FONT-SIZE UNSELECTED-COLOR)))
(begin-for-test
  (check-equal? (create-circle 10 10 UNSELECTED) blue-circ-with-text-10-10
                "Circle should contain velocity (10,10) and be in blue")
   (check-equal? (create-circle 10 10 SELECTED) red-circ-with-text-10-10
                "Circle should contain velocity (10,10) and be in red"))
  
  

;; circ-to-scene: Circle -> Scene
;; GIVEN: A Circle with the (x,y) co-ordinates
;; RETURNS: given world with the circle at (x,y) co-ordinates
;; EXAMPLES:
;; (circ-to-scene (make-circ 12 14 13 15 UNSELECTED 0 0)) -> (circ-scene-at-12-14)
;; STRATEGY: Using template of Circle
(define (circ-to-scene c)
  (place-image (create-circle (circ-vx c) (circ-vy c) (circ-selected? c))
               (circ-x c) (circ-y c)
               (create-scene-with-selection-circle (circ-selected? c)
                                                   ( + (circ-x c) (circ-dsx c))
                                                   ( + (circ-y c) (circ-dsy c))
               EMPTY-CANVAS)))

;;TESTS:
(define circ-scene-at-10-10
  (place-image (create-circle 10 10 UNSELECTED)
               10 10 EMPTY-CANVAS))
(define circ-scene-at-10-20
  (place-image (create-circle 10 10 UNSELECTED)
               10 20 EMPTY-CANVAS))
                              
                              
(define selected-circ-scene-at-10-10
  (place-image (create-circle 10 10 SELECTED)
 10 10(place-image (circle SRADIUS MODE SELECTED-COLOR) 17 17 EMPTY-CANVAS)))

(define selected-circ-scene-at-10-20
  (place-image (create-circle 10 10 SELECTED)
 10 20(place-image (circle SRADIUS MODE SELECTED-COLOR) 17 17 EMPTY-CANVAS)))


(begin-for-test
  (check-equal? (circ-to-scene (make-circ 10 10 10 10 UNSELECTED 0 0)) circ-scene-at-10-10
                "Scene should contain circle image with velocity info (10,10)")
  (check-equal? (circ-to-scene (make-circ 10 10 10 10 SELECTED 7 7)) selected-circ-scene-at-10-10
                "Scene should contain circle image with velocity info (10,10)"))
          




;; world-to-scene : WorldState -> Scene
;; GIVEN: WorldState object which contains the details of 2 circles to represent the world
;; RETURNS: a Scene that portrays the given world with 2 circles drawn at the given co-ordinates.
;; EXAMPLE:(world-to-scene (make-world (make-circ 10 11 12 10  UNSELECTED 0 0) 
;;  (make-circ 10 20 12 10 UNSELECTED 0 0)UNPAUSED)) -> (world-scene-with-2-circles)
;; STRATEGY: Use template for World on w and template for circle on circ1 of w
(define (world-to-scene w)
  (create-scene-with-selection-circle (circ-selected? (world-circ1 w))
                                      (+ (circ-x (world-circ1 w)) (circ-dsx (world-circ1 w)))
                                      (+ (circ-y (world-circ1 w)) (circ-dsy (world-circ1 w)))
                                      (create-scene-with-2-circ (world-circ1 w) (world-circ2 w))))

;TESTS:
(define world-scene-with-2-circ
  (place-image blue-circ-with-text-10-10 10 10 circ-scene-at-10-20))

(define selected-world-scene-with-2-circ
  (place-image (circle SRADIUS MODE SELECTED-COLOR) 17 17
               (place-image red-circ-with-text-10-10 10 10 circ-scene-at-10-20)))


(begin-for-test
 (check-equal? (world-to-scene (make-world
                 (make-circ 10 10 10 10 UNSELECTED 0 0)
                 (make-circ 10 20 10 10 UNSELECTED 0 0) UNPAUSED))
                world-scene-with-2-circ "Scene does not match")
 (check-equal? (world-to-scene (make-world
                 (make-circ 10 10 10 10 SELECTED 7 7)
                 (make-circ 10 20 10 10 UNSELECTED 0 0) UNPAUSED))
                selected-world-scene-with-2-circ "Scene does not match"))
 
 
;; create-scene-with-2-circ : Circle Circle -> Scene
;; GIVEN: 2 Circles to be placed on the scene
;; RETURNS: Scene with 2 Circles placed on it
;; EXAMPLES: Refer world-to-scene
;; STRATEGY: Use template of Circle 
(define (create-scene-with-2-circ c1 c2)
  (place-image (create-circle (circ-vx c1) (circ-vy c1) (circ-selected? c1))
               (circ-x c1) (circ-y c1)
               (circ-to-scene c2)))












































