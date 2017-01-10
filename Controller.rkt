#lang racket

(require "extras.rkt")
(require rackunit)
(require 2htdp/image)
(require 2htdp/universe)
(require "Interfaces.rkt")
(require "PerfectBounce.rkt")
(require "Constants.rkt") 


(provide
 Controller%)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A Controller% is a
;; (new Controller% [model Model<%>]
;;      [x-coordinate Integer]
;;      [y-coordinate Interger])
;; INTERPRETATION:
;; The model is an object of the Model<%> and
;; stores all the data related to the particle 
;; x is the x-coordinate of the center of the controller
;; y is the y-coordinate of the center of the controller
(define Controller%
  (class* object% (Controller<%>)

  
    (init-field
     [x-coordinate CANVAS-XCENTER] ; Intializing center of the controller
     [y-coordinate CANVAS-YCENTER])

    
     ;; boundaries of the field
    (field [lo-x 0]) ; x lower boundary
    (field [hi-x 150]) ; x higher boundary
    (field [lo-y 0]) ; y lower boundary
    (field [hi-y 100]) ; y higher boundary

    ;; position and velocity of the particle
    (field [x 0]) ; x-position of particle
    (field [y 0]) ; y-position of particle
    (field [vx 0]) ; velocity of particle in x direction
    (field [vy 0]) ; velocity of particle in y direction

    (field [mouse-xcoordinate 0]) ; Intializing mouse coordinates
    (field [mouse-ycoordinate 0])

    (field [handle-selected? false]) ; Intializing handle-selected? to false
    (field [controller-selected? false])  ; Intializing controller-selected? to false

    (field [width 0])    ; Intializing width and height of the controller
    (field [height 0])
    
    (field [half-width 0]) ; Intializin half width of the rectangle
    (field [half-height 0]) ; Intializin half height of the rectangle

    (super-new)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ;; receive-signal : Signal -> Void
    ;; GIVEN : receives a signal from the model and adjusts controller accordingly
    ;; EFFECT : updates data in the controller from the signal received.
     (define/public (receive-signal signal)
       (if (particle? signal)
       (begin
        (set! x (particle-x signal))
        (set! y (particle-y signal))
        (set! vx (particle-vx signal)) 
        (set! vy (particle-vy signal)))
       ELSE))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;; after-button-up : Integer Integer -> Void
    ;; GIVEN: an (x,y) location of mouse
    ;; EFFECT : handle is set unselected and if mouse event is inside handle
    ;; then the controller can be dragged with the help of handle.
     (define/public (after-button-up mx my)
        (cond
        [(inside-handle? mx my) (set! handle-selected? false)]
        [(inside-canvas? mx my) (send this canvas-after-button-up mx my)]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         
    ; after-button-down : Integer Integer -> Void
    ; GIVEN: the location of a button-down event
    ; EFFECT: makes the viewer selected
    ; The viewer stays selected until a button down somewhere else
    ; STRATEGY: Cases on whether the event is in this object
    (define/public (after-button-down mx my)
      (cond
        [(send this inside-handle? mx my)
         (begin
           (set! handle-selected? true)
           (set! mouse-xcoordinate (- mx x-coordinate))
           (set! mouse-ycoordinate (- my y-coordinate)))]
        [(send this inside-canvas? mx my)
        (send this canvas-after-button-down mx my)]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;; inside-handle? : Int Int -> Boolean
    ;; GIVEN: the x and y coordinates of the mouse
    ;; RETURNS: true iff the x and y coordinates of
    ;; the mouse are inside the handle of the X controller
   (define/public (inside-handle? mx my)
      (and
       (<= (- x-coordinate (/ width 2)) mx (+ (- x-coordinate (/ width 2)) HANDLE))
       (<= (- y-coordinate (/ height 2)) my (+ (- y-coordinate (/ height 2)) HANDLE))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;; inside-canvas? : Int Int -> Boolean
    ;; GIVEN: the x and y coordinates of the mouse
    ;; RETURNS: true iff the x and y coordinates of
    ;; the mouse are inside the handle of the Y contoller
    (define/public (inside-canvas? mx my)
      (and
       (<= (- x-coordinate (/ width 2)) mx (+ x-coordinate (/ width 2)))
       (<= (- y-coordinate (/ height 2)) my (+ y-coordinate (/ height 2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   
    ; after-drag : Integer Integer -> Void
    ; GIVEN: the location of a drag event
    ; STRATEGY: Cases on whether this is selected.
    ; If it is selected, move it so that the vector from its position to
    ; the drag event is equal to mouse coordinates. 
    (define/public (after-drag mx my)
     (cond
       [handle-selected? 
        (begin
          (set! x-coordinate (- mx mouse-xcoordinate))
          (set! y-coordinate (- my mouse-ycoordinate)))]
       [controller-selected?
        (send this canvas-after-drag mx my)]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   
    ; after-move : Integer Integer -> Widget
    ; GIVEN: a location
    ; RETURNS: the state of this object that should follow the
    ; specified mouse event at the given location.
    (define/public (after-move mx my)
      'controller-after-move-value) 
       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;; KeyEvent -> Void
    ;; GIVEN: a KeyEvent
    ;; EFFECT: Swidget updated after the given keyevent
    (abstract after-key-event)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;; -> Image
    ;; GIVEN: no  value
    ;; RETURNS: an image of the data inside this
    ;; controller.
    (abstract data-image)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;; -> Void
    ;; EFFECT : Updates model after tick
    (define/public (after-tick) 'after-tick-trap)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;; Scene -> Scene
    ;; GIVEN: a Scene
    ;; RETURNS: a new Scene
    (define/public (add-to-scene scene)
      (place-image (controller-image) x-coordinate y-coordinate
                   (place-image (square HANDLE "outline" (get-color))
                                (+ (- x-coordinate half-width) (/ HANDLE 2))
                                (+ (- y-coordinate half-height) (/ HANDLE 2))
                                scene)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;      

    ;; get-color : -> String
    (define (get-color)
      (if handle-selected?
          "red"
          "black"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; controller-image: -> Image
    ;; GIVEN: no value
    ;; RETURNS: an image of the X
    ;; controller on the canvas.
    ;; STRATEGY: combine simpler functions 
    (define/public (controller-image) 
      (let ((the-data-image (send this data-image)))
        (begin
          (controller-width  the-data-image)
          (controller-height the-data-image)
          (overlay 
           the-data-image
           (rectangle
            width
            height
            "outline"
            "black")))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
    ;; controller-width: (-> Image) -> Void
    ;; GIVEN: a function that takes no
    ;; input and returns an image
    ;; RETURNS: no value
    ;; EFFECT: updates the width of the given image 
    (define (controller-width the-data-image)
      (begin
        (set! width (max width (+ (image-width the-data-image) HANDLE)))
        (set! half-width (/ width 2))
        GAP))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
    ;; controller-height: (-> Image) -> Void
    ;; GIVEN: a function that takes no
    ;; input and returns an image
    ;; RETURNS: no value
    ;; EFFECT: updates the height of the given image
    (define (controller-height the-data-image)
      (begin
        (set! height (max height (+ (image-height the-data-image) HANDLE)))
        (set! half-height (/ height 2))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; controller-still-selected? : -> Boolean
    ;; RETURNS: True if the mouse is inside
    ;; controller or controller handle.
    ;; DETAILS: In this case it is false
    (define/public (controller-still-selected?)
     false)


    ;; -> Real
    ;; Returns the particle's x coordinate value
    (define/public (for-test:get-particle-x)
      x)

    ;; -> Real
    ;; Returns the particle's y coordinate value
    (define/public (for-test:get-particle-y) 
      y)

    ;; -> Integer
    ;; Returns the particle's x velocity value
    (define/public (for-test:get-particle-vx)
      vx)

    ;; -> Integer
    ;; Returns the particle's y velocity value
    (define/public (for-test:get-particle-vy)
      vy
    )))




    
    