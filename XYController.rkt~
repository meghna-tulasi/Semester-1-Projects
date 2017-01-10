#lang racket
(require "extras.rkt")
(require 2htdp/image)
(require 2htdp/universe)
(require "Interfaces.rkt")
(require "Model.rkt")
(require "Controller.rkt")
(require "Constants.rkt")
(require rackunit)


(provide
 XYController%)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; An XYController is a (new XYController% [model Model<%>])
;; where model is an object of the Model<%>
;; The XYController simulates the movement of the particle on a canvas

(define XYController%
  (class* Controller% (Controller<%>)


    (init-field model)  ; the model stores all the information about the particle.

    (inherit-field lo-x ; field x and y axis boundaries
                   hi-x
                   lo-y
                   hi-y)

    (inherit-field x-coordinate
                   y-coordinate)
      ;; inherit fields from super class Controller%
   (inherit-field x  ;x coordinate
                  vx ;x velocity
                  y  ;y coordinate
                  vy ;y velocity
                  mouse-xcoordinate
                  mouse-ycoordinate
                  controller-selected?
                  width
                  half-width
                  height
                  half-height)

    (super-new)
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; at initialization, register this controller with the model
    ;; Controller<%> -> Void
    ;; EFFECT : Registers the given controller to receive signal
    (send model register this)

    (set! width (+ CONTROLLERWIDTH SPACE))
    (set! height (+ CONTROLLERHEIGHT SPACE)) 
    (set! half-width (/ width 2))
    (set! half-height (/ height 2))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; controller-still-selected? : -> Boolean
    ;; RETURNS: True iff the mouse is inside
    ;; this controller
    (define/override (controller-still-selected?) controller-selected?)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; after-key-event: KeyEvent -> Void
    ;; GIVEN: a key event
    ;; RETURNS: any value that can be neglected
    ;; EFFECT: method is ignored in this controller
    (define/override (after-key-event kev) 'after-key-event)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; data-image : -> Image
    ;; GIVEN: no  value
    ;; RETURNS: an image of the data inside position controller.
    ;; STRATEGY: combine simpler functions
   (define/override (data-image)
      (overlay
       (rectangle CONTROLLERWIDTH CONTROLLERHEIGHT RECT-TYPE INNER-RECT-COLOR)
       (place-image
        (overlay
         PARTICLE-CENTER-IMG
         PARTICLE-IMG)
        x y
        (rectangle CONTROLLERWIDTH CONTROLLERHEIGHT "solid" "white"))))

     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; canvas-after-button-down: NonNegInt NonNegInt -> Void
    ;; GIVEN: the location of a button-down event
    ;; STRATEGY: Setting the controller-selected as true
    (define/public (canvas-after-button-down mx my)
      (begin
        (set! controller-selected? true)
        (send model controller-still-selected? true)
        (set! mouse-xcoordinate (- mx x))
        (set! mouse-ycoordinate (- my y))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; canvas-after-button-up: NonNegInt NonNegInt -> Void
    ;; GIVEN: the location of a button-up event
    ;; STRATEGY: Setting the controller-selected as false
     (define/public (canvas-after-button-up mx my)
        (begin
          (set! controller-selected? false)
          (send model controller-still-selected? false)
          (set! mouse-xcoordinate 0)
          (set! mouse-ycoordinate 0)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; canvas-after-drag: NonNegInt NonNegInt -> Void
    ;; GIVEN: the location of a after-drag event
    (define/public (canvas-after-drag mx my)
       (if controller-selected?
           (begin
           (send model execute-command
                 (make-set-xposition
                  (within-limits lo-x (- mx mouse-xcoordinate) hi-x)))
           (send model execute-command
                 (make-set-yposition
                  (within-limits lo-y (- my mouse-ycoordinate) hi-y))))
          
           0)) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; within-limits: Integer Integer Integer -> Integer
    ;; GIVEN: a coordinate value and its lo and hi values
    ;; RETURNS: the same value if it is within bounds, else value of the boundary if crossed
    ;; DESIGN STRATEGY: Combine simpler functions
    
    (define (within-limits lo val hi) 
      (max lo (min val hi)))))
    

    
;;;;;;;; TESTS ;;;;;;;;



(begin-for-test
  (local
    ((define m (new Model%))
     (define xycontroller (new XYController% [model m])))
    (send xycontroller after-key-event "right")
    (check-equal?
     (send xycontroller for-test:get-particle-y) 50)
    (send xycontroller canvas-after-button-down 310 260)
    (send xycontroller canvas-after-drag 350 290)
    (send xycontroller canvas-after-button-up 350 290)
    (send xycontroller data-image)
    (send xycontroller controller-still-selected?)
    (check-equal?
     (send xycontroller for-test:get-particle-x) 115)))
