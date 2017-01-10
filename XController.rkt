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
 XController%)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; XController is a (new XController% [model Model<%>])

(define XController%
  (class* Controller% (Controller<%>)
    
    
    (init-field model)  ; the model stores all the information about the particle.
    
    (inherit-field lo-x
                   hi-x) ; field x-axis boundaries
    
    (inherit-field x-coordinate
                   y-coordinate)
    ;; inherit fields from super class Controller%
    (inherit-field x  ;x coordinate
                   vx ;x velocity
                   mouse-xcoordinate
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
    
    (set! width (+ CONTROLLERWIDTH SPACE)) ;; width is the width of the rectangle 
    (set! height SPACE)      ;; height is the height of the rectangle 
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
       (rectangle CONTROLLERWIDTH 40 RECT-TYPE INNER-RECT-COLOR)
       (place-image
        (overlay
         PARTICLE-CENTER-IMG
         PARTICLE-IMG)
        x 20 
        (rectangle CONTROLLERWIDTH 40 "solid" "white"))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; canvas-after-button-down: NonNegInt NonNegInt -> Void
    ;; GIVEN: the location of a button-down event
    ;; STRATEGY: Setting the controller-selected as true
    (define/public (canvas-after-button-down mx my)
      (begin
        (set! controller-selected? true)
        (send model controller-still-selected? true)
        (set! mouse-xcoordinate (- mx x)))) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; canvas-after-button-up: NonNegInt NonNegInt -> Void
    ;; GIVEN: the location of a button-up event
    ;; STRATEGY: Setting the controller-selected as false
    (define/public (canvas-after-button-up mx my)
      (begin
        (set! controller-selected? false)
        (send model controller-still-selected? false) 
        (set! mouse-xcoordinate 0)))  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; canvas-after-drag: NonNegInt NonNegInt -> Void
    ;; GIVEN: the location of a after-drag event
    ;; EFFECT: this method is to be ignored    
    (define/public (canvas-after-drag mx my)
      (if controller-selected?
          (send model execute-command
                (make-set-xposition (within-limits lo-x (- mx mouse-xcoordinate) hi-x)))
          ELSE))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; within-limits: Integer Integer Integer -> Integer
    ;; GIVEN: a coordinate value and its lo and hi values
    ;; RETURNS: the same value if it is within bounds, else value of the boundary if crossed
    ;; DESIGN STRATEGY: Combine simpler functions
    
    (define (within-limits lo val hi) 
      (max lo (min val hi)))))



;;;; TESTS ;;;;;;



(begin-for-test
  (local
    ((define m (new Model%))
     (define xcontroller (new XController% [model m])))
    (send xcontroller after-key-event "right")
    (send xcontroller canvas-after-button-down 300 250)
    (send xcontroller canvas-after-drag 320 250)
    (check-equal?
     (send xcontroller for-test:get-particle-x) 95)
    (send xcontroller canvas-after-button-up 300 200)
    (send xcontroller data-image)
    (send xcontroller controller-still-selected?)
    (check-equal?
     (send xcontroller for-test:get-particle-y) 50)))


