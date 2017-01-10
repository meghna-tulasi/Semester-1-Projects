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
 YController%)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; YController is a (new YController% [model Model<%>])

(define YController%
  (class* Controller% (Controller<%>)
    
    
    (init-field model)  ; the model stores all the information about the particle.
    
    (inherit-field lo-y
                   hi-y) ; field y-axis boundaries
    
    
    ;; inherit fields from super class Controller%
    (inherit-field y  ;y coordinate
                   vy ;y velocity
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
    
    (set! width SPACE) ;; width is the width of the rectangle 
    (set! height (+ CONTROLLERHEIGHT SPACE))  ;; height is the height of the rectangle 
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
       (rectangle 40 100 RECT-TYPE INNER-RECT-COLOR)
       (place-image
        (overlay
         PARTICLE-CENTER-IMG
         PARTICLE-IMG)
        20 y
        (rectangle 40 100 "solid" "white"))))
    
    
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; canvas-after-button-down: NonNegInt NonNegInt -> Void
    ;; GIVEN: the location of a button-down event
    ;; EFFECT: Setting the controller-selected as true
    (define/public (canvas-after-button-down mx my)
      (begin
        (set! controller-selected? true)
        (send model controller-still-selected? true)
        (set! mouse-ycoordinate (- my y))))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; canvas-after-button-up: NonNegInt NonNegInt -> Void
    ;; GIVEN: the location of a button-up event
    ;; EFFECT: Setting the controller-selected as false
    (define/public (canvas-after-button-up mx my)
      (begin
        (set! controller-selected? false)
        (send model controller-still-selected? false) 
        (set! mouse-ycoordinate 0)))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; canvas-after-drag: NonNegInt NonNegInt -> Void
    ;; GIVEN: the location of a after-drag event
    ;; EFFECT: this method is to be ignored       
    (define/public (canvas-after-drag mx my)
      (if controller-selected?
          (send model execute-command
                (make-set-yposition (within-limits lo-y (- my mouse-ycoordinate) hi-y)))
          ELSE))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; within-limits: Integer Integer Integer -> Integer
    ;; GIVEN: a coordinate value and its lo and hi values
    ;; RETURNS: the same value if it is within bounds, else value of the boundary if crossed
    ;; DESIGN STRATEGY: Combine simpler functions
    
    (define (within-limits lo val hi) 
      (max lo (min val hi)))))


;;;;;;; TESTS ;;;;;;;;;;


(begin-for-test
  (local
    ((define m (new Model%))
     (define ycontroller (new YController% [model m])))
    (send ycontroller after-key-event "right")
    (check-equal?
     (send ycontroller for-test:get-particle-y) 50)
    (send ycontroller canvas-after-button-down 310 240)
    (send ycontroller canvas-after-drag 320 282)
    (send ycontroller canvas-after-button-up 300 250)
    (send ycontroller controller-still-selected?)
    (send ycontroller data-image)
    (check-equal?
     (send ycontroller for-test:get-particle-y) 92)
    (check-equal?
     (send ycontroller for-test:get-particle-x) 75)))

