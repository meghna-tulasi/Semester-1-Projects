#lang racket
(require "extras.rkt")
(require 2htdp/image)
(require 2htdp/universe)
(require rackunit)
(require "Interfaces.rkt")
(require "Model.rkt")
(require "Controller.rkt")
(require "Constants.rkt")


(provide
 PositionController%)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; a PositionController% is a (new PositionController% [model Model<%>])
;; Where the model stores the object of Model class and stores 
;; information of the particle.
(define PositionController%
  (class* Controller% (Controller<%>)
    
    (init-field model)  ; the model stores all the information about the particle.
    
    (inherit-field lo-x
                   hi-x) ; field x-axis boundaries
    
    (inherit-field lo-y
                   hi-y) ; field y-axis boundaries
    
    
    
    ;; inherit fields from super class Controller%
    (inherit-field x  ;x coordinate
                   y  ;y coordinate
                   vx ;x velocity
                   vy ;y velocity
                   mouse-xcoordinate
                   mouse-ycoordinate
                   controller-selected?
                   handle-selected?
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
    
    
    (set! width CONTROLLERWIDTH) ;; width is the width of the rectangle 
    (set! height CONTROLLERHEIGHT) ;; height is the height of the rectangle 
    (set! half-width (/ width 2))
    (set! half-height (/ height 2))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; KeyEvent -> Void
    ;; GIVEN: a KeyEvent
    ;; EFFECT: the arrow key events can alter the position of the particle.
    (define/override (after-key-event kev)
      (if controller-selected?
          (cond
            [(key=? LEFT-KEY-EVENT kev)
             (send model execute-command
                   (make-set-xposition (within-limits
                                        lo-x (- x FIVE) hi-x)))]
            [(key=? RIGHT-KEY-EVENT kev)
             (send model execute-command
                   (make-set-xposition
                    (within-limits
                     lo-x (+ x FIVE) hi-x)))]

            [(key=? UP-KEY-EVENT kev)
             (send model execute-command
                   (make-set-yposition
                    (within-limits
                     lo-y (- y FIVE) hi-y)))]

            [(key=? DOWN-KEY-EVENT kev)
             (send model execute-command
                   (make-set-yposition
                    (within-limits
                     lo-y (+ y FIVE) hi-y)))])
          ELSE))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; within-limits: Integer Integer Integer -> Integer
    ;; GIVEN: a coordinate value and its lo and hi values
    ;; RETURNS: the same value if it is within bounds, else value of the boundary if crossed
    ;; DESIGN STRATEGY: Combine simpler functions
    
    (define (within-limits lo val hi)
      (max lo (min val hi)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; get-color : -> String
    (define (get-color)
      (if controller-selected?
          "red"
          "black"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
    ;; data-image : -> Image
    ;; GIVEN: no  value
    ;; RETURNS: an image of the data inside position controller.
    ;; STRATEGY: combine simpler functions
    (define/override (data-image)
      (above
       (text "Arrow keys change position" FONTSIZE10             
             (get-color))
       (text (string-append
              "X = "
              (real->decimal-string x)
              "Y = "
              (real->decimal-string y))
             FONTSIZE12 (get-color))
       (text (string-append
              "VX = "
              (real->decimal-string vx)
              "VY = "
              (real->decimal-string vy))
             FONTSIZE12 (get-color))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; canvas-after-button-down: NonNegInt NonNegInt -> Void
    ;; GIVEN: the location of a button-down event
    ;; STRATEGY: Setting the controller-selected as true
    (define/public (canvas-after-button-down mx my)
      (set! controller-selected? true))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; canvas-after-button-up: NonNegInt NonNegInt -> Void
    ;; GIVEN: the location of a button-up event
    ;; STRATEGY: Setting the controller-selected as false
    (define/public (canvas-after-button-up mx my)
      (set! controller-selected? false))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; canvas-after-drag: NonNegInt NonNegInt -> Void
    ;; GIVEN: the location of a after-drag event
    ;; EFFECT: this method is to be ignored
    (define/public (canvas-after-drag mx my)
      'position-controller-after-drag-value))) 


;;;;;;;; TESTS ;;;;;;;

(begin-for-test
  (local
    ((define m (new Model%))
     (define poscontrol (new PositionController% [model m])))
    (send poscontrol after-key-event "right")
    (send poscontrol data-image)
    (send poscontrol after-button-down 310 250)
    (send poscontrol after-key-event "right")
    (check-equal?
     (send poscontrol for-test:get-particle-x) 80)
    (send poscontrol after-key-event "left")
    (send poscontrol after-key-event "left")
    (check-equal?
     (send poscontrol for-test:get-particle-x) 70)
    (send poscontrol after-key-event "up")
    (check-equal?
     (send poscontrol for-test:get-particle-y) 45)
    (send poscontrol after-key-event "down")
    (send poscontrol after-key-event "down")
    (check-equal?
     (send poscontrol for-test:get-particle-y) 55)
    (send poscontrol canvas-after-button-down 320 270)
    (send poscontrol canvas-after-drag 400 200)
    (send poscontrol canvas-after-button-up 325 280)
    (send poscontrol data-image)
    (check-equal?
     (send poscontrol for-test:get-particle-y) 55)))

(begin-for-test
  (local
    ((define m (new Model%))
     (define pos (new PositionController% [model m])))
    (send pos canvas-after-drag 300 200)))




