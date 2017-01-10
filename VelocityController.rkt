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
 VelocityController%)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VelocityController% is a (new VelocityController% [model Model<%>])
;; Where the model stores the object of Model class and stores 
;; information of the particle.
(define VelocityController%
  (class* Controller% (Controller<%>)
    
    (init-field model)  ; the model stores all the information about the particle.
    
    
    ;; inherit fields from super class Controller%
    (inherit-field x  ;x coordinate
                   y  ;y coordinate
                   vx ;x velocity
                   vy ;y velocity
                   controller-selected? ;mouse button-down event has occured?
                   handle-selected?
                   width
                   half-width
                   height
                   half-height)
    
    (super-new)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Controller<%> -> Void
    ;; EFFECT : Registers the given controller to receive signal
    (send model register this)
    
    (set! width CONTROLLERWIDTH) ;; width is the width of the rectangle 
    (set! height CONTROLLERHEIGHT) ;; height is the height of the rectangle
    (set! half-width (/ width 2))  ;; half of the width of the controller
    (set! half-height (/ height 2)) ;; half of the height of the controller
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ;; KeyEvent -> Void
    ;; GIVEN: a KeyEvent
    ;; EFFECT: the arrow key events can alter the position of the particle.
    (define/override (after-key-event kev)
      (if controller-selected?
          (cond
            [(key=? LEFT-KEY-EVENT kev)
             (send model execute-command
                   (make-incr-xvelocity -5))]
            [(key=? RIGHT-KEY-EVENT kev)
             (send model execute-command
                   (make-incr-xvelocity FIVE))]
            
            [(key=? UP-KEY-EVENT kev)
             (send model execute-command
                   (make-incr-yvelocity -5))]
            [(key=? DOWN-KEY-EVENT kev)
             (send model execute-command
                   (make-incr-yvelocity FIVE))])
          ELSE))
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
    ;; get-color : -> String
    (define (get-color)
      (if controller-selected?
          "red"
          "black"))
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ;; data-image : -> Image
    ;; GIVEN: no  value
    ;; RETURNS: an image of the data inside position controller.
    ;; STRATEGY: combine simpler functions
    (define/override (data-image)
      (above
       (text "Arrow keys change velocity" 10             
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
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
      'controlledbyvelocity)
    
    ; for-test::x : -> Integer
    ; RETURNS: the value of the x position of the particle
    (define/public (for-test::x) x)
    
    ; for-test::y : -> Integer
    ; RETURNS: the value of the y position of the particle
    (define/public (for-test::y) y)
    
    ; for-test::vx : -> Integer
    ; RETURNS: the value of the velocity of the particle
    ; on x axis
    (define/public (for-test::vx) vx)
    
    ; for-test::vy : -> Integer
    ; RETURNS: the value of the velocity of the particle
    ; on y axis
    (define/public (for-test::vy) vy)
    
    ; for-test::selected : -> Boolean
    ; RETURNS: true iff the controller is selected
    (define/public (for-test::controller-selected) controller-selected?)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; TESTS ;;;;;


(begin-for-test
  (local
    ((define m (new Model%))
     (define velcontrol (new VelocityController% [model m])))
    (send velcontrol after-key-event "right")
    (send velcontrol data-image)
    (send velcontrol after-button-down 310 260)
    (send velcontrol after-key-event "right")
    (check-equal?
     (send velcontrol for-test::vx) 5)
    (send velcontrol after-key-event "left")
    (send velcontrol after-key-event "left")
    (check-equal?
     (send velcontrol for-test::vx) -5)
    (send velcontrol after-key-event "up")
    (check-equal?
     (send velcontrol for-test::vy) -5)
    (send velcontrol after-key-event "down")
    (send velcontrol after-key-event "down")
    (check-equal?
     (send velcontrol for-test::vy) 5)
    (send velcontrol canvas-after-button-down 320 270)
    (send velcontrol canvas-after-drag 400 200)
    (check-equal?
     (send velcontrol for-test::x) 75)
    (send velcontrol canvas-after-button-up 400 200)
    (check-equal?
     (send velcontrol for-test::y) 50)
    (send velcontrol data-image)
    (check-equal?
     (send velcontrol for-test::controller-selected) #f)
    (check-equal?
     (send velcontrol for-test::vy) 5)))