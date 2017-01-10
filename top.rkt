#lang racket

;The entire system works on a 600 x 500 canvas.
;Hitting one of the following keys causes a new controller
;to appear in the center of the canvas:
;"p" : Position controller
;"v" : velocity controller
;"x" : X controller
;"y" : Y controller
;"z" : XY controller
;Each controller has a 10x10 handle.
;Dragging on the handle moves the controller around the canvas.
;A button-down inside a controller selects the controller for input.
;In the position or velocity controller, the arrow keys are used for input.
;The arrow keys alter the position or velocity of the
;particle in the indicated direction.
;Each press of an arrow key alters the appropriate quantity by 5.
;to run the program -> (run 0.5)

(require rackunit) 
(require "Model.rkt")
(require "WidgetWorks.rkt")
(require "Interfaces.rkt")
(require "extras.rkt")
(require "ControllerFactory.rkt")
(require "Controller.rkt")
(require "Constants.rkt")
(require "PerfectBounce.rkt")

(provide run)

(check-location "11" "top.rkt")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; run : PosReal -> Void
;; GIVEN: a frame rate, in seconds/tick
;; EFFECT: Creates and runs the MVC simulation with the given frame rate
;; DESIGN STRATEGY: Combine simpler functions
(define (run rate)
  (let ((c (container-init CANVAS-WIDTH CANVAS-HEIGHT))
        (m (make-model)))
    (begin
      (send c add-stateful-widget m)
      (send c add-stateful-widget (make-controller-factory c m))
      (send c run rate)))) 

