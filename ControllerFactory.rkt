#lang racket
(require 2htdp/universe)
(require rackunit)
(require "extras.rkt")
(require "WidgetWorks.rkt")
(require "Interfaces.rkt")
(require "PositionController.rkt")
(require "VelocityController.rkt")
(require "XController.rkt")
(require "YController.rkt") 
(require "XYController.rkt")
(require "Constants.rkt")
(require "Model.rkt")


(provide
 ControllerFactory%
 make-controller-factory)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make-controller-factory : Container Model -> SWidget
(define (make-controller-factory c m)
  (new ControllerFactory% [c c][m m]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A ControllerFactory% is a
;; (new ControllerFactory%
;;     [c Container<%>]
;;     [m Model<%>])
;; where c is an object of Container<%> which
;; stores its objects and handles the events on occuring on the object.
;; m is an object of Model<%> which stores all the information about the
;; particle to be simulated.
;; The Controller factory implements SWidget<%>

(define ControllerFactory%
  (class* object% (SWidget<%>)

    ; the container contains controllers
    (init-field c)   ; Container

    ; the model to which the controllers will be connected
    (init-field m)   ; Model

    (super-new)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
    ;; after-key-event: KeyEvent -> Void
    ;; GIVEN: a key Event
    ;; RETUNS: a value that can be ignored
    ;; EFFECT: the controller is created as per the
    ;; key events which is handled by the add-veiwer method
    (define/public (after-key-event kev)
      (cond
        [(key=? kev VELOCITY-KEY-EVENT) (add-viewer VelocityController%)]
        [(key=? kev POS-KEY-EVENT) (add-viewer PositionController%)]
        [(key=? kev X-KEY-EVENT) (add-viewer XController%)]
        [(key=? kev Y-KEY-EVENT) (add-viewer YController%)]
        [(key=? kev XY-KEY-EVENT) (add-viewer XYController%)]))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
     ;; (Model -> Controller) -> Void
    (define/public (add-viewer make-viewer)
      (send c add-stateful-widget (new make-viewer [model m])))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; the factory is invisible
    (define/public (add-to-scene s) s)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;r
    ;; the factory doesn't respond to any other events
    (define/public (after-tick) 'controller-factory-after-tick-trap)
    
    (define/public (after-button-down mx my)
      'controller-factory-after-button-down-trap)
    
    (define/public (after-drag mx my)
      'controller-factory-after-drag-trap)
    
    (define/public (after-move mx my)
      'controller-factory-after-move-trap)
    
    (define/public (after-button-up mx my)
      'controller-factory-after-button-up-trap)

    ))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TESTS:

(begin-for-test
  (local
    ((define m (new Model%))
     (define c (container-init CANVAS-WIDTH CANVAS-HEIGHT))
     (define f (new ControllerFactory% [c c] [m m])))
    (send f after-key-event VELOCITY-KEY-EVENT)
    (send f after-key-event POS-KEY-EVENT)
    (send f after-key-event X-KEY-EVENT)
    (send f after-key-event Y-KEY-EVENT)
    (send f after-key-event XY-KEY-EVENT)
    (check-equal?
     (send f add-to-scene EMPTY-CANVAS)
     EMPTY-CANVAS)
    (send f after-tick)
    (send f after-button-down 30 30)
    (send f after-button-up 30 30)
    (send f after-move 30 30)
    (send f after-drag 30 30)))


        