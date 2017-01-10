#lang racket

(require "WidgetWorks.rkt")

(provide Controller<%>
         Model<%>)

(provide 
  (struct-out set-xposition)
  (struct-out set-yposition)
  (struct-out incr-xvelocity)
  (struct-out incr-yvelocity)
  (struct-out report-position)
  (struct-out report-velocity))
  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The Controller<%> interface  extends the SWidget<%> interface
;; Each controller object implements the Controller<%> interface

(define Controller<%>    
  (interface (SWidget<%>)
    
    ;; Signal -> Void
    ;; receive a signal from the model and adjust controller
    ;; accordingly
    receive-signal))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Every Model implements the Model<%> interface

(define Model<%>
  (interface ()
    
    ;; -> Void
    ;; EFFECT : Updates model after tick
    after-tick        

    ;; Controller<%> -> Void
    ;; EFFECT : Registers the given controller to receive signal
    register

    ;; controller-still-selected? : -> Boolean
    ;; RETURNS: True iff the mouse is inside
    ;; this controller
    controller-still-selected?

    ;; Command -> Void
    ;; EFFECT : Executes the given command
    execute-command))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS FOR COMMUNICATION WITH THE MODEL

;; A Command is one of 
;; -- (make-set-xposition pos-x)
;; -- (make-set-yposition pos-y)
;; -- (make-set-xvelocity dvx)
;; -- (make-set-yvelocity dvy)

;; A Signal is one of
;; -- (make-report-position x y)
;; -- (make-report-velocity vx vy)


(define-struct set-xposition (pos-x) #:transparent)
(define-struct set-yposition (pos-y) #:transparent)
(define-struct incr-xvelocity (dvx) #:transparent)
(define-struct incr-yvelocity (dvy) #:transparent)
(define-struct report-position (pos-x pos-y) #:transparent)
(define-struct report-velocity (vx vy) #:transparent)

