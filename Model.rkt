#lang racket

;; The model comprises of a particle. The particle, bounces with a given
;; velocity within a rectangular boundary when unselected
;; It accepts commands and reports when its status changes to the controllers

(require rackunit)
(require "extras.rkt")
(require "Interfaces.rkt")
(require "PerfectBounce.rkt")
(require "WidgetWorks.rkt")


(provide
 Model%
 make-model)
 

;; DATA DEFINITIONS :


;; LISTOFCONTROLLER<%> 
;;
;; A ListofController<%> (loc) is either
;; -- empty
;; -- (cons Controller<%> loc)
;;
;;
;; A ListofController<%> is either an empty list
;; or it consists of a Controller<%> and a ListofController<%>
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; TEMPLATE FOR LISTOFCONTROLLER<%>
;;
;; loc-fn : ListofController<%> -> ??
;; (define (loc-fn loc)
;;   (cond
;;     [(empty? loc) ...]
;;     [else (... (Controller<%>-fn (first loc))
;;                (loc-fn (rest loc)))]))


;; -> Model
(define (make-model) (new Model%))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A Model% is a (new Model% [x Integer]
;;                           [y Integer])
;; INTERPRETATION:
;; x is the x coordinate of the center of the particle
;; y is the y-coordinate of the center of the particle.
;; It contains all the details of the particle.

(define Model%
  (class* object% (Model<%>)
    
    ;; boundaries of the field
    (field [lo-x 0]) ; x lower boundary
    (field [hi-x 150]) ; x higher boundary 
    (field [lo-y 0]) ; y lower boundary
    (field [hi-y 100]) ; y higher boundary
    (field [r (make-rect lo-x hi-x lo-y hi-y)]) ; rectangle with the mentioned bounds
    
    
    ;; position and velocity of the object
    (init-field [x (/ (+ lo-x hi-x) 2)]) ; Integer
    (init-field [y (/ (+ lo-y hi-y) 2)]) ; Integer 
    (field [vx 0]) ; Integer
    (field [vy 0]) ; Integer
    
    (init-field [canvas-selected? false]); Intializing canvas selected with false
    
    ;; list of controllers. The list of registered controllers
    (init-field [loc empty]) ; ListOfController<%> 
    
    (super-new)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
    ;; within-limits: Integer Integer Integer -> Integer
    ;; GIVEN: a coordinate value and its lo and hi values
    ;; RETURNS: the same value if it is within bounds, else value of the boundary if crossed
    ;; DESIGN STRATEGY: Combine simpler functions
    
    (define (within-limits lo val hi)
      (max lo (min val hi)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ;; after-tick: -> Void
    ;; GIVEN: No args
    ;; EFFECT: Moves the object by vx and vy when it is unpaused
    ;;         Updates x, y , vx and vy
    ;;         publishes x and y at every tick
    ;;         publishes velocity at every tick
    
    (define/public (after-tick)
        (let ((p (particle-after-tick (make-particle x y vx vy) r)))
          (if canvas-selected?
              (publish-particle (make-particle x y vx vy)) 
              (begin
                (set! x (within-limits lo-x (particle-x p) hi-x))
                (set! y (within-limits lo-y (particle-y p) hi-y))
                (set! vx (particle-vx p))
                (set! vy (particle-vy p))
                (publish-particle p))))) 
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; controller-still-selected? : Boolean Value -> Boolean
    ;; RETURNS : boolean value depending on the selection of controller
    (define/public (controller-still-selected? t)
      (set! canvas-selected? t))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;     
    ;; register: Controller<%> -> Void
    ;; GIVEN: A Controller
    ;; EFFECT: Registers the new controller and send position and velocity data to it
    (define/public (register c)
      (begin
        (set! loc (cons c loc))
        (send c receive-signal (make-particle x y vx vy))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;       
    ;; execute-command: Command -> Void
    ;; GIVEN: A Command 
    ;; EFFECT: decodes the command, executes it, and sends updates to the
    ;;         controllers.
    ;; DESIGN STRATEGY: Cases on Command cmd
    (define/public (execute-command cmd)
      (begin
        (cond
          [(set-xposition? cmd)
           (begin
             (set! x (set-xposition-pos-x cmd))
             (publish-particle (make-particle x y vx vy)))]

          [(set-yposition? cmd)
           (begin
             (set! y (set-yposition-pos-y cmd))
             (publish-particle (make-particle x y vx vy)))]
          
          [(incr-xvelocity? cmd)
           (begin
             (set! vx (+ vx (incr-xvelocity-dvx cmd)))
             (publish-particle (make-particle x y vx vy)))]

          [(incr-yvelocity? cmd)
           (begin
             (set! vy (+ vy (incr-yvelocity-dvy cmd)))
             (publish-particle (make-particle x y vx vy)))])))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
    
    ;; report position or velocity to each controller: 
    
    ;; publish-particle : Particle -> Void
    ;; EFFECT: reports the position to each controller in the list of controllers
    (define (publish-particle particle)
      (for-each
       ; Controller<%> -> Void
       ; EFFECt : sends controller signal about the state of particle
       (lambda (obs) (send obs receive-signal particle))
       loc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    
    ;; The model responds to after-tick, but not to any of the other
    ;; SWidget messages
    (define/public (after-button-down mx my) 'trap)
    (define/public (after-button-up mx my) 'trap)
    (define/public (after-drag mx my) 'trap)
    (define/public (after-move mx my) 'trap)
    (define/public (after-key-event kev) 'trap)
    (define/public (add-to-scene s) s)
    
    


;;;;;; TESTS ;;;;;;;;

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

   
    ))

(begin-for-test 
  (local
    ((define m (new Model%))
     (define m1 (new Model% [x 10] [y 20])))
     (send m execute-command (make-set-xposition 15))
    (send m execute-command (make-set-yposition 20))
     (send m execute-command (make-incr-xvelocity 0))
     (send m execute-command (make-incr-yvelocity 0))
    (check-equal?
     (send m for-test::x)
     15 "Does not return x coordinate of the particle")
     (check-equal?
     (send m for-test::y)
     20 "Does not return y coordinate of the particle")
     (check-equal?
     (send m for-test::vx)
           0
           "Does not return velocity of particle on x axis")
     (check-equal?
     (send m for-test::vy)
           0
           "Does not return velocity of particle on y axis")
  (send m after-tick)
  (check-equal?
     (send m for-test::x)
           15
           "Does not return x coordinate of the particle after tick")
     (check-equal?
     (send m for-test::y)
           20
           "Does not return y coordinate of the particle after tick")
     (check-equal?
     (send m for-test::vx)
           0
           "Does not return velocity of particle on x axis after tick")
     (check-equal?
      (send m for-test::vy)
           0
           "Does not return velocity of particle on y axis after tick")
    
    (send m controller-still-selected? true) 
    (send m after-tick) 
    (send m1 after-tick)
    (send m after-button-down 300 200)
    (send m after-button-up 320 250)
    (send m after-drag 400 210)
    (send m1 after-move 300 200)
    (send m after-key-event "s")
    ))
