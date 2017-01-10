#lang racket
(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)   
(require 2htdp/image)
(require "WidgetWorks.rkt")
(require "SBlock.rkt")
(require "Constants.rkt")



(provide
 SBlockFactory% 
 make-factory)


;; A SBlockFactory class is a 
;; (new SBlockFactory%  [cubelets-init Container<%>]
;;                      [block-initial-x Int]
;;                      [block-initial-y Int]
;;                      [list-of-sblocks ListOfBlocks])
;;
;; INTERPPRETATION:
;; Represents sblocks created on canvas

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define SBlockFactory%
  (class* object% (SWidget<%>)

    (init-field cubelets-init) 

    (init-field block-initialx  ;;Coordinates of block that it should appear on canvas
                block-initialy)
    
    (init-field [list-of-sblocks empty]) 
    
 
   (super-new)

    ; -> Void
    ; GIVEN: no arguments
    ; EFFECT: updates this widget to the state it should have
    ; following a tick.
   (define/public (after-tick) this)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;      

    ; after-button-down : Integer Integer -> Void
    ; GIVEN: a location
    ; EFFECT: updates this widget to the state it should have
    ; following the specified mouse event at the given location.
   (define/public (after-button-down mx my)
     (set! block-initialx mx)
     (set! block-initialy my))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
       
    ; after-button-up : Integer Integer -> Void
    ; GIVEN: a location
    ; EFFECT: updates this widget to the state it should have
    ; following the specified mouse event at the given location.
   (define/public (after-button-up mx my)
     (set! block-initialx mx)
     (set! block-initialy my))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; after-drag : Integer Integer -> Void
    ; GIVEN: a location
    ; EFFECT: updates this widget to the state it should have
    ; following the specified mouse event at the given location.
   (define/public (after-drag mx my) this)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
    ; after-move : Integer Integer -> Void
    ; GIVEN: a location
    ; EFFECT: updates this widget to the state it should have
    ; following the specified mouse event at the given location.
    (define/public (after-move mx my)
      (set! block-initialx mx)
      (set! block-initialy my))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; KeyEvent -> Void
    ; GIVEN: a key event
    ; EFFECT: updates this widget to the state it should have
    ; following the given key event
    (define/public (after-key-event kev)
      (cond 
         [(key=? kev NEW-SBLOCK-EVENT) 
         (let
             ((new-sblock (make-block block-initialx block-initialy list-of-sblocks)))
               (begin
                 (draw-sblock new-sblock)
                 (send new-sblock add-neighbor list-of-sblocks)
                 (set! list-of-sblocks (cons new-sblock list-of-sblocks))))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; draw-sblock : SBlock<%> -> Void
    ;; GIVEN : block
    ;; EFFECT : block added to the existing blocks in the team
    (define (draw-sblock sblock) 
      (begin
        (for-each
         ;; SBlock<%> -> Void
         ;; EFFECT : draws a block
         (lambda (b) (send b add-neighbor (list sblock)))
         list-of-sblocks)
        (send cubelets-init add-stateful-widget sblock))) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; Scene -> Scene
    ; GIVEN: a scene
    ; RETURNS: a scene like the given one, but with this object
    ; painted on it.
    (define/public (add-to-scene scene) scene)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; for-test::x-pos -> Integer
    ;; RETURNS : x coordinate at which new SBlock is created
    (define/public (for-test::x) block-initialx)
    
    ;; for-test::y-pos -> Integer
    ;; RETURNS : y coordinate at which new SBlock is created
    (define/public (for-test::y) block-initialy)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make-factory : Container<%> Int int -> SBlockFactory
;; GIVEN : container wherein a factory adds blocks at given x and y coordinates
;; RETURNS : a factory
;; STRATEGY : Combining simple functions
(define (make-factory c x y)
  (new SBlockFactory% 
       [cubelets-init c]
       [block-initialx x]
       [block-initialy y]))  
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TESTS :

(begin-for-test
  (local
    ((define block-factory
       (make-factory
        (container-init CANVAS-WIDTH CANVAS-HEIGHT)
        CANVAS-HALF-WIDTH
        CANVAS-HALF-HEIGHT)))
    
    (send  block-factory after-tick)
    (send  block-factory after-drag 300 250)
    (send  block-factory after-key-event NEW-SBLOCK-EVENT)
    
    (check-equal? (send  block-factory for-test::x)CANVAS-HALF-WIDTH
                  "Incorrect x coordinate returned")
    
    (check-equal? (send  block-factory for-test::y)CANVAS-HALF-HEIGHT
                  "Incorrect x coordinate returned")
    
    (send block-factory after-button-down 300 250)
    (check-equal?
     (send  block-factory for-test::x)CANVAS-HALF-WIDTH
     "Incorrect x coordinate returned")
    
    (send block-factory after-button-up 300 300)
    (check-equal?
     (send  block-factory for-test::y)CANVAS-HALF-WIDTH
     "Incorrect x coordinate returned")
    
    (send block-factory after-move 300 250)
    (check-equal?
     (send  block-factory for-test::x)CANVAS-HALF-WIDTH
     "Incorrect x coordinate returned")
   

    (check-equal?
     (send block-factory add-to-scene EMPTY-CANVAS) EMPTY-CANVAS
     "Incorrect canvas status")))


 
   









    