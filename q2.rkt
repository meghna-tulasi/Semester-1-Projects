#lang racket

(require rackunit)
(require 2htdp/universe)
(require 2htdp/image)
(require "extras.rkt")
(require "WidgetWorks.rkt")
(require "SBlockInterface.rkt")
(require "SBlock.rkt")
(require "SBlockFactory.rkt") 
(require "Constants.rkt")



(provide
 SBlock<%>
 cubelets-init)
 

(define cubelets-init (container-init CANVAS-WIDTH CANVAS-HEIGHT))
(define SBlock-Factory (make-factory cubelets-init CANVAS-HALF-WIDTH CANVAS-HALF-HEIGHT))
                         

;cubelets-init : -> Container
;GIVEN: no arguments
;RETURNS: a Container, initially with no blocks, which when run, will
;run in a 600x500 canvas and process the events in the description above

(define (run rate)
  (begin
    (send cubelets-init add-stateful-widget SBlock-Factory)
    (send cubelets-init run rate)))