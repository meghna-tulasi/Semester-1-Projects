;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname reference) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;To implement a system for a graphical interface for trees.
;;Hitting "c" when no node is selected creates a new root node that is a circle. 
;;Hitting "c" when a node is selected adds a new son to the selected node.
;;Hitting "s" at any time should behave like "c", except that the new node
;;created is a square instead of a circle.
;;Hitting "d" when a node is selected deletes the selected node. whole subtree. 


(require rackunit)
(require "extras.rkt")
(check-location "06" "q1.rkt")
(require 2htdp/universe)
(require 2htdp/image)

(provide
 initial-world
 run
 world-after-mouse-event
 world-after-key-event
 world-to-trees
 tree-to-root
 tree-to-sons
 node-to-center
 node-to-selected?)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;CONSTANT-DEFINITIONS:

(define CANVAS-WIDTH 500)
(define CANVAS-HEIGHT 400)

(define CIRCLE-RADIUS 20)
(define SQUARE-SIDE 40)

(define BUTTON-DOWN "button-down")
(define BUTTON-UP "button-up")
(define DRAG "drag")
(define MOVE "move")
(define ENTER "enter")
(define LEAVE "leave")

(define SELECTED true)
(define UNSELECTED false)

(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;DATA-DEFINITIONS:

(define-struct tree (node lot))
;;A Tree is a
;;(make-tree Node LOT)
;;INTERPRETATION:
;;node->represents node of the tree
;;lot->represents list of subtrees for a node

;;TEMPLATE:
;;tree-fn: Tree -> ?
;(define (tree-fn t)
;  (...
;   (tree-node t)
;   (lot-fn (tree-lot t))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct node (x y selected? mx my relativex relativey circle?))
;;A Node is a
;;(make-node Int Int Boolean Int Int Int Int Boolean)
;;INTERPRETATION:
;;(x,y)->represent coordinates of the center of node.
;;selected? -> represents if node is selected or not.
;;(mx,my)->represent coordinates of mouse when node is selected.
;;relativex represents the relative distance of x of center of circle to mouse x position.
;;relativey represents the relative distance of y of center of circle to mouse y position
;;circle? indicates whether this node is a circle or a square

;;TEMPLATE:
;;node-fn: Node -> ?
;(define (node-fn n)
;  (...
;   (node-x n)
;   (node-y n)
;   (node-selected? n)
;   (node-mx n)
;   (node-my n)
;   (node-relativex n)
;   (node-relativey n)
;   (node-circle? n)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;ListOfTrees is one of the following:
;;--empty
;;--(cons Tree LOT)

;;TEMPLATE:
;;lot-fn :LOT -?
;(define (lot-fn lot)
;  (cond
;    [(empty? lot)...]
;    [else (...
;           (tree-fn (first lot))
;           (lot-fn (rest lot)))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct world (lot))
;;A World is a
;;(make-world LOT)
;;INTERPRETATION:
;;world is a list of trees

;;TEMPLATE:
;;world-fn: World -> ?
;(define (world-fn w)
;  (...
;   (world-lot w)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;VARIABLES USED FOR TESTING :

(define NODE1(make-node 50 40 SELECTED 55 65 12 40 true))
(define NODE2(make-node 60 100 SELECTED 75 85 70 140 true))
(define NODE7(make-node 140 100 SELECTED 100 130 70 170 true))
(define NODE3(make-node 160 120 UNSELECTED 190 160 70 170 true))
(define NODE4(make-node 60 120 UNSELECTED 190 160 70 170 true))
(define NODE5(make-node 60 120 UNSELECTED 190 160 70 170 false))
(define NODE6(make-node 160 120 SELECTED 190 160 70 170 false))
(define NODE8(make-node 160 120 UNSELECTED 190 160 70 170 false))

(define TREE1 (make-tree NODE1 empty))
(define TREE2 (make-tree NODE2 empty))
(define TREE3 (make-tree NODE3 (list TREE1 TREE2)))
(define TREE4 (make-tree NODE4 (list TREE1 TREE3)))
(define TREE5 (make-tree NODE5 (list TREE1 TREE3)))
(define TREE6 (make-tree NODE1 (list TREE2)))
(define TREE7 (make-tree NODE1 (list TREE6)))
(define TREE9 (make-tree NODE4 empty))
(define TREE8 (make-tree NODE3 (list TREE9)))
(define TREE10 (make-tree NODE4 (list TREE8)))


(define solid-square
  (square 40 "solid" "green"))
(define outline-square
  (square 40 "outline" "green"))
(define outline-circle
  (circle 20 "outline" "green"))

(define scene-with-line-circle
  (overlay
   (scene+line
    (place-image
     (circle 20 "solid" "green") 50 40
     (rectangle CANVAS-WIDTH CANVAS-HEIGHT "outline" "black"))
    50 40 160 120 "blue")
   (rectangle CANVAS-WIDTH CANVAS-HEIGHT "outline" "black")))

(define scene-with-line-circle-2
  (overlay
   (scene+line
    (place-image
     (circle 20 "solid" "green") 50 40
     (rectangle CANVAS-WIDTH CANVAS-HEIGHT "outline" "black"))
    50 40 160 120 "blue")
   (overlay
    (scene+line
     (place-image
      (circle 20 "solid" "green") 60 100
      (rectangle CANVAS-WIDTH CANVAS-HEIGHT "outline" "black"))
     60 100 50 40 "blue")
    (rectangle CANVAS-WIDTH CANVAS-HEIGHT "outline" "black"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;FUNCTION-DEFINITIONS:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;initial-world : Any -> World
;;GIVEN: any value
;;RETURNS: an initial world.  The given value is ignored.
;;EXAMPLES:(initial-world 2) -> (make-world '())
;;STRATEGY: Use the template of world 
(define (initial-world any)
  (make-world empty))

;;TESTS:
(begin-for-test
  (check-equal? (initial-world 2)(make-world '())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;run :  Any -> World
;;GIVEN: any value
;;EFFECT: runs a copy of an initial world
;;RETURNS: the final state of the world.  The given value is ignored.
;;EXAMPLES:(run 1) -> initial-scene
;;STRATEGY: Combine simpler functions.
(define (run anyvalue)
  (big-bang (initial-world "start")
            (on-key world-after-key-event)
            (on-mouse world-after-mouse-event)
            (on-draw world-to-scene)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;world-after-mouse-event : World Integer Integer MouseEvent -> World
;;GIVEN: a World, a location, and a MouseEvent
;;RETURNS: the state of the world as it should be following
;;the given mouse event at that location.
;;EXAMPLES:(world-after-mouse-event (make-world
;;   (list (make-tree (make-node 32 37 SELECTED 30 30 -2 -7 true)
;;   (list (make-tree (make-node 82 87 UNSELECTED 30 30 52 57 true) empty))))) 52 57 DRAG)
;;->(make-world (list (make-tree (make-node 54 64 #true 52 57 -2 -7 #true) (list (make-tree
;;   (make-node 104 114 #false 52 57 52 57 #true) '())))))
;;(world-after-mouse-event (make-world (list (make-tree
;;  (make-node 12 17 SELECTED 30 30 34 78) empty)))20 20 BUTTON-UP)
;;-> (make-world (list (make-tree (make-node 12 17 #false 20 20 8 3) '())))
;;STRATEGY:Divide into cases based on mouse events
(define (world-after-mouse-event w mx my mev)
  (cond
    [(mouse=? BUTTON-DOWN mev) (button-down w mx my)]
    [(mouse=? BUTTON-UP mev) (button-up w mx my)]
    [(mouse=? DRAG mev) (drag w mx my)]
    [else w]))

;;TESTS:
(begin-for-test
  (check-equal?
   (world-after-mouse-event (make-world (list TREE1 TREE2)) 180 150  BUTTON-DOWN)
   (make-world (list (make-tree NODE1 '())
                     (make-tree NODE2 '()))))
  (check-equal?
   (world-after-mouse-event (make-world (list TREE1 TREE2)) 180 150  BUTTON-UP)
   (make-world (list (make-tree (make-node 50 40 #false 180 150 130 110 #true) '())
                     (make-tree (make-node 60 100 #false 180 150 120 50 #true) '()))))
  (check-equal?
   (world-after-mouse-event (make-world (list TREE1 TREE2)) 180 150  DRAG)
   (make-world (list (make-tree (make-node 175 125 #true 180 150 5 25 #true) '())
                     (make-tree (make-node 165 165 #true 180 150 15 -15 #true) '()))))
  (check-equal?
   (world-after-mouse-event (make-world (list TREE1 TREE2)) 180 150  ENTER)
   (make-world (list (make-tree NODE1 '())
                     (make-tree NODE2 '())))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;button-down:World Int Int -> World
;;GIVEN: World (x,y) coordinates of a mouse 
;;RETURNS: world after button down event
;;EXAMPLES:(button-down (make-world (list (make-tree
;;   (make-node 12 17 UNSELECTED 30 30 34 78) empty))) 30 30)
;;->(make-world (list (make-tree (make-node 12 17 #true 30 30 18 13) '())))
;;STRATEGY:Use the template of world and using HOF map on world-lot
(define (button-down w mx my)
  (make-world
   (map
    (lambda (t)
      (tree-after-button-down t mx my))
    (world-lot w))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;tree-after-button-down: Tree Int Int -> Tree
;;GIVEN: Tree and mouse positions
;;RETURNS:Tree after button-down
;;EXAMPLES:(tree-after-button-down (make-tree
;;   (make-node 12 17 UNSELECTED 30 30 34 78) empty) 30 30 UNSELECTED)
;;->(make-tree (make-node 12 17 #true 30 30 18 13) '())
;;HALTING MEASURE: the number of element in lot of t
;;STRATEGY: Use the template of tree and using HOF map on tree-lot

(define (tree-after-button-down t mx my)
  (make-tree
   (node-after-button-down (tree-node t) mx my)
   (map (lambda (x) (tree-after-button-down x mx my)) (tree-lot t))))

;;TESTS:
(begin-for-test
  (check-equal?
   (tree-after-button-down (make-tree NODE1 (list TREE2 TREE3)) 55 65)
   (make-tree
    (make-node 50 40 #true 55 65 12 40 #true)
    (list
     (make-tree (make-node 60 100 #true 75 85 70 140 #true) '())
     (make-tree (make-node 160 120 #false 190 160 70 170 #true)
                (list (make-tree (make-node 50 40 #true 55 65 12 40 #true) '())
                      (make-tree (make-node 60 100 #true 75 85 70 140 #true) '())))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;node-after-button-down: Node Int Int -> Node
;;GIVEN: Node and mouse positions
;;RETURNS:Node after button-down
;;EXAMPLES:(node-after-button-down (make-node 12 17 UNSELECTED 30 30 34 78) 30 30)
;;->(make-node 12 17 #true 30 30 18 13)
;;STRATEGY: Use the template of node 
(define (node-after-button-down n mx my)   
  (if (mouse-event-inside-node? n mx my)
      (make-node (node-x n) (node-y n) SELECTED mx my
                 (- mx (node-x n)) (- my (node-y n)) (node-circle? n)) 
      n))

;;TESTS:
(begin-for-test
  (check-equal?
   (node-after-button-down (make-node 22 27 UNSELECTED 30 30 34 78 true) 30 30)
   (make-node 22 27 #true 30 30 8 3 #true))
  
  (check-equal?
   (node-after-button-down NODE2 110 150)
   (make-node 60 100 #true 75 85 70 140 #true))
  (check-equal?
   (node-after-button-down (make-node 50 40 UNSELECTED 80 75 95 85 true) 70 55)
   (make-node 50 40 #false 80 75 95 85 #true)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;     
;;mouse-event-inside-node? : Node Int Int -> Boolean
;;GIVEN: node and mouse positions
;;RETURNS: true if mouse positions are inside the circle or square
;;EXAMPLES:(mouse-event-inside-node? (make-node 12 17 UNSELECTED 30 30 34 78 false) 30 30)->#true
;;STRATEGY: Use the template of node  
(define (mouse-event-inside-node? n mx my)
  (if (node-circle? n)
      (>= (sqr CIRCLE-RADIUS) 
          (+ (sqr (- (node-x n) mx)) (sqr (- (node-y n) my))))
      (and
       (and ( >= mx (- (node-x n) CIRCLE-RADIUS)) ( <= mx (+ (node-x n) CIRCLE-RADIUS)))
       (and ( >= my (- (node-y n) CIRCLE-RADIUS)) ( <= my (+ (node-y n) CIRCLE-RADIUS))))))

;;TESTS:
(begin-for-test
  (check-equal?
   (mouse-event-inside-node? (make-node 12 17 UNSELECTED 30 30 34 78 false) 30 30)true)
  (check-equal?
   (mouse-event-inside-node? (make-node  50 40 SELECTED 80 75 95 85 true) 50 50)true))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;button-up:World Int Int -> World
;;GIVEN: World (x,y) coordinates of a mouse 
;;RETURNS: world after button up event
;;EXAMPLES:(button-up(make-world (list
;;   (make-tree (make-node 12 17 SELECTED 30 30 34 78) empty))) 30 30)
;;->(make-world (list (make-tree (make-node 12 17 #false 30 30 18 13) '())))
;;STRATEGY: Use the template of world and HOF map on world-lot
(define (button-up w mx my)
  (make-world
   (map
    (lambda (t)
      (tree-after-button-up t mx my))
    (world-lot w))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;tree-after-button-up: Tree Int Int-> Tree
;;GIVEN: Tree and mouse positions
;;RETURNS:Tree after button-up
;;EXAMPLES:(tree-after-button-up (make-tree (make-node 12 17 SELECTED 30 30 34 78) empty) 30 30)
;;->(make-tree (make-node 12 17 #false 30 30 18 13) '())
;;HALTING MEASURE: the number of element in lot of t
;;STRATEGY:Use the template of tree and HOF on tree-lot

(define (tree-after-button-up t mx my)
  (make-tree
   (node-after-button-up (tree-node t) mx my)
   (map (lambda (x) (tree-after-button-up x mx my)) (tree-lot t))))

;;TESTS:
(begin-for-test
  (check-equal?
   (tree-after-button-up (make-tree NODE1 (list TREE2 TREE3)) 55 65)
   (make-tree
    (make-node 50 40 #false 55 65 5 25 #true)
    (list
     (make-tree (make-node 60 100 #false 55 65 -5 -35 #true) '())
     (make-tree (make-node 160 120 #false 190 160 70 170 #true)
                (list (make-tree (make-node 50 40 #false 55 65 5 25 #true) '())
                      (make-tree (make-node 60 100 #false 55 65 -5 -35 #true) '())))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;node-after-button-up: Node Int Int-> Node
;;GIVEN: Node and mouse positions
;;RETURNS:Node after button-up
;;EXAMPLES:(node-after-button-up (make-node 12 17 SELECTED 30 30 34 78) 30 30)
;;->(make-node 12 17 #false 30 30 18 13)
;;STRATEGY:Use the template of node
(define (node-after-button-up n mx my)   
  (if (node-selected? n)
      (make-node
       (node-x n)
       (node-y n) UNSELECTED mx my
       (- mx (node-x n)) (- my (node-y n)) (node-circle? n))
      n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;     
;;drag:World Int Int -> World
;;GIVEN: World (x,y) coordinates of a mouse
;;RETURNS: World after drag event
;;EXAMPLES:(drag (make-world (list
;;  (make-tree (make-node 12 17 SELECTED 30 30 18 13) empty))) 40 60)
;;->(make-world (list (make-tree (make-node 22 47 #true 40 60 18 13) '())))
;;STRATEGY:Use the template of world and HOF map on world-lot
(define (drag w mx my)
  (make-world
   (map
    (lambda (t)
      (tree-after-drag t mx my))
    (world-lot w))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;tree-after-drag: Tree Int Int-> Tree
;;GIVEN: Tree and mouse positions
;;RETURNS:Tree after drag
;;EXAMPLES:(tree-after-drag (make-tree (make-node 12 17 SELECTED 30 30 18 13) empty) 40 60)
;;->(list (make-node 22 47 #true 40 60 18 13)
;;HALTING MEASURE: the number of element in lot of t
;;STRATEGY: Use the template of tree and HOF map on tree-lot

(define (tree-after-drag t mx my)
  (cond
    [(empty? t) empty]
    [(node-selected? (tree-node t))
     (make-tree
      (node-after-drag
       (tree-node t) mx my (- mx (node-mx (tree-node t))) (- my (node-my (tree-node t))))
      (map
       (lambda (x)
         (subtree-after-drag x mx my (- mx (node-mx (tree-node t))) (- my (node-my (tree-node t)))))
       (tree-lot t)))]
    [else (make-tree
           (tree-node t)
           (map (lambda (y)
                  (tree-after-drag y mx my )) (tree-lot t)))]))

;;TEST:
(begin-for-test
  (check-equal? (tree-after-drag TREE1 95 85)
                (make-tree (make-node 90 60 #true 95 85 5 25 #true) '()))
  
  (check-equal? (tree-after-drag '() 60 50)'())
  
  (check-equal?
   (tree-after-drag TREE4 80 95)
   (make-tree
    (make-node 60 120 #false 190 160 70 170 #true)
    (list (make-tree (make-node 75 70 #true 80 95 5 25 #true) '())
          (make-tree (make-node 160 120 #false 190 160 70 170 #true)
                     (list (make-tree (make-node 75 70 #true 80 95 5 25 #true) '())
                           (make-tree (make-node 65 110 #true 80 95 15 -15 #true) '()))))))
  (check-equal?
   (tree-after-drag TREE3 150 170)
   (make-tree
    (make-node 160 120 #false 190 160 70 170 #true)
    (list (make-tree (make-node 145 145 #true 150 170 5 25 #true) '())
          (make-tree (make-node 135 185 #true 150 170 15 -15 #true) '()))))
  (check-equal?
   (tree-after-drag TREE7 150 170)
   (make-tree
    (make-node 145 145 #true 150 170 5 25 #true)
    (list (make-tree
           (make-node 145 145 #true 150 170 5 25 #true)
           (list (make-tree (make-node 155 205 #true 150 170 -5 -35 #true) '())))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
;;subtree-after-drag: Tree Int Int Int Int -> tree
;;GIVEN: Tree and mouse and relative position of tree from the mouse positions.
;;RETURNS:Tree at the given mouse positions
;;EXAMPLES:(subtree-after-drag (make-tree (make-node 12 17 SELECTED 30 30 18 13 true) '()) 40 44 100 120)->
;;(make-tree (make-node 112 137 #true 40 44 -72 -93 #true) '())
;;HALTING MEASURE: the number of element in lot of t
;;STRATEGY:Use HOF map on tree
(define (subtree-after-drag t mx my relativex relativey)
  (make-tree
   (node-after-drag (tree-node t)  mx my relativex relativey)
   (map (lambda (a) (subtree-after-drag a mx my relativex relativey)) (tree-lot t))))

;;TEST:
(begin-for-test
  (check-equal? (subtree-after-drag TREE1 80 95 30 25)
                (make-tree (make-node 80 65 #true 80 95 0 30 #true) '()))
  
  (check-equal?
   (subtree-after-drag TREE4 80 95 30 25)
   (make-tree (make-node 90 145 #false 80 95 -10 -50 #true)
              (list
               (make-tree (make-node 80 65 #true 80 95 0 30 #true) '())
               (make-tree (make-node 190 145 #false 80 95 -110 -50 #true)
                          (list (make-tree (make-node 80 65 #true 80 95 0 30 #true) '())
                                (make-tree (make-node 90 125 #true 80 95 -10 -30 #true) '())))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
;;node-after-drag: Node Int Int-> Node
;;GIVEN: Node and relative distances of mouse positions
;;RETURNS:Node after drag
;;EXAMPLES:(node-after-drag (make-node 12 17 SELECTED 30 30 18 13 true) 40 44)
;;->(make-node 22 31 #true 40 44 18 13)
;;STRATEGY:Use the template of node

(define (node-after-drag n mx my rx ry)
  (make-node (+ (node-x n) rx)
             (+ (node-y n) ry)
             (node-selected? n) mx my (- mx (+ (node-x n) rx))
             (- my (+ (node-y n) ry)) (node-circle? n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;world-after-key-event : World KeyEvent -> World
;;GIVEN: a World and a key event
;;RETURNS: the state of the world as it should be following the given key event
;;EXAMPLES:(world-after-key-event (make-world (list (make-tree (make-node 12 17 SELECTED 32 32 20 15 true)
;;(list (make-tree (make-node 11 11 UNSELECTED 32 32 21 21 true) empty))))) "d")
;; ->(make-world (list (make-tree (make-node 11 11 #false 32 32 21 21 #true) '())))
;;STRATEGY:Divide into cases on key event

(define (world-after-key-event w ke)
  (cond
    [(key=? "c" ke) (make-world (trees-after-cs (world-lot w) true))]
    [(key=? "s" ke) (make-world (trees-after-cs (world-lot w) false))]
    [(key=? "d" ke) (make-world (trees-after-d (world-lot w)))]
    [else w]))

;;TEST:
(begin-for-test
  (check-equal?
   (world-after-key-event (make-world (list TREE1 TREE2)) "c")
   (make-world
    (list
     (make-tree (make-node 50 40 #true 55 65 12 40 #true)
                (list (make-tree (make-node 50 100 #false 0 0 0 0 #true) '())))
     (make-tree (make-node 60 100 #true 75 85 70 140 #true)
                (list (make-tree (make-node 60 160 #false 0 0 0 0 #true) '()))))))
  
  (check-equal?
   (world-after-key-event (make-world (list TREE1 TREE2)) "s")
   (make-world
    (list
     (make-tree (make-node 50 40 #true 55 65 12 40 #true)
                (list (make-tree (make-node 50 100 #false 0 0 0 0 #false) '())))
     (make-tree (make-node 60 100 #true 75 85 70 140 #true)
                (list (make-tree (make-node 60 160 #false 0 0 0 0 #false) '()))))))
  
  (check-equal?
   (world-after-key-event (make-world (list TREE1 TREE2)) "d")
   (make-world '()))
  
  (check-equal?
   (world-after-key-event (make-world (list TREE1 TREE2)) "e")
   (make-world (list (make-tree (make-node 50 40 #true 55 65 12 40 #true) '())
                     (make-tree (make-node 60 100 #true 75 85 70 140 #true) '())))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
;;trees-after-cs: ListOfTree Boolean -> ListOfTree
;;GIVEN:List of tree and Circle/Square
;;RETURNS:List of tree after create a circle/square
;;EXAMPLES:(trees-after-cs (list (make-tree (make-node 11 11 UNSELECTED 32 32 21 21 true) empty)
;;   (make-tree (make-node 21 21 UNSELECTED 32 32 31 45 true) empty)) true)
;;->(list
;;  (make-tree (make-node 250 20 #false 0 0 0 0 #true) '())
;;  (make-tree (make-node 11 11 #false 32 32 21 21 #true) '())
;;  (make-tree (make-node 21 21 #false 32 32 31 45 #true) '()))
;;HALTING MEASURE: the number of element in lot
;;STRATEGY: Use the template of tree and HOF map on tree
(define (trees-after-cs lot b)
  (cond
    [(empty? lot)
     (list (make-tree-after-cs b))]
    [(not (all-unselected? lot))
     (cons (make-tree-after-cs b) lot)]
    [else (map (lambda (t) (tree-after-cs t b)) lot)]))

;;TEST:
(begin-for-test
  (check-equal?
   (trees-after-cs '() true)
   (list (make-tree (make-node 250 20 #false 0 0 0 0 #true) '())))
  (check-equal?
   (trees-after-cs (list TREE8) true)
   (list(make-tree (make-node 250 20 #false 0 0 0 0 #true) '())
        (make-tree
         (make-node 160 120 #false 190 160 70 170 #true)
         (list (make-tree (make-node 60 120 #false 190 160 70 170 #true) '()))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;make-tree-after-cs: Boolean -> ListOfTree
;;GIVEN:A boolean indicates Circle/Square
;;RETURNS;a new tree after keyevent c/s
;;EXAMPLES:(make-tree-after-cs b) =  (make-tree
;;   (make-node (/ CANVAS-WIDTH 2) CIRCLE-RADIUS UNSELECTED 0 0 0 0 b) empty)
;;STRATEGY: Use the template of tree

(define (make-tree-after-cs b)
  (make-tree
   (make-node (/ CANVAS-WIDTH 2) CIRCLE-RADIUS UNSELECTED 0 0 0 0 b)
   empty))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; all-unselected?: ListOfTree -> Boolean
;; GIVEN: a list of trees
;; RETURNS: whether nodes in this list of trees are all unselected or not
;; EXAMPLES:all-unselected?
; (list
;(make-tree (make-node 250 20 #false 0 0 0 0 #true) '())
;(make-tree (make-node 11 11 #false 32 32 21 21 #true) '())
;(make-tree (make-node 21 21 #false 32 32 31 45 #true) '())))
;->#false
;;HALTING MEASURE: the number of element in lot
;;STRATEGY:Use HOF ormap on node
(define (all-unselected? lot)
  (if (empty? lot) false
      (ormap
       (lambda (x)
         (nodes-all-unselected? x))
       lot))) 
;;TEST:
(begin-for-test
  (check-equal?(all-unselected? (list TREE1 TREE3))true)
  (check-equal?(all-unselected? (list TREE8))false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; nodes-all-unselected?: Tree -> Boolean
;; GIVEN: a tree
;; RETURNS: whether nodes in this tree are all unselected or not
;; EXAMPLES: (nodes-all-unselected? (make-tree (make-node 250 20 #false 0 0 0 0 #true) '()))->#false
;; HALTING MEASURE: the number of element in lot of t
;; STRATEGY:Use the template of tree
(define (nodes-all-unselected? t)
  (or
   (node-selected? (tree-node t))
   (all-unselected? (tree-lot t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;tree-after-cs: Tree Boolean-> Tree
;;GIVEN: Tree and true if it is circle
;;RETURNS: Tree after keyevent c/s
;;EXAMPLES:(tree-after-cs (make-tree (make-node 250 20 #false 0 0 0 0 #true) '()) true)
;; ->(make-tree (make-node 250 20 #false 0 0 0 0 #true) '())
;;HALTING MEASURE: the number of element in lot of t
;;STRATEGY: Use HOF on tree-lot
(define (tree-after-cs t b)
  (cond
    [(empty? t) empty]
    [(not (node-selected? (tree-node t)))
     (make-tree (tree-node t)
                (map (lambda (x) (tree-after-cs x b)) (tree-lot t)))]
    [(empty?
      (tree-lot t))
     (make-tree (tree-node t)
                (list (make-tree (create-node-beneath t b) empty)))]                                            
    [else
     (make-tree
      (tree-node t)
      (cons
       (create-node-left  t b)
       (map (lambda (x) (tree-after-cs x b)) (tree-lot t))))]))
;;TESTS:
(begin-for-test
  (check-equal?
   (tree-after-cs TREE8 true)
   (make-tree (make-node 160 120 #false 190 160 70 170 #true)
              (list (make-tree (make-node 60 120 #false 190 160 70 170 #true) '()))))
  (check-equal? (tree-after-cs '() true)'())
  
  (check-equal?
   (tree-after-cs TREE6 false)
   (make-tree
    (make-node 50 40 #true 55 65 12 40 #true)
    (list (make-tree (make-node 0 100 #false 0 0 0 0 #false) '())
          (make-tree (make-node 60 100 #true 75 85 70 140 #true)
                     (list (make-tree (make-node 60 160 #false 0 0 0 0 #false) '())))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
;; leftmost-x: ListOfTree -> Integer
;; GIVEN: List of tree
;; RETURNS: the x coordinate of leftmost node
;; EXAMPLES:(leftmost-x  (list
;;(make-tree (make-node 250 20 #false 0 0 0 0 #true) '())
;;(make-tree (make-node 11 11 #false 32 32 21 21 #true) '())
;;(make-tree (make-node 21 21 #false 32 32 31 45 #true) '())))->11
;; HALTING MEASURE: the length of lot
;; STRATEGY:Use the template of the tree
(define (leftmost-x lot)
  (if (empty? (rest lot)) (node-x (tree-node (first lot)))
      (min (node-x (tree-node (first lot))) (leftmost-x (rest lot)))))

;;TESTS:
(begin-for-test
  (check-equal? (leftmost-x (list TREE3 TREE5))60))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
;; create-node-left: Tree Boolean -> Tree 
;; GIVEN: a Tree and a boolean
;; RETURNS: a Tree with an added node according to the boolean
;; EXAMPLES:(create-node-left (make-tree (make-node 250 20 #false 0 0 0 0 #true)
;;   (list (make-tree (make-node 50 70 #false 0 0 0 0 #true)'())
;;   (make-tree (make-node 80 90 #false 0 0 0 0 #true)'()))) true)
;;->(make-tree (make-node -10 80 #false 0 0 0 0 #true) '())
;; STRATEGY:Use the template of tree
(define (create-node-left t b)
  (make-tree
   (make-node (- (leftmost-x (tree-lot t)) (* 3 CIRCLE-RADIUS))
              (+ (node-y (tree-node t)) (* 3 CIRCLE-RADIUS))
              UNSELECTED 0 0 0 0 b) empty))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
;; create-node-beneath: Tree Boolean -> Tree
;; GIVEN: a Tree and a boolean
;; RETURNS: a Tree with an added node according to the boolean
;; EXAMPLES:(create-node-beneath (make-tree (make-node 250 20 #false 0 0 0 0 #true)
;;   (list (make-tree (make-node 50 70 #false 0 0 0 0 #true)'())
;;   (make-tree (make-node 80 90 #false 0 0 0 0 #true)'()))) true) 
;;->(make-node 250 80 #false 0 0 0 0 #true)
;; STRATEGY:Use the template of node
(define (create-node-beneath t b)
  (make-node (node-x (tree-node t))
             (+ (node-y (tree-node t)) (* 3 CIRCLE-RADIUS))
             UNSELECTED 0 0 0 0 b))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
;;trees-after-d: ListOfTree -> ListOfTree
;;GIVEN:List of tree
;;RETURNS: List of tree after deleting 
;;EXAMPLES:(trees-after-d (list (make-tree (make-node 50 70 #false 0 0 0 0 #true)'())
;;  (make-tree (make-node 80 90 #false 0 0 0 0 #true)'())))
;;->(list (make-tree (make-node 50 70 #false 0 0 0 0 #true) '())
;;  (make-tree (make-node 80 90 #false 0 0 0 0 #true) '()))
;;Halting Measure: the length of lot
;;STRATEGY:Combine simple functions

(define (trees-after-d lot)
  (if (empty? lot)
      empty
      (append (tree-after-d (first lot))
              (trees-after-d (rest lot)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
;;tree-after-d: Tree -> ListOfTree
;;GIVEN:a tree
;;RETURNS: a List of tree after deleting
;;EXAMPLES:(tree-after-d (make-tree (make-node 50 70 #false 0 0 0 0 #true)'()))->
;;(list (make-tree (make-node 50 70 #false 0 0 0 0 #true) '()))
;;HALTING MEASURE: the number of element in lot of t
;;STRATEGY:Use the template of tree

(define (tree-after-d t)
  (cond
    [(node-selected? (tree-node t))
     (trees-after-d (tree-lot t))]
    [(empty? (tree-lot t))
     (list t)]
    [else
     (list
      (make-tree (tree-node t)
                 (trees-after-d (tree-lot t))))]))

;;TESTS:
(begin-for-test
  (check-equal?(tree-after-d TREE3)
               (list
                (make-tree (make-node 160 120 #false 190 160 70 170 #true) '())))
  (check-equal?(tree-after-d TREE8)
               (list
                (make-tree
                 (make-node 160 120 #false 190 160 70 170 #true)
                 (list(make-tree (make-node 60 120 #false 190 160 70 170 #true) '()))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
;;world-to-trees : World -> ListOfTree
;;GIVEN: a World
;;RETURNS: a list of all the trees in the given world.
;;EXAMPLES: (world-to-trees (make-world (list (make-tree (make-node 50 70 #false 0 0 0 0 #true)'())
;;(make-tree (make-node 80 90 #false 0 0 0 0 #true)'())))) ->
;;(list (make-tree (make-node 50 70 #false 0 0 0 0 #true) '())
;; (make-tree (make-node 80 90 #false 0 0 0 0 #true) '()))
;;STRATEGY:Use the template of world
(define (world-to-trees w)
  (world-lot w))

;;TESTS:
(begin-for-test
  (check-equal?
   (world-to-trees (make-world (list TREE8 TREE6)))
   (list
    (make-tree (make-node 160 120 #false 190 160 70 170 #true)
               (list (make-tree (make-node 60 120 #false 190 160 70 170 #true) '())))
    (make-tree (make-node 50 40 #true 55 65 12 40 #true)
               (list (make-tree (make-node 60 100 #true 75 85 70 140 #true) '()))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
;;tree-to-root : Tree -> Node
;;GIVEN: a tree
;;RETURNS: the node at the root of the tree
;;EXAMPLES:(tree-to-root (make-tree (make-node 80 90 #false 0 0 0 0 #true)
;; (list (make-tree (make-node 50 70 #false 0 0 0 0 #true)'())
;;(make-tree (make-node 80 90 #false 0 0 0 0 #true)'()))))->
;;(make-node 80 90 #false 0 0 0 0 #true)
;;STRATEGY:Use the template of tree
(define (tree-to-root t)
  (tree-node t))
;;TESTS:
(begin-for-test
  (check-equal? (tree-to-root TREE4)(make-node 60 120 #false 190 160 70 170 #true)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
;;tree-to-sons : Tree -> ListOfTree
;;GIVEN: a tree
;;RETURNS: the data associated with the immediate subtrees of the given tree. 
;;EXAMPLE:(tree-to-sons(make-tree (make-node 80 90 #false 0 0 0 0 #true)
;; (list (make-tree (make-node 50 70 #false 0 0 0 0 #true)'())
;;(make-tree (make-node 80 90 #false 0 0 0 0 #true)'()))))->
;;(list (make-tree (make-node 50 70 #false 0 0 0 0 #true) '())
;; (make-tree (make-node 80 90 #false 0 0 0 0 #true) '()))
;;STRATEGY: Use the template of tree
(define (tree-to-sons t)
  (tree-lot t))
;;TESTS:
(begin-for-test
  (check-equal?
   (tree-to-sons TREE4)
   (list
    (make-tree (make-node 50 40 #true 55 65 12 40 #true) '())
    (make-tree (make-node 160 120 #false 190 160 70 170 #true)
               (list (make-tree (make-node 50 40 #true 55 65 12 40 #true) '())
                     (make-tree (make-node 60 100 #true 75 85 70 140 #true) '()))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
;;node-to-center : Node -> Posn
;;GIVEN: node
;;RETURNS: the center of the given node as it is to be displayed on the
;;scene.
;;Note: this function returns a Posn (an ISL builtin).  This is for the
;;convenience of the testing framework, and you may or may not wish to
;;represent the center of the node in this way.
;;EXAMPLES: (node-to-center (make-node 50 70 #false 0 0 0 0 #true))->(make-posn 50 70)
;;STRATEGY: Use the template of node
(define (node-to-center n)
  (make-posn (node-x n) (node-y n)))
;;TESTS:
(begin-for-test
  (check-equal? (node-to-center NODE1)(make-posn 50 40)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
;;node-to-selected? : Node -> Boolean
;;GIVEN:node
;;RETURNS: true iff the given node is selected.
;;EXAMPLES:(node-to-selected? (make-node 50 70 #false 0 0 0 0 #true))->#false
;;STRATEGY: Use the template of node
(define (node-to-selected? n)
  (node-selected? n))
;;TESTS:
(begin-for-test
  (check-equal? (node-to-selected?  NODE1)true))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
;; world-to-scene : World -> Scene
;; GIVEN: a world object which contains ListOfTree
;; RETURNS: a Scene that portrays the given world with the list of trees
;; drawn at the given coordinates
;; EXAMPLES:(world-to-scene (make-world (list (make-tree (make-node 50 70 #false 0 0 0 0 #true)'())
;;(make-tree (make-node 80 90 #false 0 0 0 0 #true)'())))) -> scene-with-two-nodes
;; STRATEGY: Use the template of world
(define (world-to-scene w)
  (trees-to-scene (world-lot w)))
;;TESTS:
(begin-for-test
  (check-equal? (world-to-scene (make-world empty)) EMPTY-CANVAS))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
;; trees-to-scene : ListOfTree -> Scene
;; GIVEN: a list of trees
;; RETURNS: a Scene that portrays the the list of trees
;; drawn at the given coordinates
;; EXAMPLES:(trees-to-scene (list TREE1 TREE2)) ->scene-with-2-nodes
;; HALTING MEASURE: the number of element in lot
;; STRATEGY:Use the template of LOT
(define (trees-to-scene lot)
  (if (empty? lot)
      EMPTY-CANVAS
      
      (overlay (tree-to-scene (first lot))
               (trees-to-scene (rest lot)))))
;;TESTS:
(begin-for-test
  (check-equal? (trees-to-scene (list TREE1 TREE2)) scene-with-2-nodes))

(define scene-with-2-nodes
  
  (place-image (circle 20 "solid" "green") 50 40  
               (place-image (circle 20 "solid" "green") 60 100 EMPTY-CANVAS)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
;; tree-to-scene : Tree -> Scene
;; GIVEN: a tree
;; RETURNS: a Scene that portrays the given tree at the given coordinates
;; EXAMPLES:(tree-to-scene TREE1) -> Scene with NODE1
;; HALTING MEASURE: the number of element in lot of t
;; STRATEGY: Use the template of Tree
(define (tree-to-scene t)
  (place-image
   (node-to-scene (tree-node t))
   (node-x (tree-node t))
   (node-y (tree-node t))
   (sons-to-scene (tree-lot t) (tree-node t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
;; node-to-scene : node -> Scene
;; GIVEN: a node
;; RETURNS: a Scene that portrays the root node at the given coordinates
;; EXAMPLES:(node-to-scene NODE6)-> solid-square)
;; STRATEGY:Divide into cases based on node selected of circle/square
(define (node-to-scene n)
  (cond
    [(and (node-selected? n) (node-circle? n))
     (circle CIRCLE-RADIUS "solid" "green")]
    [(and (not (node-selected? n)) (node-circle? n))
     (circle CIRCLE-RADIUS "outline" "green")]
    [(and (node-selected? n) (not (node-circle? n)))
     (square SQUARE-SIDE "solid" "green")]
    [else
     (square SQUARE-SIDE "outline" "green")]))
;TESTS:
(begin-for-test
  (check-equal? (node-to-scene NODE6) solid-square)
  (check-equal? (node-to-scene NODE3) outline-circle)
  (check-equal? (node-to-scene NODE5) outline-square))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
;; sons-to-scene : ListOfTree Node -> Scene
;; GIVEN: a list of trees and node
;; RETURNS: a Scene that portrays sons and lines
;; of the given trees at the given coordinates with parent nodes
;; EXAMPLES:(sons-to-scene (list TREE1 TREE2) NODE1)-> (scene-with-connected-nodes)
;; HALTING MEASURE: the number of element in lot
;; STRATEGY:Use the template of LOT
(define (sons-to-scene lot n)
  (if (empty? lot)
      (rectangle CANVAS-WIDTH CANVAS-HEIGHT "outline" "black")
      (overlay (subtree-to-scene (first lot) n)
               (sons-to-scene (rest lot) n))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
;; subtrees-to-scene : Tree Node -> Scene
;; GIVEN: a tree and a node
;; RETURNS: a Scene that portrays tree with lines and sons
;; EXAMPLES:(subtree-to-scene TREE1 NODE3)-> scene-with-line-circle
;; HALTING MEASURE: the number of element in lot
;; STRATEGY:Use the template of tree
(define (subtree-to-scene t n)
  (overlay
   (scene+line
    (place-image (node-to-scene (tree-node t))
                 (node-x (tree-node t))
                 (node-y (tree-node t))
                 (rectangle CANVAS-WIDTH CANVAS-HEIGHT "outline" "black"))
    (node-x (tree-node t))
    (node-y (tree-node t))
    (node-x n)
    (node-y n)
    "blue")
   (sons-to-scene (tree-lot t) (tree-node t))))

;;TESTS:
(begin-for-test
  (check-equal? (subtree-to-scene TREE1 NODE3) scene-with-line-circle)
  (check-equal? (subtree-to-scene TREE6 NODE3) scene-with-line-circle-2))
