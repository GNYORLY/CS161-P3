;
; CS161 Hw3: Sokoban
; 
; *********************
;    READ THIS FIRST
; ********************* 
;
; All functions that you need to modify are marked with 'EXERCISE' in their header comments.
; Do not modify a-star.lsp.
; This file also contains many helper functions. You may call any of them in your functions.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any node.
; That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your heuristic functions.
; Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on memory.
; So, it may crash on some hard sokoban problems and there is no easy fix (unless you buy 
; Allegro). 
; Of course, other versions of Lisp may also crash if the problem is too hard, but the amount
; of memory available will be relatively more relaxed.
; Improving the quality of the heuristic will mitigate this problem, as it will allow A* to
; solve hard problems with fewer node expansions.
; 
; In either case, this limitation should not significantly affect your grade.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition (which will
; affect your score).
;  
;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.
;

;
; For reloading modified code.
; I found this easier than typing (load "filename") every time.
;
(defun reload()
(load "hw3.lsp")
)

;
; For loading a-star.lsp.
;
(defun load-a-star()
(load "a-star.lsp"))

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all()
(reload)
(load-a-star)
)

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
;
;
(defun sokoban (s h)
(a* s #'goal-test #'next-states h)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.
;

;
; For reloading modified code.
; I found this easier than typing (load "filename") every time.
;
(defun reload()
  (load "hw3.lsp")
  )

;
; For loading a-star.lsp.
;
(defun load-a-star()
  (load "a-star.lsp"))

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all()
  (reload)
  (load-a-star)
  )

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
;
;
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (= v blank)
  )

(defun isWall (v)
  (= v wall)
  )

(defun isBox (v)
  (= v box)
  )

(defun isKeeper (v)
  (= v keeper)
  )

(defun isStar (v)
  (= v star)
  )

(defun isBoxStar (v)
  (= v boxstar)
  )

(defun isKeeperStar (v)
  (= v keeperstar)
  )

;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
  )

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
;
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.
;
(defun getKeeperPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getKeeperColumn (car s) 0)))
	     (if x
		 ;keeper is in this row
		 (list x row)
		 ;otherwise move on
		 (getKeeperPosition (cdr s) (+ row 1))
		 );end if
	       );end let
	 );end t
	);end cond
  );end defun

;
; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond ((null L) nil)
	(t (let ((cur (car L))
		 (res (cleanUpList (cdr L)))
		 )
	     (if cur
		 (cons cur res)
		  res
		 )
	     );end let
	   );end t
	);end cond
  );end

; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of a Sokoban game.
; (no box is on a non-goal square)
;
; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.
;

; If the current element of s is a list, call goal-test on
; both the current element and the rest of s. Otherwise, call
; isBox() on the element. If isBox() ever returns true,
; return nil. If the end of the row is reached, return t.
; And the result of each row in the state.
(defun goal-test (s)
  (cond ((null s) t)
        ((listp (car s)) (and (goal-test (car s)) (goal-test (cdr s))))
        ((isBox (car s)) nil)
        (t (goal-test (cdr s)))
      )
  );end defun


; EXERCISE: Modify this function to return the list of
; sucessor states of s.
;
; This is the top-level next-states (successor) function.
; Some skeleton code is provided below.
; You may delete them totally, depending on your approach.
;
; If you want to use it, you will need to set 'result' to be
; the set of states after moving the keeper in each of the 4 directions.
; A pseudo-code for this is:
;
; ...
; (result (list (try-move s UP) (try-move s DOWN) (try-move s LEFT) (try-move s RIGHT)))
; ...
;
; You will need to define the function try-move and decide how to represent UP,DOWN,LEFT,RIGHT.
; Any NIL result returned from try-move can be removed by cleanUpList.
;
;
(defun next-states (s)
  (let* ((pos (getKeeperPosition s 0))
	 (x (car pos))
	 (y (cadr pos))
	 ;x and y are now the coordinate of the keeper in s.
     ;result is next states for each of the possible moves u, d, l, and r
	 (result (list (try-move s 'u) (try-move s 'd) (try-move s 'l) (try-move s 'r)))
	 )
    (cleanUpList result);end
   );end let
  );

; get-square returns the square content at position (r, c) in s
(defun get-square (s r c)
    (cond ((null s) '1) ;if out-of-bounds, return a wall
          ((listp (car s)) (cond                   ;if current element is a list,
              ((= r 0) (get-square (car s) r c))   ;if r=0, call get-square on list
              (t (get-square (cdr s) (- r 1) c)))) ;else, go to next row and decrement r
          (t (cond ((= c 0) (car s))                     ;if c=0, return current element
                   (t (get-square (cdr s) r (- c 1)))))) ;else, go to next col and decrement c
    )

; set-square returns the state after replacing the square content at position (r, c) with v in s
(defun set-square (s r c v)
    (cond ((null s) nil) ; if out-of-bounds, return NIL
        ; if current element is a list and r=0, call set-square on that element and
        ; insert it into the tail of s, else go to the next row
          ((listp (car s)) (cond
              ((= r 0) (cons (set-square (car s) r c v) (cdr s)))
              (t (cons (car s) (set-square (cdr s) (- r 1) c v)))))
        ; if currently looking inside a row, if c=0, insert v into the tail of row
        ; else, go to the next element
          (t (cond ((= c 0) (cons v (cdr s)))
                   (t (cons (car s) (set-square (cdr s) r (- c 1) v))))))
    )

; try-move returns the state after moving in direction d from s
; the possible moves for d are represented by "u d l r" for up down left and right
(defun try-move (s d)
    (let* ((pos (getKeeperPosition s 0)) (keeper (get-square s (cadr pos) (car pos))))
          (cond ((null s) nil)
          ;if direction is up, check the contents of the square above (r-1, c) and above2 (r-2, c) to determine the next state
          ;if there is a blank in the next position move the keeper there, if there is a wall, return NIL
          ;if there is a box or boxstar, check the position after the box (next2), if it is blank or a goal, move the box there
          ;and move the keeper to the previous position of the box, else return NIL.
          ;If the box moves into a goal, make it a boxstar, if the keeper moves into a goal, make it a keeperstar
          ;follow the same process for each of the four possible directions
          ((equal d 'u) (let* ((next (get-square s (- (cadr pos) 1) (car pos)))(next2 (get-square s (- (cadr pos) 2) (car pos))))
               (cond
                   ((isBlank next) (cond
                       ((isKeeperStar keeper) (set-square (set-square s (cadr pos) (car pos) 4) (- (cadr pos) 1) (car pos) 3))
                       (t (set-square (set-square s (cadr pos) (car pos) 0) (- (cadr pos) 1) (car pos) 3))))
                   ((isWall next) nil)
                   ((isBox next)
                       (cond ((isBlank next2) (cond ((isKeeperStar keeper)
                           (set-square (set-square (set-square s (cadr pos) (car pos) 4) (- (cadr pos) 1) (car pos) 3) (- (cadr pos) 2) (car pos) 2))
                           (t (set-square (set-square (set-square s (cadr pos) (car pos) 0) (- (cadr pos) 1) (car pos) 3) (- (cadr pos) 2) (car pos) 2))))
                       ((isStar next2) (cond ((isKeeperStar keeper)
                           (set-square (set-square (set-square s (cadr pos) (car pos) 4) (- (cadr pos) 1) (car pos) 3) (- (cadr pos) 2) (car pos) 5))
                           (t (set-square (set-square (set-square s (cadr pos) (car pos) 0) (- (cadr pos) 1) (car pos) 3) (- (cadr pos) 2) (car pos) 5))))
                       (t nil)))
                   ((isStar next) (cond
                       ((isKeeperStar keeper) (set-square (set-square s (cadr pos) (car pos) 4) (- (cadr pos) 1) (car pos) 6))
                       (t (set-square (set-square s (cadr pos) (car pos) 0) (- (cadr pos) 1) (car pos) 6))))
                   ((isBoxStar next)
                       (cond ((isBlank next2) (cond ((isKeeperStar keeper)
                           (set-square (set-square (set-square s (cadr pos) (car pos) 4) (- (cadr pos) 1) (car pos) 6) (- (cadr pos) 2) (car pos) 2))
                           (t (set-square (set-square (set-square s (cadr pos) (car pos) 0) (- (cadr pos) 1) (car pos) 6) (- (cadr pos) 2) (car pos) 2))))
                       ((isStar next2) (cond ((isKeeperStar keeper)
                           (set-square (set-square (set-square s (cadr pos) (car pos) 4) (- (cadr pos) 1) (car pos) 6) (- (cadr pos) 2) (car pos) 5))
                           (t (set-square (set-square (set-square s (cadr pos) (car pos) 0) (- (cadr pos) 1) (car pos) 6) (- (cadr pos) 2) (car pos) 5))))
                       (t nil))))))
          ;if direction is down, check the contents of the square below (r+1, c) and below2 (r+2, c) to determine the next state
          ((equal d 'd) (let* ((next (get-square s (+ (cadr pos) 1) (car pos)))(next2 (get-square s (+ (cadr pos) 2) (car pos))))
               (cond
                   ((isBlank next) (cond
                       ((isKeeperStar keeper) (set-square (set-square s (cadr pos) (car pos) 4) (+ (cadr pos) 1) (car pos) 3))
                       (t (set-square (set-square s (cadr pos) (car pos) 0) (+ (cadr pos) 1) (car pos) 3))))
                   ((isWall next) nil)
                   ((isBox next)
                       (cond ((isBlank next2) (cond ((isKeeperStar keeper)
                           (set-square (set-square (set-square s (cadr pos) (car pos) 4) (+ (cadr pos) 1) (car pos) 3) (+ (cadr pos) 2) (car pos) 2))
                           (t (set-square (set-square (set-square s (cadr pos) (car pos) 0) (+ (cadr pos) 1) (car pos) 3) (+ (cadr pos) 2) (car pos) 2))))
                       ((isStar next2) (cond ((isKeeperStar keeper)
                           (set-square (set-square (set-square s (cadr pos) (car pos) 4) (+ (cadr pos) 1) (car pos) 3) (+ (cadr pos) 2) (car pos) 5))
                           (t (set-square (set-square (set-square s (cadr pos) (car pos) 0) (+ (cadr pos) 1) (car pos) 3) (+ (cadr pos) 2) (car pos) 5))))
                       (t nil)))
                   ((isStar next) (cond
                       ((isKeeperStar keeper) (set-square (set-square s (cadr pos) (car pos) 4) (+ (cadr pos) 1) (car pos) 6))
                       (t (set-square (set-square s (cadr pos) (car pos) 0) (+ (cadr pos) 1) (car pos) 6))))
                   ((isBoxStar next)
                       (cond ((isBlank next2) (cond ((isKeeperStar keeper)
                           (set-square (set-square (set-square s (cadr pos) (car pos) 4) (+ (cadr pos) 1) (car pos) 6) (+ (cadr pos) 2) (car pos) 2))
                           (t (set-square (set-square (set-square s (cadr pos) (car pos) 0) (+ (cadr pos) 1) (car pos) 6) (+ (cadr pos) 2) (car pos) 2))))
                       ((isStar next2) (cond ((isKeeperStar keeper)
                           (set-square (set-square (set-square s (cadr pos) (car pos) 4) (+ (cadr pos) 1) (car pos) 6) (+ (cadr pos) 2) (car pos) 5))
                           (t (set-square (set-square (set-square s (cadr pos) (car pos) 0) (+ (cadr pos) 1) (car pos) 6) (+ (cadr pos) 2) (car pos) 5))))
                       (t nil))))))
          ;if direction is left, check the contents of the square left (r, c-1) and left2 (r, c-2) to determine the next state
          ((equal d 'l) (let* ((next (get-square s (cadr pos) (- (car pos) 1) ))(next2 (get-square s (cadr pos) (- (car pos) 2))))
               (cond
                   ((isBlank next) (cond
                       ((isKeeperStar keeper) (set-square (set-square s (cadr pos) (car pos) 4) (cadr pos) (- (car pos) 1)  3))
                       (t (set-square (set-square s (cadr pos) (car pos) 0) (cadr pos) (- (car pos) 1)  3))))
                   ((isWall next) nil)
                   ((isBox next)
                       (cond ((isBlank next2) (cond ((isKeeperStar keeper)
                           (set-square (set-square (set-square s (cadr pos) (car pos) 4) (cadr pos) (- (car pos) 1)  3) (cadr pos) (- (car pos) 2) 2))
                           (t (set-square (set-square (set-square s (cadr pos) (car pos) 0) (cadr pos) (- (car pos) 1)  3) (cadr pos) (- (car pos) 2) 2))))
                       ((isStar next2) (cond ((isKeeperStar keeper)
                           (set-square (set-square (set-square s (cadr pos) (car pos) 4) (cadr pos) (- (car pos) 1)  3) (cadr pos) (- (car pos) 2) 5))
                           (t (set-square (set-square (set-square s (cadr pos) (car pos) 0) (cadr pos) (- (car pos) 1)  3) (cadr pos) (- (car pos) 2) 5))))
                       (t nil)))
                   ((isStar next) (cond
                       ((isKeeperStar keeper) (set-square (set-square s (cadr pos) (car pos) 4) (cadr pos) (- (car pos) 1)  6))
                       (t (set-square (set-square s (cadr pos) (car pos) 0) (cadr pos) (- (car pos) 1)  6))))
                   ((isBoxStar next)
                       (cond ((isBlank next2) (cond ((isKeeperStar keeper)
                           (set-square (set-square (set-square s (cadr pos) (car pos) 4) (cadr pos) (- (car pos) 1)  6) (cadr pos) (- (car pos) 2) 2))
                           (t (set-square (set-square (set-square s (cadr pos) (car pos) 0) (cadr pos) (- (car pos) 1)  6) (cadr pos) (- (car pos) 2) 2))))
                       ((isStar next2) (cond ((isKeeperStar keeper)
                           (set-square (set-square (set-square s (cadr pos) (car pos) 4) (cadr pos) (- (car pos) 1)  6) (cadr pos) (- (car pos) 2) 5))
                           (t (set-square (set-square (set-square s (cadr pos) (car pos) 0) (cadr pos) (- (car pos) 1)  6) (cadr pos) (- (car pos) 2) 5))))
                       (t nil))))))
          ;if direction is right, check the contents of the square right (r, c+1) and right2 (r, c+2) to determine the next state
          ((equal d 'r) (let* ((next (get-square s (cadr pos) (+ (car pos) 1) ))(next2 (get-square s (cadr pos) (+ (car pos) 2))))
               (cond
                   ((isBlank next) (cond
                       ((isKeeperStar keeper) (set-square (set-square s (cadr pos) (car pos) 4) (cadr pos) (+ (car pos) 1)  3))
                       (t (set-square (set-square s (cadr pos) (car pos) 0) (cadr pos) (+ (car pos) 1)  3))))
                   ((isWall next) nil)
                   ((isBox next)
                       (cond ((isBlank next2) (cond ((isKeeperStar keeper)
                           (set-square (set-square (set-square s (cadr pos) (car pos) 4) (cadr pos) (+ (car pos) 1)  3) (cadr pos) (+ (car pos) 2) 2))
                           (t (set-square (set-square (set-square s (cadr pos) (car pos) 0) (cadr pos) (+ (car pos) 1)  3) (cadr pos) (+ (car pos) 2) 2))))
                       ((isStar next2) (cond ((isKeeperStar keeper)
                           (set-square (set-square (set-square s (cadr pos) (car pos) 4) (cadr pos) (+ (car pos) 1)  3) (cadr pos) (+ (car pos) 2) 5))
                           (t (set-square (set-square (set-square s (cadr pos) (car pos) 0) (cadr pos) (+ (car pos) 1)  3) (cadr pos) (+ (car pos) 2) 5))))
                       (t nil)))
                   ((isStar next) (cond
                       ((isKeeperStar keeper) (set-square (set-square s (cadr pos) (car pos) 4) (cadr pos) (+ (car pos) 1)  6))
                       (t (set-square (set-square s (cadr pos) (car pos) 0) (cadr pos) (+ (car pos) 1)  6))))
                   ((isBoxStar next)
                       (cond ((isBlank next2) (cond ((isKeeperStar keeper)
                           (set-square (set-square (set-square s (cadr pos) (car pos) 4) (cadr pos) (+ (car pos) 1)  6) (cadr pos) (+ (car pos) 2) 2))
                           (t (set-square (set-square (set-square s (cadr pos) (car pos) 0) (cadr pos) (+ (car pos) 1)  6) (cadr pos) (+ (car pos) 2) 2))))
                       ((isStar next2) (cond ((isKeeperStar keeper)
                           (set-square (set-square (set-square s (cadr pos) (car pos) 4) (cadr pos) (+ (car pos) 1)  6) (cadr pos) (+ (car pos) 2) 5))
                           (t (set-square (set-square (set-square s (cadr pos) (car pos) 0) (cadr pos) (+ (car pos) 1)  6) (cadr pos) (+ (car pos) 2) 5))))
                       (t nil)))))))))

; EXERCISE: Modify this function to compute the trivial
; admissible heuristic.
;

;h0 just returns 0
(defun h0 (s)
    0
  )

; EXERCISE: Modify this function to compute the
; number of misplaced boxes in s.
;

;the implementation of h1 is very similar to goal-test
;for each row, call isBox on each square and add 1 to the count of boxes
;sum up the counts of each row
(defun h1 (s)
  (cond ((null s) 0)
        ((listp (car s)) (+ (h1 (car s)) (h1 (cdr s))))
        ((isBox (car s)) (+ 1 (h1 (cdr s))))
        (t (h1 (cdr s)))))

; EXERCISE: Change the name of this function to h<UID> where
; <UID> is your actual student ID number. Then, modify this
; function to compute an admissible heuristic value of s.
;
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the
; running time of a function call.
;

;total Manhattan distance from each box to the nearest goal
;EDIT: not enough time to implement, will use total boxes instead

(defun h504666652 (s)
    (cond ((null s) 0)
        ((listp (car s)) (+ (h1 (car s)) (h1 (cdr s))))
        ((isBox (car s)) (+ 1 (h1 (cdr s))))
        (t (h1 (cdr s)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.
 | Each problem can be visualized by calling (printstate <problem>). For example, (printstate p1).
 | Problems are roughly ordered by their difficulties.
 | For most problems, we also privide 2 additional number per problem:
 |    1) # of nodes expanded by A* using our next-states and h0 heuristic.
 |    2) the depth of the optimal solution.
 | These numbers are located at the comments of the problems. For example, the first problem below 
 | was solved by 80 nodes expansion of A* and its optimal solution depth is 7.
 | 
 | Your implementation may not result in the same number of nodes expanded, but it should probably
 | give something in the same ballpark. As for the solution depth, any admissible heuristic must 
 | make A* return an optimal solution. So, the depths of the optimal solutions provided could be used
 | for checking whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible to solve without a good heuristic!
 | 
 |#

;(80,7)
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 0 0 4 1)
	   (1 1 1 1 1 1)))

;(110,10)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 0 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(211,12)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(300,13)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 0 0)
	   (0 3 1 0 0 0 0)))

;(551,10)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 0 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(722,12)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 0 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(1738,50)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 0 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(1763,22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 0 0 4 1)
	   (1 1 1 1 1 1)))

;(1806,41)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 0 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(10082,51)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 0 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(16517,48)
(setq p11 '((1 1 1 1 1 1 1)
	    (1 4 0 0 0 4 1)
	    (1 0 2 2 1 0 1)
	    (1 0 2 0 1 3 1)
	    (1 1 2 0 1 0 1)
	    (1 4 0 0 4 0 1)
	    (1 1 1 1 1 1 1)))

;(22035,38)
(setq p12 '((0 0 0 0 1 1 1 1 1 0 0 0)
	    (1 1 1 1 1 0 0 0 1 1 1 1)
	    (1 0 0 0 2 0 0 0 0 0 0 1)
	    (1 3 0 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 2 1 1 1 0 0 0 1)
	    (1 0 0 0 0 1 0 1 4 0 4 1)
	    (1 1 1 1 1 1 0 1 1 1 1 1)))

;(26905,28)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 4 0 0 0 0 0 2 0 1)
	    (1 0 2 0 0 0 0 0 4 1)
	    (1 0 3 0 0 0 0 0 2 1)
	    (1 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 0 0 0 0 4 1)
	    (1 1 1 1 1 1 1 1 1 1)))

;(41715,53)
(setq p14 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 0 0)
	    (0 2 0 4 0 0 0)	   
	    (3 2 1 1 1 0 0)
	    (0 0 1 4 0 0 0)))

;(48695,44)
(setq p15 '((1 1 1 1 1 1 1)
	    (1 0 0 0 0 0 1)
	    (1 0 0 2 2 0 1)
	    (1 0 2 0 2 3 1)
	    (1 4 4 1 1 1 1)
	    (1 4 4 1 0 0 0)
	    (1 1 1 1 0 0 0)
	    ))

;(91344,111)
(setq p16 '((1 1 1 1 1 0 0 0)
	    (1 0 0 0 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(3301278,76)
(setq p17 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 0 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(??,25)
(setq p18 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 0 1 0 0 0 0)	    
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))
;(??,21)
(setq p19 '((0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 0 1 0 0 1 0 0 0 0)
	    (0 0 0 0 0 0 3 0 0 0 2 0)
	    (0 0 0 0 1 0 0 1 0 0 0 4)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 2 0 4 1 0 0 0)))

;(??,??)
(setq p20 '((0 0 0 1 1 1 1 0 0)
	    (1 1 1 1 0 0 1 1 0)
	    (1 0 0 0 2 0 0 1 0)
	    (1 0 0 5 5 5 0 1 0)
	    (1 0 0 4 0 4 0 1 1)
	    (1 1 0 5 0 5 0 0 1)
	    (0 1 1 5 5 5 0 0 1)
	    (0 0 1 0 2 0 1 1 1)
	    (0 0 1 0 3 0 1 0 0)
	    (0 0 1 1 1 1 1 0 0)))

;(??,??)
(setq p21 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 0 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

;(??,??)
(setq p22 '((0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 2 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 1 1 1 0 0 2 1 1 0 0 0 0 0 0 0 0 0)
	    (0 0 1 0 0 2 0 2 0 1 0 0 0 0 0 0 0 0 0)
	    (1 1 1 0 1 0 1 1 0 1 0 0 0 1 1 1 1 1 1)
	    (1 0 0 0 1 0 1 1 0 1 1 1 1 1 0 0 4 4 1)
	    (1 0 2 0 0 2 0 0 0 0 0 0 0 0 0 0 4 4 1)
	    (1 1 1 1 1 0 1 1 1 0 1 3 1 1 0 0 4 4 1)
	    (0 0 0 0 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1)
	    (0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))
	);end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)    
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn    
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun


