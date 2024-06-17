# Missionaries and Cannibals Problem with Iterative Deepening Depth-First Search (IDDFS)

## Overview
This code solves the Missionaries and Cannibals problem using the Iterative Deepening Depth-First Search (IDDFS) algorithm. The problem involves moving missionaries and cannibals across a river using a boat, ensuring that missionaries are never outnumbered by cannibals on either side.

## Parameters
- `*depth*`: The maximum depth for the depth-limited search.
- `*missionaries-count*`: The total number of missionaries.
- `*cannibals-count*`: The total number of cannibals.

These parameters are parsed from command-line arguments or default to 40, 15, and 15, respectively.

## Functions

### `valid-state`
Checks if the given state is valid, ensuring that missionaries are not outnumbered by cannibals on either side of the river.
```lisp
(defun valid-state (m c b)
    "Check for valid state"
    ;; Ensure missionaries are always more or equal to cannibals
    (and (or (>= m c) (eql m 0))
         (<= (+ m c) (* 2 *missionaries-count*)) ;; Total people 30
         (or (>= (- *missionaries-count* m) (- *cannibals-count* c)) (eql (- *missionaries-count* m) 0))))
```

### `get-moves`
Generates possible next states given the current state.
```lisp
(defun get-moves (m c b)
    "Generate possible next states given the current state"
    (let ((moves nil)) ;; Store moves
        (dotimes (dm 3) ;; Loop for missionaries
            (dotimes (dc 3) ;; Loop for cannibals
                (when (and (<= 1 (+ dm dc) 6) (or (> dm 0) (> dc 0))) ;; Check for valid movements
                    (multiple-value-bind (nm nc nb) ;; Calculate new moves
                        (if (eql b 1) ;; Is boat on starting side
                            (values (- m (* dm b)) (- c (* dc b)) (- 1 b)) ;; Move from start side to destination
                            (values (+ m (* dm b)) (+ c (* dc b)) (- 1 b))) ;; Move from destination side to start
                    (when (valid-state nm nc nb) ;; If new state is valid, add to list
                        (push (list nm nc nb) moves))))))
    moves))
```

### `dls`
Performs a depth-limited search from the given node to the goal state up to a specified depth.
```lisp
(defun dls (node goal depth)
    "Depth limited searching"
    (format t "Processing state at depth ~A: Missionaries: ~A, Cannibals: ~A, Boat: ~A~%"
            depth (first node) (second node) (if (eql (third node) 1) "Start" "Destination"))
    (if (equal node goal)
        (return-from dls (list node))
        (if (<= depth 0)
            (return-from dls nil)
            (let ((valid-children (remove-if (lambda (x) (member x *visited* :test 'equal))
                                            (get-moves (first node) (second node) (third node)))))
                (push node *visited*)
                (dolist (child valid-children) ;; Recursive search
                    (let ((result (dls child goal (1- depth))))
                        (when result
                            (return-from dls (cons node result)))))))))
```

### `iddfs`
Implements the Iterative Deepening Depth-First Search (IDDFS) algorithm.
```lisp
(defun iddfs (start goal max-depth)
    "ID DFS"
    (loop for depth from 0 to max-depth
        do (progn 
            (format t "Starting search at depth ~A~%" depth)
            (setq *visited* nil) ;; Reset the visited list at each level
            (let ((result (dls start goal depth)))
                (when result
                    (format t "Potential solution found at depth ~A~%" depth)
                    (return result))))))
```

### `print-solution`
Finds and prints the solution path from the initial state to the goal state.
```lisp
(defun print-solution ()
    "The solution"
    (let ((solution (iddfs (list *missionaries-count* *cannibals-count* 1) '(0 0 0) *depth*)))
    (if solution 
        (dolist (state solution) ;; Print the state in loop for result
            (format t "Missionaries: ~A, Cannibals: ~A, Boat: ~A~%"
                    (first state)
                    (second state)
                    (if (eql (third state) 1) "Start" "Destination")))
        (format t "No solution found: ~%"))))
```

## Global Variables
- `*visited*`: Keeps track of visited states during the search.

## Usage
1. Load the code into your Common Lisp environment.
2. Run the `print-solution` function to start the IDDFS algorithm.
3. The program will print the solution path and the number of nodes expanded during the search.

This implementation demonstrates the use of the IDDFS algorithm to solve the Missionaries and Cannibals problem, providing a clear path from the initial state to the goal state while exploring nodes iteratively to avoid deep recursion.
