(in-package :cl-user)

(defpackage :kakuro
  (:documentation "Solver for kakuro puzzles")
  (:use :cl)
  (:export cell blank-cell constraint-cell wall-cell puzzle
           parse-puzzle))

(in-package :kakuro)

(defconstant +max-digit+ 9)

(defun ways (sum len)
  "Calculate the ways to sum up to `sum` using `len` terms"
  (defun ways-aux (sum len max-term)
    (cond
      ((minusp sum) '())
      ((and (zerop sum) (= len 0)) (list '()))
      ((zerop sum) '())
      (t
       (loop for term from 1 upto (min sum max-term)
          nconc (mapcar (lambda (way) (cons term way))
                        (ways-aux (- sum term) (1- len) (1- term)))))))
  (mapcar #'nreverse
          (ways-aux sum len +max-digit+)))

(defun make-all-candidates ()
  "A list of all candidate entries"
  (loop for i from 1 upto +max-digit+ collect i))

(defclass cell ()
  ((x :accessor x :initarg :x :type (integer 0 *)
      :documentation "x-coordinate of the cell, origin top-left, going down")
   (y :accessor y :initarg :y :type (integer 0 *)
      :documentation "y-coordinate of the cell, origin top-left, going right")
   (puzzle :accessor puzzle :initarg :puzzle :type puzzle
           :documentation "puzzle to which this cell belongs")))

(defclass constraint-cell (cell)
  ((horiz :accessor horiz :initarg :horiz :initform nil
          :documentation "horizontal sum constraint")
   (verti :accessor verti :initarg :verti :initform nil
          :documentation "vertical sum constraint"))
  (:documentation "cell representing a constraint on the sum of other cells"))

(defclass blank-cell (cell)
  ((horiz :accessor horiz :initarg :horiz :initform nil :type constraint-cell)
   (verti :accessor verti :initarg :verti :initform nil :type constraint-cell)
   (candidates :initform (make-all-candidates) :type list))
  (:documentation "cell representing a player-fillable entry with constraints"))

(defclass wall-cell (cell)
  ()
  (:documentation "cell representing a wall with no constraint or entry"))

(defclass puzzle ()
  ((height :initarg :height :type (integer 1 *)
           :documentation "number of rows")
   (width :initarg :width :type (integer 1 *)
          :documentation "number of columns")
   (cells :initarg :cells :type (array array)
          :documentation "cells in the puzzle")))

(defmethod initialize-instance :after ((p puzzle) &key)
  (with-slots (height width cells) p
    (loop for x below height do
         (loop for y below width
            do (setf (puzzle (aref cells x y)) p)))))

(defmethod puzzle-cell ((p puzzle) x y)
  "get the cell object at coordinates (x, y)"
  (aref (slot-value p 'cells) x y))

(defmethod blank-cells ((p puzzle))
  (with-slots (height width cells) p
    (loop for i below height nconc
         (loop for j below width
            for cell = (aref cells i j)
            if (typep cell 'blank-cell)
            collect cell))))

(defun entry-cell (entry x y)
  (if (symbolp entry)
      (ecase entry
        ((blank b) (make-instance 'blank-cell :x x :y y))
        ((wall w) (make-instance 'wall-cell :x x :y y)))
      (let ((horiz (first entry)) (verti (second entry)))
        (make-instance 'constraint-cell :x x :y y
                       :horiz horiz :verti verti))))

(defun parse-puzzle (&optional (stream *standard-input*))
  (let ((*package* (symbol-package 'kakuro))) ; read into kakuro package
    (let ((header (read stream)))
      (unless (eql header 'kakuro)
        (error (format nil "not a kakuro puzzle: ~a" header)))
      (let* ((height (read stream)) (width (read stream)))
        (declare ((integer 1 *) height width))
        (let ((curr-horiz-constraints (make-array height))
              (curr-verti-constraints (make-array width))
              (cells (make-array (list height width))))
          (loop for x below height do
               (loop for y below width
                  for cell = (entry-cell (read stream) x y)
                  do (setf (aref cells x y) cell)
                  if (typep cell 'blank-cell)
                  do (setf (horiz cell) (svref curr-horiz-constraints x)
                           (verti cell) (svref curr-verti-constraints y))
                  else
                  if (typep cell 'wall-cell)
                  do (setf (svref curr-horiz-constraints x) nil
                           (svref curr-verti-constraints y) nil)
                  else
                  if (typep cell 'constraint-cell)
                  do (setf (svref curr-horiz-constraints x) (horiz cell)
                           (svref curr-verti-constraints y) (verti cell))
                  end
                  end))
          (make-instance 'puzzle :height height :width width :cells cells))))))
