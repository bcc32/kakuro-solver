(defpackage :kakuro
  (:documentation "Solver for kakuro puzzles")
  (:use :cl)
  (:export :cell :blank-cell :constraint-cell :wall-cell :puzzle
           :read-puzzle :clear-puzzle :solve-puzzle :main))

(in-package :kakuro)

(defconstant +max-digit+ 9)
(defconstant +usage-notes+
  "usage: kakuro [< input_file] [> output_file]")

(defclass cell ()
  ((x :accessor x :initarg :x :type (integer 0 *)
      :documentation "x-coordinate of the cell, origin top-left, going down")
   (y :accessor y :initarg :y :type (integer 0 *)
      :documentation "y-coordinate of the cell, origin top-left, going right")
   (puzzle :accessor puzzle-of :initarg :puzzle :type puzzle
           :documentation "puzzle to which this cell belongs")))

(defclass constraint-cell (cell)
  ((horizontal-constraint :accessor horizontal-constraint :initarg :horizontal-constraint :initform nil
          :documentation "required sum of the cells in the same row as this constraint cell")
   (vertical-constraint :accessor vertical-constraint :initarg :vertical-constraint :initform nil
          :documentation "required sum of the cells in the same column as this constraint cell"))
  (:documentation "cell representing a constraint on the sum of other cells"))

(defmethod print-object ((c constraint-cell) stream)
  ;; constraint on column is displayed left of constraint on row
  (format stream "(~a ~a)" (vertical-constraint c) (horizontal-constraint c)))

(defclass blank-cell (cell)
  ((constraint-row :accessor constraint-row :initarg :constraint-row :initform nil :type constraint-cell
          :documentation "the constraint cell in whose row this cell belongs")
   (constraint-col :accessor constraint-col :initarg :constraint-col :initform nil :type constraint-cell
          :documentation "the constraint cell in whose column this cell belongs")
   (mark :accessor mark :initarg :mark :initform nil
         :documentation "number written in the cell"))
  (:documentation "cell representing a player-fillable entry with constraints"))

(defmethod print-object ((b blank-cell) stream)
  (format stream "~a" (or (mark b) "b")))

(defclass wall-cell (cell)
  ()
  (:documentation "cell representing a wall with no constraint or entry"))

(defmethod print-object ((w wall-cell) stream)
  (format stream "w"))

(defclass puzzle ()
  ((height :accessor height :initarg :height :type (integer 1 *)
           :documentation "number of rows")
   (width :accessor width :initarg :width :type (integer 1 *)
          :documentation "number of columns")
   (cells :initarg :cells :type (array array)
          :documentation "cells in the puzzle")))

(defmethod initialize-instance :after ((p puzzle) &key)
  (with-slots (height width cells) p
    (loop for x below height do
         (loop for y below width
            do (setf (puzzle-of (aref cells x y)) p)))))

(defun puzzle-cell (p x y)
  "get the cell object in `p` at coordinates (`x`, `y`)"
  (declare (puzzle p))
  (aref (slot-value p 'cells) x y))

(defmethod print-object ((p puzzle) stream)
  (format stream "kakuro ~d ~d~%" (height p) (width p))
  (dotimes (i (height p))
    (dotimes (j (width p))
      (format stream "~10a" (puzzle-cell p i j)))
    (terpri stream)))

(defun blank-cells (p)
  "get the blank cells in `p`"
  (declare (puzzle p))
  (with-slots (height width cells) p
    (loop for i below height nconc
         (loop for j below width
            for cell = (aref cells i j)
            if (typep cell 'blank-cell)
            collect cell))))

(defun cell-col (cell)
  "list the cells with the same contiguous column as `cell`"
  (declare (cell cell))
  (delete-if-not #'(lambda (other-cell) (eql (constraint-col cell) (constraint-col other-cell)))
                 (blank-cells (puzzle-of cell))))

(defun cell-row (cell)
  "list the cells with the same contiguous row as `cell`"
  (declare (cell cell))
  (delete-if-not #'(lambda (other-cell) (eql (constraint-row cell) (constraint-row other-cell)))
                 (blank-cells (puzzle-of cell))))

(defun ways (sum len &optional other-cells)
  "calculate the ways to sum up to `sum` using `len` terms, optionally with already-marked cells"
  (defun ways-aux (sum len max-term)
    (cond
      ((minusp sum) '())
      ((and (zerop sum) (= len 0)) (list '()))
      ((zerop sum) '())
      (t
       (loop for term from 1 upto (min sum max-term)
          nconc (mapcar (lambda (way) (cons term way))
                        (ways-aux (- sum term) (1- len) (1- term)))))))
  (loop
     with ways = (mapcar #'nreverse (ways-aux sum len +max-digit+))
     for cell in other-cells
     for mark = (mark cell)
     if mark
     do (setf ways (delete-if-not (lambda (way) (find mark way)) ways))
     finally (return ways)))

(defun make-all-candidates ()
  "list all candidate entries"
  (loop for i from 1 upto +max-digit+ collect i))

(defun candidates (cell)
  "find all the numbers that `cell` could possibly hold"
  (declare (cell cell))
  ;; TODO cleanup
  (let ((cand (make-all-candidates)))
    ;; figure out possible sets of numbers for the respective row/column sums
    (when (and (constraint-row cell) (horizontal-constraint (constraint-row cell)))
      (let ((row-sum (horizontal-constraint (constraint-row cell)))
            (row (cell-row cell)))
        (setf cand (nintersection cand
                                  (reduce #'union
                                          (ways row-sum (length row) row))))))
    (when (and (constraint-col cell) (vertical-constraint (constraint-col cell)))
      (let ((col-sum (vertical-constraint (constraint-col cell)))
            (col (cell-col cell)))
        (setf cand (nintersection cand
                                  (reduce #'union
                                          (ways col-sum (length col) col))))))
    ;; remove any candidates which are already in the row/column
    (let ((col (cell-col cell)))
      (loop for cell in col
         if (mark cell)
         do (setf cand (delete (mark cell) cand))))
    (let ((row (cell-row cell)))
      (loop for cell in row
         if (mark cell)
         do (setf cand (delete (mark cell) cand))))
    cand))

(defun entry-cell (entry x y)
  "convert an object read from a stream into a cell"
  (etypecase entry
    (symbol
     (ecase (intern (symbol-name entry) (symbol-package 'kakuro))
       ((blank b) (make-instance 'blank-cell :x x :y y))
       ((wall w) (make-instance 'wall-cell :x x :y y))))
    (cons
     (let ((horiz (second entry)) (verti (first entry)))
       (make-instance 'constraint-cell :x x :y y
                      :horiz horiz :verti verti)))
    (number
     (make-instance 'blank-cell :x x :y y :mark entry))))

(defun read-puzzle (&optional (stream *standard-input*))
  "reads a puzzle object from `stream`"
  (let ((header (read stream)))
    (unless (equalp (symbol-name header) "kakuro")
      (error (format nil "not a kakuro puzzle: ~a" header)))
    (let* ((height (read stream)) (width (read stream)))
      (declare ((integer 1 *) height width))
      (let ((curr-horiz-constraints (make-array height :initial-element nil))
            (curr-verti-constraints (make-array width :initial-element nil))
            (cells (make-array (list height width))))
        (loop for x below height do
             (loop for y below width
                for cell = (entry-cell (read stream) x y)
                do (setf (aref cells x y) cell)
                if (typep cell 'blank-cell)
                do (setf (constraint-row cell) (svref curr-horiz-constraints x)
                         (constraint-col cell) (svref curr-verti-constraints y))
                else
                if (typep cell 'wall-cell)
                do (setf (svref curr-horiz-constraints x) nil
                         (svref curr-verti-constraints y) nil)
                else
                if (typep cell 'constraint-cell)
                do (setf (svref curr-horiz-constraints x) cell
                         (svref curr-verti-constraints y) cell)
                end
                end))
        (make-instance 'puzzle :height height :width width :cells cells)))))

(defun clear-puzzle (p)
  "unmark all of the blank cells in `p` and return it"
  (dolist (c (blank-cells p))
    (setf (mark c) nil))
  p)

(defun solve-puzzle (p)
  "solve `p` and return it"
  (declare (puzzle p))
  (loop
     with blank-cells = (blank-cells p)
     while (loop for cell in blank-cells
              for c = (candidates cell)
              if (null (mark cell))
              if (= 1 (length c))
              do (setf (mark cell) (first c))
              and return t)
     finally (return p)))

(defun main ()
  (handler-case (prin1 (solve-puzzle (read-puzzle)))
    (warning (con)
      (print con)
      (print +usage-notes+))))
