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
  ((horiz :accessor horiz :initarg :horiz :initform nil :type constraint-cell
          :documentation "horizontal constraint cell")
   (verti :accessor verti :initarg :verti :initform nil :type constraint-cell
          :documentation "vertical constraint cell")
   (mark :accessor mark :initform nil
         :documentation "number written in the cell"))
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

(defmethod cell-col ((c cell))
  (delete-if-not #'(lambda (x) (eql (verti c) (verti x)))
                 (blank-cells (puzzle c))))

(defmethod cell-row ((c cell))
  (delete-if-not #'(lambda (x) (eql (horiz c) (horiz x)))
                 (blank-cells (puzzle c))))

(defmethod candidates ((c cell))
  ;; TODO cleanup
  (let ((cand (make-all-candidates)))
    (when (horiz c)
      (let ((h (horiz (horiz c))))
        (when h
          (setf cand (nintersection cand
                                    (reduce #'union
                                            (ways h (length (cell-row c)))))))))
    (when (verti c)
      (let ((v (verti (verti c))))
        (when v
          (setf cand (nintersection cand
                                    (reduce #'union
                                            (ways v (length (cell-col c)))))))))
    (let ((col (cell-col c)))
      (loop for cell in col
         if (mark cell)
         do (setf cand (delete (mark cell) cand))))
    (let ((row (cell-row c)))
      (loop for cell in row
         if (mark cell)
         do (setf cand (delete (mark cell) cand))))
    cand))

(defun entry-cell (entry x y)
  (if (symbolp entry)
      (ecase (intern (symbol-name entry) (symbol-package 'kakuro))
        ((blank b) (make-instance 'blank-cell :x x :y y))
        ((wall w) (make-instance 'wall-cell :x x :y y)))
      (let ((horiz (second entry)) (verti (first entry)))
        (make-instance 'constraint-cell :x x :y y
                       :horiz horiz :verti verti))))

(defun parse-puzzle (&optional (stream *standard-input*))
  (let ((header (read stream)))
    (unless (equalp (symbol-name header) "kakuro")
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
                do (setf (svref curr-horiz-constraints x) cell
                         (svref curr-verti-constraints y) cell)
                end
                end))
        (make-instance 'puzzle :height height :width width :cells cells)))))

(defun solve-puzzle (p)
  (loop
     with blank-cells = (blank-cells p)
     while (loop for cell in blank-cells
              for c = (candidates cell)
              if (null (mark cell))
              if (= 1 (length c))
              do (setf (mark cell) (first c))
              and return t)
     finally (return p)))
