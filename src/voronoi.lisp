;;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
;;;;
;;;; voronoi.lisp
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in the root directory for more info.
;;;;
;;;; This is a suboptimal implementation with hopeless documentation.

(in-package :black-tie)


;;; Classes

(defclass voronoi-2d ()
  ((1/p :initform nil :reader 1/p)
   (number-of-points :initarg :number-of-points :initform 64
                     :reader number-of-points)
   (points :initform nil :reader points)
   (psqrt :initform nil :reader psqrt)))


;;; Methods

(defmethod initialize-instance :after ((v2d voronoi-2d) &key)
  (let* ((psqrt (floor (sqrt (number-of-points v2d))))
         (1/p (/ 1.0 psqrt)))
    (setf (slot-value v2d '1/p) 1/p)
    (setf (slot-value v2d 'points) (make-array (list psqrt psqrt)))
    (setf (slot-value v2d 'psqrt) psqrt)
    (with-2d (x psqrt y psqrt)
      (setf (aref (slot-value v2d 'points) x y)
            (cons (+ (random 1/p) (* x 1/p))
                  (+ (random 1/p) (* y 1/p)))))))


(defmethod distances ((v2d voronoi-2d) x y &key (sorted t))
  "X and Y need be a float between 0 and 1.  This function will return a list
  of the distances to the nearest points for X and Y."
  (let ((distances nil)
        ;; determine which cell the current x,y is in
        (px (floor (/ x (1/p v2d))))
        (py (floor (/ y (1/p v2d)))))
    ;; check the surrounding cells
    (with-2d (u 2 v 2 :x-min -1 :y-min -1)
      (unless (or (< (+ px u) 0) (>= (+ px u) (psqrt v2d))
                  (< (+ py v) 0) (>= (+ py v) (psqrt v2d)))
        (push (vlength
                (vector (- x (car (aref (points v2d) (+ px u) (+ py v))))
                        (- y (cdr (aref (points v2d) (+ px u) (+ py v))))))
              distances)))
    (if sorted
        (sort distances #'<)
        distances)))
