;;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
;;;;
;;;; textures.lisp
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; o http://www.connectedpixel.com/blog/texture/marble

;;; Packages

(asdf :black-tie)
(asdf :zpng)

(in-package :black-tie)


;;; Variables

(defparameter *voronoi* (make-instance 'voronoi-2d))


;;; Macros

(defmacro rgb-texture ((&key (file "tmp.png") (width 512) (height 512)
                             (r 255) (g 255) (b 255))
                       &body body)
  `(let* ((png (make-instance 'zpng:png :color-type :truecolor
                              :width ,width :height ,height))
          (image (zpng:data-array png)))
     (with-2d (x ,width y ,height)
       (let ((red 0) (green 0) (blue 0)
             (noise 0))
         ,@body
         (setf noise (/ (+ noise 1) 2))
         (setf red   (truncate (* noise ,r)))
         (setf green (truncate (* noise ,g)))
         (setf blue  (truncate (* noise ,b)))
         (cond ((> red 255) (setf red 255))
               ((< red 0) (setf red 0)))
         (cond ((> green 255) (setf green 255))
               ((< green 0) (setf green 0)))
         (cond ((> blue 255) (setf blue 255))
               ((< blue 0) (setf blue 0)))
         (set-rgb image x y red green blue)))
     (zpng:write-png png ,file)))


(defmacro voronoi-2d-texture ((&key (file "tmp.png") (instance *voronoi*)
                                    (width 512) (height 512))
                              &body body)
  `(let* ((png (make-instance 'zpng:png :color-type :truecolor
                              :width ,width :height ,height))
          (image (zpng:data-array png)))
     (with-2d (x ,width y ,height)
       (let ((distances (distances ,instance (/ x ,width) (/ y ,height)
                                   :sorted nil))
             (rgb 0))
         ,@body
         (setf rgb (floor (* rgb 255)))
         (when (> rgb 255)
           (setf rgb 255))
         (set-rgb image x y rgb rgb rgb)))
     (zpng:write-png png ,file)))


;;; Functions

(defun set-blue (image x y value)
  (setf (aref image y x 2) value))


(defun set-green (image x y value)
  (setf (aref image y x 1) value))


(defun set-red (image x y value)
  (setf (aref image y x 0) value))


(defun set-rgb (image x y red green blue)
  (set-blue image x y blue)
  (set-green image x y green)
  (set-red image x y red))


(defun test-1d (&key (fn #'simplex-noise-1d-sf) (width 512) (height 512))
  (let* ((png (make-instance 'zpng:png :color-type :truecolor
                             :width width :height height))
         (image (zpng:data-array png)))
    (loop with ystep = 16
          for y from 0 below (/ height ystep)
          for xoffset = (* y width)
          do (loop for x from (+ 0 xoffset) below (+ width xoffset)
                   for xrgb from 0
                   for xf = (/ x 32.0)
                   for rgb = (truncate (* (+ (funcall fn xf) 1) 127.0))
                   do (loop for i from 0 below ystep
                            for yrgb = (+ (* y ystep) i)
                            do (set-rgb image xrgb yrgb rgb rgb rgb))))
      (zpng:write-png png "test-1d.png")))


(defun test-2d (&key (fn #'simplex-noise-2d-sf) (width 512) (height 512))
  (let* ((png (make-instance 'zpng:png :color-type :truecolor
                             :width width :height height))
         (image (zpng:data-array png)))
    (with-2d (x width y height)
      (let* ((xf (/ x 32.0))
             (yf (/ y 32.0))
             (rgb (truncate (* (+ (funcall fn xf yf) 1) 127.0))))
        (set-rgb image x y rgb rgb rgb)))
    (zpng:write-png png "test-2d.png")))


(defun test-3d (&key (fn #'simplex-noise-3d-sf) (width 512) (height 512))
  (let* ((png (make-instance 'zpng:png :color-type :truecolor
                             :width width :height height))
         (image (zpng:data-array png)))
    (with-2d (x width y height)
      (let* ((xf (/ x 32.0))
             (yf (/ y 32.0))
             (rgb (truncate (* (+ (funcall fn xf yf 0.0) 1) 127.0))))
        (set-rgb image x y rgb rgb rgb)))
    (zpng:write-png png "test-3d.png")))


;;; Textures

(defun texture-canvas ()
  (rgb-texture (:file "canvas.png" :r 180 :g 160 :b 140)
    (setf noise (* (simplex-noise-2d-sf (/ x 256.0) (/ y 256.0)) 0.2))
    ;; streaks
    (incf noise (* (simplex-noise-2d-sf (/ x 2.0) (/ y 500.0)) 0.4))
    (incf noise (* (simplex-noise-2d-sf (/ x 500.0) (/ y 2.0)) 0.4))
    (setf noise (/ (+ noise 1.5) 2))))


(defun texture-grass (&key (width 512) (height 512))
  (let* ((png (make-instance 'zpng:png :color-type :truecolor
                             :width width :height height))
         (image (zpng:data-array png)))
    (with-2d (x width y height)
      (let ((red 0) (green 0) (blue 0) (noise1 0) (noise2 0))
        (setf noise1 (* (perlin-noise-sf (* x 10.1) (* y 10.1) 0.0) 0.6))
        (setf noise2 (* (perlin-noise-sf (/ x 128.0) (/ y 128.0) -0.1) 0.3))
        (incf noise2 (* (perlin-noise-sf (/ x 256.0) (/ y 256.0) 0.1) 0.1))
        (setf noise1 (/ (+ noise1 1) 2))
        (setf noise2 (/ (+ noise2 1) 2))
        (setf red   (truncate (+ (* noise1  40) (* noise2  90))))
        (setf green (truncate (+ (* noise1 100) (* noise2  50))))
        (setf blue  (truncate (+ (* noise1  40) (* noise2  40))))
        (cond ((> red 255) (setf red 255))
              ((< red 0) (setf red 0)))
        (cond ((> green 255) (setf green 255))
              ((< green 0) (setf green 0)))
        (cond ((> blue 255) (setf blue 255))
              ((< blue 0) (setf blue 0)))
        (set-rgb image x y red green blue)))
    (zpng:write-png png "grass.png")))


(defun texture-goo ()
  (rgb-texture (:file "goo.png" :r 80 :g 180 :b 80)
    (setf noise (* (simplex-noise-2d-sf (/ x 250.0) (/ y 250.0)) 0.5))
    (incf noise (* (simplex-noise-2d-sf (/ x 100.0) (/ y 100.0)) 0.25))
    (incf noise (* (simplex-noise-2d-sf (/ x 50.0) (/ y 50.0)) 0.125))
    (incf noise (* (simplex-noise-2d-sf (/ x 1.0) (/ y 1.0)) 0.0625))
    (setf noise (abs noise))))


(defun texture-wood-1 ()
  (rgb-texture (:file "wood-1.png" :r 180 :g 130 :b 80)
    (setf noise (* (simplex-noise-2d-sf (/ x 750.0) (/ y 750.0)) 15))
    (setf noise (mod noise 1))))


(defun texture-wood-2 ()
  (rgb-texture (:file "wood-2.png" :r 180 :g 130 :b 80)
    (setf noise (* (simplex-noise-2d-sf (/ x 500.0) (/ y 750.0)) 15))
    (setf noise (mod noise 1))
    (incf noise (* (simplex-noise-2d-sf (/ x 1.0) (/ y 1.0)) 0.5))
    (incf noise (* (simplex-noise-2d-sf (/ x 500.0) (/ y 2.0)) 0.7))))


(defun voronoi-2d ()
  (voronoi-2d-texture (:file "voronoi-2d.png")
    (setf distances (sort distances #'<))
    (setf rgb (* (first distances) 4))))


(defun voronoi-2d-second ()
  (voronoi-2d-texture (:file "voronoi-2d-second.png")
    (setf distances (sort distances #'<))
    (setf rgb (* (second distances) 4))))


(defun voronoi-2d-sum ()
  (voronoi-2d-texture (:file "voronoi-2d-sum.png")
    (setf distances (sort distances #'<))
    (setf rgb (* (reduce #'+ (subseq distances 0 2)) 4))))


;;; Run all the functions.

(let ((fns '(test-1d test-2d test-3d
             texture-canvas texture-grass texture-goo
             texture-wood-1 texture-wood-2
             voronoi-2d voronoi-2d-second voronoi-2d-sum)))
  (loop with nr-of-fns = (length fns)
        for fn in fns
        for i from 1
        do (format t "[~A/~A] Running ~A...~%" i nr-of-fns fn)
           (funcall fn)))

(quit)
