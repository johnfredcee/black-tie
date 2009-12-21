;;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
;;;;
;;;; package.lisp
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in the root directory for more info.

(in-package :cl-user)

(defpackage :black-tie
  (:use :cl)
  (:export :perlin-noise
           :perlin-noise-reference :perlin-noise-ref
           :perlin-noise-single-float :perlin-noise-sf
           :simplex-noise-1d
           :simplex-noise-1d-reference :simplex-noise-1d-ref
           :simplex-noise-1d-single-float :simplex-noise-1d-sf
           :simplex-noise-2d
           :simplex-noise-2d-reference :simplex-noise-2d-ref
           :simplex-noise-2d-single-float :simplex-noise-2d-sf
           :simplex-noise-3d
           :simplex-noise-3d-reference :simplex-noise-3d-ref
           :simplex-noise-3d-single-float :simplex-noise-3d-sf
           :bands3d :fbm2d :fbm3d :ridge3d :turbulence3d))
