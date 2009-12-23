;;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
;;;;
;;;; black-tie.asd
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in the Okra root directory for more info.

(in-package :cl-user)

(asdf:defsystem :black-tie
  :version "0.1a"
  :components ((:module src
                :serial t
                :components ((:file "package")
                             (:file "config")
                             (:file "macros")
                             (:file "common")
                             (:file "perlin-noise")
                             (:file "simplex-noise")
                             (:file "voronoi")
                             (:file "noise-functions")))))
