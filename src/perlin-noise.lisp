;;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
;;;;
;;;; perlin-noise.lisp
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in the root directory for more info.
;;;;
;;;; From: http://mrl.nyu.edu/~perlin/noise/
;;;;
;;;; There's another implementation here: http://www.koders.com/lisp/fid49FCA421DBCE60AFE806BBCD84887C14ECD487D5.aspx
;;;;
;;;; It looks more extensive, but I just wanted to write my own to understand
;;;; it better.
;;;;
;;;; Also check this out some time in the future: http://www.gamedev.net/community/forums/mod/journal/journal.asp?jn=263350&reply_id=2889484

(in-package :black-tie)


;;;# Reference Implementation (don't change it unless it's incorrect)

;; f(t) = 6t^5 - 15t^4 + 10t3
(defun fade-ref (v)
  (* v v v (+ (* v (- (* v 6) 15)) 10)))


(defun grad-ref (hash x y z)
  (let* ((h (logand hash 15))
         (u (if (< h 8) x y))
         (v (if (< h 4) y (if (or (= h 12) (= h 14)) x z))))
    (+ (if (= (logand h 1) 0) u (- u))
       (if (= (logand h 2) 0) v (- v)))))


(defun lerp-ref (v a b)
  (+ a (* v (- b a))))


(defun perlin-noise-reference (x y z)
  "X, Y and Z should be floats for the best results."
  (let* ((xc (logand (floor x) 255))
         (yc (logand (floor y) 255))
         (zc (logand (floor z) 255))
         (x (- x (floor x)))  ; (x (mod x 1)) is faster in SBCL
         (y (- y (floor y)))  ; (y (mod y 1)) is faster in SBCL
         (z (- z (floor z)))  ; (z (mod z 1)) is faster in SBCL
         (u (fade-ref x))
         (v (fade-ref y))
         (w (fade-ref z))
         (a (+ (svref +pnp+ xc) yc))
         (aa (+ (svref +pnp+ a) zc))
         (ab (+ (svref +pnp+ (+ a 1)) zc))
         (b (+ (svref +pnp+ (+ xc 1)) yc))
         (ba (+ (svref +pnp+ b) zc))
         (bb (+ (svref +pnp+ (+ b 1)) zc)))
    (lerp-ref w (lerp-ref v (lerp-ref u (grad-ref aa x y z)
                                        (grad-ref ba (- x 1) y z))
                            (lerp-ref u (grad-ref ab x (- y 1) z)
                                        (grad-ref bb (- x 1) (- y 1) z)))
                (lerp-ref v (lerp-ref u (grad-ref (+ aa 1) x y (- z 1))
                                        (grad-ref (+ ba 1) (- x 1) y (- z 1)))
                            (lerp-ref u (grad-ref (+ ab 1) x (- y 1) (- z 1))
                                        (grad-ref (+ bb 1) (- x 1) (- y 1)
                                                  (- z 1)))))))

(defalias #'perlin-noise-reference 'perlin-noise-ref)


;;;# Standard (float) Version

(defun fade (v)
  (declare (inline * + -)
           (optimize (compilation-speed 0) (debug 0) (safety 0) (space 0)
                     (speed 3))
           #+nil(type float v))  ; makes things worse in SBCL
  (* v v v (+ (* v (- (* v 6) 15)) 10)))


(defun grad (hash x y z)
  (declare (inline * + - < = logand)
           (optimize (compilation-speed 0) (debug 0) (safety 0) (space 0)
                     (speed 3))
           (type fixnum hash)
           #+nil(type float x y z))  ; makes things worse in SBCL
  (let* ((h (logand hash 15))
         (u (if (< h 8) x y))
         (v (if (< h 4) y (if (or (= h 12) (= h 14)) x z))))
    (+ (if (= (logand h 1) 0) u (- u))
       (if (= (logand h 2) 0) v (- v)))))


(defun lerp (v a b)
  (declare (inline * + -)
           (optimize (compilation-speed 0) (debug 0) (safety 0) (space 0)
                     (speed 3))
           #+nil(type float v a b))  ; makes things worse in SBCL
  (+ a (* v (- b a))))


(defun perlin-noise (x y z)
  ;"X, Y and Z need to be FLOATS."
  (declare (inline + - fade floor grad lerp logand mod svref)
           (optimize (compilation-speed 0) (debug 0) (safety 0) (space 0)
                     (speed 3))
           #+nil(type float x y z))  ; makes things worse in SBCL
  (let* ((xc (logand (the fixnum (floor x)) 255))
         (yc (logand (the fixnum (floor y)) 255))
         (zc (logand (the fixnum (floor z)) 255))
         (x (mod x 1))
         (y (mod y 1))
         (z (mod z 1))
         (u (fade x))
         (v (fade y))
         (w (fade z))
         (a (+ (the fixnum (svref +pnp+ xc)) yc))
         (aa (+ (the fixnum (svref +pnp+ a)) zc))
         (ab (+ (the fixnum (svref +pnp+ (+ a 1))) zc))
         (b (+ (the fixnum (svref +pnp+ (+ xc 1))) yc))
         (ba (+ (the fixnum (svref +pnp+ b )) zc))
         (bb (+ (the fixnum (svref +pnp+ (+ b 1))) zc)))
    (lerp w (lerp v (lerp u (grad aa x y z)
                            (grad ba (- x 1) y z))
                    (lerp u (grad ab x (- y 1) z)
                            (grad bb (- x 1) (- y 1) z)))
            (lerp v (lerp u (grad (+ aa 1) x y (- z 1))
                            (grad (+ ba 1) (- x 1) y (- z 1)))
                    (lerp u (grad (+ ab 1) x (- y 1) (- z 1))
                            (grad (+ bb 1) (- x 1) (- y 1) (- z 1)))))))


;;;# Single-Float Version

(defun fade-sf (v)
  (declare (inline * + -)
           (optimize (compilation-speed 0) (debug 0) (safety 0) (space 0)
                     (speed 3))
           (type single-float v))
  (* v v v (+ (* v (- (* v 6) 15)) 10)))


(defun grad-sf (hash x y z)
  (declare (inline * + - < = logand)
           (optimize (compilation-speed 0) (debug 0) (safety 0) (space 0)
                     (speed 3))
           (type fixnum hash)
           (type single-float x y z))
  (let* ((h (logand hash 15))
         (u (if (< h 8) x y))
         (v (if (< h 4) y (if (or (= h 12) (= h 14)) x z))))
    (+ (if (= (logand h 1) 0) u (- u))
       (if (= (logand h 2) 0) v (- v)))))


(defun lerp-sf (v a b)
  (declare (inline * + -)
           (optimize (compilation-speed 0) (debug 0) (safety 0) (space 0)
                     (speed 3))
           (type single-float v a b))
  (+ a (* v (- b a))))


(defun perlin-noise-single-float (x y z)
  "X, Y and Z need to be SINGLE-FLOATS."
  (declare (inline + - fade-sf floor grad-sf lerp-sf logand mod svref)
           (optimize (compilation-speed 0) (debug 0) (safety 0) (space 0)
                     (speed 3))
           (type single-float x y z))
  (let* ((xc (logand (the fixnum (floor x)) 255))
         (yc (logand (the fixnum (floor y)) 255))
         (zc (logand (the fixnum (floor z)) 255))
         (x (mod x 1))
         (y (mod y 1))
         (z (mod z 1))
         (u (fade-sf x))
         (v (fade-sf y))
         (w (fade-sf z))
         (a (+ (the fixnum (svref +pnp+ xc)) yc))
         (aa (+ (the fixnum (svref +pnp+ a)) zc))
         (ab (+ (the fixnum (svref +pnp+ (+ a 1))) zc))
         (b (+ (the fixnum (svref +pnp+ (+ xc 1))) yc))
         (ba (+ (the fixnum (svref +pnp+ b)) zc))
         (bb (+ (the fixnum (svref +pnp+ (+ b 1))) zc)))
    (lerp-sf w
      (lerp-sf v (lerp-sf u (grad-sf aa x y z)
                            (grad-sf ba (- x 1) y z))
                 (lerp-sf u (grad-sf ab x (- y 1) z)
                            (grad-sf bb (- x 1) (- y 1) z)))
      (lerp-sf v (lerp-sf u (grad-sf (+ aa 1) x y (- z 1))
                            (grad-sf (+ ba 1) (- x 1) y (- z 1)))
                 (lerp-sf u (grad-sf (+ ab 1) x (- y 1) (- z 1))
                            (grad-sf (+ bb 1) (- x 1) (- y 1) (- z 1)))))))

(defalias #'perlin-noise-single-float 'perlin-noise-sf)


;;;# Precalculated Version

;; Needs better documentation since its usage is a little peculiar.
(defun perlin-noise-closure (&optional (precision 128))
  "Returns a closure which can be called just like PERLIN-NOISE-SINGLE-FLOAT.
  PRECISION will be used to determine the size of the precalculated perlin
  cube.  The higher the precision to more elements will be in the cube and
  the more precise it will be (and the more memory it will consume).
  The number of elements in the cube is determined as PRECISION^3.
  NOTE: This does not give the same results for the same inputs as
        PERLIN-NOISE!"
  (let ((cube (make-array (list precision precision precision)
                          :element-type 'single-float))
        (precision (coerce precision 'single-float)))
    (with-3d (x (truncate precision)
              y (truncate precision)
              z (truncate precision))
      (setf (aref cube x y z)
            (perlin-noise (coerce (- (/ x (/ precision 2)) 1) 'single-float)
                          (coerce (- (/ y (/ precision 2)) 1) 'single-float)
                          (coerce (- (/ z (/ precision 2)) 1) 'single-float))))
    (lambda (x y z)
      (declare (inline * aref mod truncate)
               (optimize (compilation-speed 0) (debug 0) (safety 0) (space 0)
                         (speed 3))
               (type single-float precision x y z))
      (aref cube (the fixnum (truncate (* (mod x 1) precision)))
                 (the fixnum (truncate (* (mod y 1) precision)))
                 (the fixnum (truncate (* (mod z 1) precision)))))))


;;;# Naive Benchmark

(defun perlin-noise-benchmark (&key (fn #'perlin-noise-reference)
                                    (iterations 2500000))
  (time (loop repeat iterations
              for x from 0.0 by 0.1
              for y from 0.0 by 0.01
              for z from 0.0 by 0.001
              do (funcall fn x y z))))


(defun perlin-noise-compare (fn1 &key (fn2 #'perlin-noise-reference)
                                      (lines 20) (sleep-time 0.0))
  "Compares the output of FN1 to the output of FN2."
  (format t " ~S <=> ~S~%------------------+-----------------~%"
          fn1 fn2)
  (loop for xyz from -3.0 to 3.0 by (/ 6.1 lines)
        for i from 0
        do (format t "~16@S <=> ~16S~%"
                   (funcall fn1 xyz xyz xyz)
                   (funcall fn2 xyz xyz xyz))
           (sleep sleep-time)))
