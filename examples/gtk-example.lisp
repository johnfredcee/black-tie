;;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
;;;;
;;;; gtk-example.lisp
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)

;;; Packages

(unless (find-package :asdf)
  (require :asdf))

(asdf:oos 'asdf:load-op :cl-gtk2-gtk)
(asdf:oos 'asdf:load-op :black-tie)

;; for development
;(asdf:oos 'asdf:load-op :swank)  ; already in my dev core
;(swank:create-server)


;;; Parameters & Variables

(defstruct gtk-ui
  (builder nil)
  (drawingarea1-graphics-context nil)
  (drawingarea1-pixbuf nil)
  (drawingarea1-window nil))

(defparameter *gtk-ui* nil)

(gtk:within-main-loop-and-wait
  (setf *gtk-ui* (make-gtk-ui :builder (make-instance 'gtk:builder))))


;;; Foreign Functions

(in-package :gtk)

(defun widget-rectangle (widget)
  (convert-from-foreign (foreign-slot-pointer (gobject:pointer widget)
                                              'gtk::%gtk-widget :allocation)
                        '(gobject:g-boxed-foreign rectangle)))

(in-package :cl-user)


;;; Utility Functions

(defun srp (system &optional (relative-path ""))
  (namestring (asdf:system-relative-pathname system relative-path)))


;;; GTK Functions

(defun get-widget (name)
  ;; XXX: Why doesn't it work with gtk:wml?
  ;(gtk:within-main-loop
    (gtk:builder-get-object (gtk-ui-builder *gtk-ui*) name));)


(defun drawingarea1-redraw (w e)
  (declare (ignore e w))
  (gtk:within-main-loop
    (let ((gc (gtk-ui-drawingarea1-graphics-context *gtk-ui*))
          (pixbuf (gtk-ui-drawingarea1-pixbuf *gtk-ui*))
          (window (gtk-ui-drawingarea1-window *gtk-ui*)))
    (when pixbuf
      (gdk:pixbuf-render-to-drawable pixbuf window gc 0 0 0 0
                                     (gdk:pixbuf-width pixbuf)
                                     (gdk:pixbuf-height pixbuf)
                                     :none 0 0)))))


(defun noisificate (&optional w e)
  (declare (ignore w e))
  (gtk:within-main-loop
    (let* ((widget (get-widget "drawingarea1"))
           (width (gdk:rectangle-width (gtk::widget-rectangle widget)))
           (height (gdk:rectangle-height (gtk::widget-rectangle widget)))
           (window (gtk:widget-window widget))
           (pixbuf (gdk:pixbuf-get-from-drawable nil window :width width
                                                 :height height))
           ;(bits (gdk:pixbuf-bits-per-sample pixbuf))
           (channels (gdk:pixbuf-n-channels pixbuf))
           (pixels (gdk:pixbuf-pixels pixbuf))
           (gc (gdk:graphics-context-new window))
           ;; settings from widgets
           (red (gtk:spin-button-value (get-widget "red")))
           (green (gtk:spin-button-value (get-widget "green")))
           (blue (gtk:spin-button-value (get-widget "blue")))
           (sbx (floor (gtk:spin-button-value (get-widget "x"))))
           (sby (floor (gtk:spin-button-value (get-widget "y"))))
           (quality (floor (gtk:spin-button-value (get-widget "quality"))))
           (zoom (gtk:spin-button-value (get-widget "zoom"))))
      (setf (gtk-ui-drawingarea1-graphics-context *gtk-ui*) gc)
      (setf (gtk-ui-drawingarea1-pixbuf *gtk-ui*) pixbuf)
      (setf (gtk-ui-drawingarea1-window *gtk-ui*) window)
      (loop for y from 0 below height by quality
            for ypn = (coerce (/ (+ y sby) height zoom) 'single-float)
            do (loop for x from 0 below width by quality
                     for offset = (+ (* y width channels) (* x channels))
                     for xpn = (coerce (/ (+ x sbx) width zoom) 'single-float)
                     for c = (/ (+ (black-tie:perlin-noise-sf xpn ypn 0.0) 1)
                                2)
                     for r = (floor (* c red))
                     for g = (floor (* c green))
                     for b = (floor (* c blue))
                     do (setf (cffi:mem-aref pixels :uint8    offset   ) r)
                        (setf (cffi:mem-aref pixels :uint8 (+ offset 1)) g)
                        (setf (cffi:mem-aref pixels :uint8 (+ offset 2)) b)
                        (when (> quality 0)
                          (loop for ys from 0 below quality
                                do (loop for xs from 0 below quality
                                         for so = (+ offset
                                                     (* ys width channels)
                                                     (* xs channels))
                                         do (unless (and (= xs 0) (= ys 0))
                          (setf (cffi:mem-aref pixels :uint8    so   ) r)
                          (setf (cffi:mem-aref pixels :uint8 (+ so 1)) g)
                          (setf (cffi:mem-aref pixels :uint8 (+ so 2)) b)))))))
      (gdk:pixbuf-render-to-drawable pixbuf window gc 0 0 0 0 width height
                                     :none 0 0))))


(defun run-app ()
  (gtk:within-main-loop-and-wait
    (gtk:builder-add-from-file (gtk-ui-builder *gtk-ui*)
                               (srp :black-tie "examples/gtk-example.glade"))
    (gtk:builder-connect-signals-simple (gtk-ui-builder *gtk-ui*)
                      `(("on_drawingarea1_expose_event" ,#'drawingarea1-redraw)
                        ("on_regenerate_clicked" ,#'noisificate)))
    ;; setting these in Glade doesn't work :-(
    (setf (gtk:spin-button-value (get-widget "red")) 255)
    (setf (gtk:spin-button-value (get-widget "green")) 255)
    (setf (gtk:spin-button-value (get-widget "blue")) 255)
    (setf (gtk:spin-button-value (get-widget "quality")) 4)
    (setf (gtk:spin-button-value (get-widget "zoom")) 0.2)
    (gtk:widget-show (get-widget "top_level") :all t))
  (noisificate))
