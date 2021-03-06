;;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
;;;;
;;;; common.lisp
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in the Black Tie root directory for more info.

(in-package :black-tie)


;;; Functions

(defun append1 (lst obj)
  (append lst (list obj)))


(defun asdf (system)
  (asdf:oos 'asdf:load-op system))


(defun current-date-time-string ()
  (multiple-value-bind (sec min hou day mon yea)
      (decode-universal-time (get-universal-time))
    (format nil "~D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
            yea mon day hou min sec)))


(defun defalias (function alias)
  "Defines an alias for FUNCTION, meaning it can be called with ALIAS as well."
  (setf (symbol-function alias) function))


(defun error-message (msg)
  (format *error-output* "~&E: ~A~%" msg))


(defun last1 (lst)
  (car (last lst)))


(defun mkfstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (format s a))))


(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))


(defun print-hash (hash)
  (maphash (lambda (key value)
             (format t "~S: ~S~%" key value))
           hash))


(defun quit ()
  (cl-user::quit))


(defun verbose (msg)
  (format *standard-output* "~&D: ~A~%" msg))


(defun vlength (v)
  "Returns the length of vector V."
  (loop for n across v
        sum (* n n) into m
        finally (return (sqrt m))))


(defun write-to-file (name object)
  (with-open-file (f name :direction :output)
    (format f "~S~%" object)))
