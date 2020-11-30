(defpackage :aoc-misc
  (:use :cl)
  (:export :read-input-as-list))

(in-package :aoc-misc)


(defun read-input (stream func)
  (let ((line (read-line stream nil)))
    (when line (cons (funcall func line) (read-input stream func)))))


(defun read-input-as-list (day &optional (func #'identity))
  (with-open-file (stream (format nil "inputs/day~2,'0d" day))
    (read-input stream func)))
    
