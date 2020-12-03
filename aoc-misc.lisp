(defpackage :aoc-misc
  (:use :cl)
  (:export :read-input-as-list
           :read-input-as-array
           :count-valid))

(in-package :aoc-misc)


(defun read-input (stream func)
  (let ((line (read-line stream nil)))
    (when line (cons (funcall func line) (read-input stream func)))))


(defun read-input-as-list (day &optional (func #'identity))
  (with-open-file (stream (format nil "inputs/day~2,'0d" day))
    (read-input stream func)))


(defun read-input-as-array (day)
  (let
    ((input-lst (read-input-as-list day)))
    (make-array (list (length input-lst) (length (first input-lst)))
                :initial-contents input-lst)))

(defun count-valid (predicate lst)
  (reduce
    (lambda (c e) (if (funcall predicate e) (1+ c) c))
    lst :initial-value 0))
