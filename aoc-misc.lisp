(defpackage :aoc-misc
  (:use :cl)
  (:export :read-broken-lines
           :read-input-as-list
           :read-input-as-array
           :count-valid
           :snoc
           :split-at
           :take-until
           :replicate))

(in-package :aoc-misc)


(defun read-input (stream func)
  (let ((line (read-line stream nil)))
    (when line (cons (funcall func line) (read-input stream func)))))

(defun create-file-name (day suffix)
  (concatenate
    'string
    (format nil "inputs/day~2,'0d" day)
    (when suffix (format nil "-~a" suffix))))

(defun read-input-as-list (day &optional (func #'identity) suffix)
  (with-open-file (stream (create-file-name day suffix))
    (read-input stream func)))

(defun read-input-as-array (day &optional (func #'identity) suffix)
  (let
    ((input-lst (read-input-as-list day (lambda (line) (map 'vector func line)) suffix)))
    (make-array (list (length input-lst) (length (first input-lst)))
                :initial-contents input-lst)))

(defun concatenate-lines (lst space? &optional str)
  (let ((line (first lst)))
    (cond
      ((and (null lst) (null str)) nil)
      ((zerop (length line)) (cons str (concatenate-lines (rest lst) space?)))
      (t
        (concatenate-lines
          (rest lst)
          space?
          (if (null str)
            line
            (concatenate 'string str (when space? " ") line)))))))

(defun read-broken-lines (day space?)
  (concatenate-lines (read-input-as-list day) space?))

(defun count-valid (predicate lst)
  (reduce
    (lambda (c e) (if (funcall predicate e) (1+ c) c))
    lst :initial-value 0))

(defun snoc (lst elm)
  (append lst (list elm)))

(defun take-until (test lst)
  (if (or (null lst) (funcall test (car lst)))
    nil
    (cons
      (car lst)
      (take-until test (cdr lst)))))

(defun split-at (n lst &optional lst2)
  (if (zerop n)
    (values (reverse lst2) lst)
    (split-at (1- n) (cdr lst) (cons (car lst) lst2))))

(defun replicate (n e)
  (unless (zerop n)
    (cons e (replicate (1- n) e))))
