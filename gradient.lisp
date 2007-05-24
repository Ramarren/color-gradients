(let ((*default-pathname-defaults* (truename "sdlbind/")))
  (require "sdlext" "sdlext"))

(defpackage :gradients (:use :common-lisp :sdl)
  (:export gradient plot-gradient))

(in-package :gradients)

(defun dist (x1 y1 x2 y2)
  (let ((xd (- x2 x1))
	(yd (- y2 y1)))
    (sqrt (+ (* xd xd) (* yd yd)))))

(defun calc-col (d1 d2 c1 c2)
  (if (zerop d1) 
    c1
    (if (zerop d2) 
      c2
      (let ((d (+ d1 d2)))
	(flet ((wav (a b)
	  (/ (+ (* a d1)(* b d2)) d))))
	  (declare (inline wav))
	  (mapcar #'wav c1 c2)))))
      

(defun gradient-horiz (w h x1 x2 y c1 c2) )

(defun gradient-verti (w h x y1 y2 c1 c2) )

(defun gradient-diag (w h x1 y1 x2 y2 c1 c2)
  (declare (inline cacl-col dist))
  (if (and (= x1 x2)(= y1 y2)) (error "Gradient points must be different"))
  (let ((data (make-array (list w h)))
	(A (/ (- y1 y2)(- (* y2 x1)(* x2 y1))))
	(B (/ (- x2 x1)(- (* y2 x1)(* x2 y1)))))
    (let ((C1 (/ (* A B) (+ (* A A) (* B B))))
	  (C2 (- y1 (* (/ B A) x1)))
	  (C4 (- y2 (* (/ B A) x2)))
	  (C3 (- (/ (* A A) (+ (* A A)(* B B))))))
      (let ((C3+ (1+ C3))
	    (C5 (* C3 C2))
	    (C6 (* C3 C4)))
	(dotimes (i w)
	  (dotimes (j h)
	    (let ((V1 (+ (* A i) (* B j))))
	      (setf (aref data i j) (calc-col 
				      (dist i j (* C1 (- V1 C2)) (- (* C3+ V1) C5))
				      (dist i j (* C1 (- V1 C4)) (- (* C3+ V1) C6))
				      c1 c2)))))))))

(defun gradient (w h x1 y1 x2 y2 c1 c2) )

(defun plot-gradient (bitmap gdata)
  (dotimes (i (array-dimension gdata 0))
    (dotimes (j (array-dimension gdata 1))
      (apply #'pixel-rgba `(,bitmap i j ,@(aref gdata i j) 255)))))
