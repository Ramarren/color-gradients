(in-package :gradients)

(defun dist (x1 y1 x2 y2)
  (let ((xd (- x2 x1))
	(yd (- y2 y1)))
    (sqrt (+ (* xd xd) (* yd yd)))))

(defun gen-calc-col (d c1 c2 dither)
  (flet ((wav (a b d1 d2)
	   (/ (+ (* a d1)(* b d2)) d))
	 (wav-dither (a b d1 d2)
	   (+ (/ (+ (* a d1)(* b d2)) d) (- (random dither) (/ dither 2)))))
    (let ((wavf (if (zerop dither) #'wav #'wav-dither)))
     #'(lambda (d1 d2)
	 (declare (inline wav wav-dither))
	 (if (> d1 d)
	   c2
	   (if (> d2 d)
	     c1
	     (mapcar #'(lambda (x y) (max 0 (min (round (funcall wavf x y d1 d2)) 255)))
		     c2 c1)))))))
      

(defun gradient-horiz (w h x1 x2 c1 c2 dither) 
  (declare (inline calc-col))
  (let ((data (make-array (list w h)))
	(calc-col (gen-calc-col (abs (- x2 x1)) c1 c2 0)))
    (dotimes (j h)
      (let ((val (funcall calc-col (abs (- j x1)) (abs (- j x2)))))
	(dotimes (i w)
	  (setf (aref data i j) 
		(mapcar #'(lambda (x) 
			    (min 255 
				 (max 0 
			           (+ x (- (random dither) (/ dither 2)))))) val)))))
    data))

(defun gradient-verti (w h y1 y2 c1 c2 dither)
  (declare (inline calc-col))
  (let ((data (make-array (list w h)))
	(calc-col (gen-calc-col (abs (- y2 y1)) c1 c2 0)))
    (dotimes (i w)
      (let ((val (funcall calc-col (abs (- i y1)) (abs (- i y2)))))
	(dotimes (j h)
	  (setf (aref data i j) 
		(mapcar #'(lambda (x) 
			    (min 255 
				 (max 0 
			           (+ x (- (random dither) (/ dither 2)))))) val)))))
    data))

(defun gradient-diag (w h x1 y1 x2 y2 col1 col2 dither)
  (declare (inline calc-col dist))
  (let ((data (make-array (list w h)))
	(calc-col (gen-calc-col (dist x1 y1 x2 y2) col1 col2 dither))
	(A (cond
	     ((and (zerop x1)(zerop y1)) (/ y2 x2))
	     ((and (zerop x2)(zerop y2)) (/ y1 x1))
	     ((and (not (zerop y2))(not (zerop x2))(= (/ y1 y2)(/ x1 x2))) 1)
	     (t (/ (- y1 y2)(- (* y2 x1)(* x2 y1))))))
	(B (cond
	     ((or (and (zerop x1)(zerop y1))
		  (and (zerop x2)(zerop y2))) -1)
	     ((and (not (zerop y2))(not (zerop x2))(= (/ y1 y2)(/ x1 x2))) -1)
	     (t (/ (- x2 x1)(- (* y2 x1)(* x2 y1)))))))
    (let ((C1 (/ (* A B) (+ (* A A) (* B B))))
	  (C2 (- y1 (* (/ B A) x1)))
	  (C4 (- y2 (* (/ B A) x2)))
	  (C3 (- (/ (* A A) (+ (* A A)(* B B))))))
      (let ((C3+ (1+ C3))
	    (C5 (* C3 C2))
	    (C6 (* C3 C4)))
	(dotimes (i w)
	  (dotimes (j h)
	    (let ((V1 (+ (* (/ A B) i) j)))
	      (setf (aref data i j) (funcall calc-col 
				      (dist i j (* C1 (- V1 C2)) (- (* C3+ V1) C5))
				      (dist i j (* C1 (- V1 C4)) (- (* C3+ V1) C6)))))))))
    data))

(defun gradient (w h x1 y1 x2 y2 c1 c2 &optional (dither 0)) 
  (if (and (= x1 x2)(= y1 y2)) (error "Gradient points must be different"))
  (cond
    ((= x1 x2) (gradient-horiz w h y1 y2 c1 c2 dither))
    ((= y1 y2) (gradient-verti w h x1 x2 c1 c2 dither))
    (t (gradient-diag w h x1 y1 x2 y2 c1 c2 dither))))

(defun plot-gradient (bitmap gdata)
  (dotimes (i (array-dimension gdata 0))
    (dotimes (j (array-dimension gdata 1))
      (apply #'pixel-rgba `(,bitmap ,i ,j ,@(aref gdata i j) 255)))))
