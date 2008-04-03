(in-package :color-gradients)

(defun dist (x1 y1 x2 y2)
  (let ((xd (- x2 x1))
	(yd (- y2 y1)))
    (sqrt (coerce (+ (* xd xd) (* yd yd)) 'single-float))))

(defun ensure-rgba (color)
  (destructuring-bind (r g b . pa) color
    (check-type r (integer 0 255))
    (check-type g (integer 0 255))
    (check-type b (integer 0 255))
    (if (and (consp pa)
	     (typep (car pa) '(integer 0 255)))
	color
	(list r g b 255))))

(defun precompute-color-table (color1 color2 steps)
  (let ((color-table (make-array steps :element-type 'list)))
    (flet ((wav (a b frac)
	     (round (+ (* a frac)(* b (- 1 frac))))))
     (destructuring-bind (r1 g1 b1 a1) (ensure-rgba color1)
       (destructuring-bind (r2 g2 b2 a2) (ensure-rgba color2)
	 (dotimes (k steps color-table)
	   (let ((frac (/ k steps)))
	     (setf (aref color-table k) (list (wav r2 r1 frac)
					      (wav g2 g1 frac)
					      (wav b2 b1 frac)
					      (wav a2 a1 frac))))))))))

(defun make-linear-gradient (point-1 point-2
			     &key (color-1 '(0 0 0 255)) (color-2 '(255 255 255 255))
			          (steps 500) (table nil) (fixnum-xy nil) (repeat :pad))
  (destructuring-bind (x1 y1) point-1
    (destructuring-bind (x2 y2) point-2
      (cond
	((and (= x1 x2)
	      (= y1 y2))
	 (error "Points defining a gradient must be different."))
	((= x1 x2)
	 (make-linear-vertical-gradient point-1 point-2
					:color-1 color-1 :color-2 color-2 :steps steps
					:table table :repeat repeat))
	((= y1 y2)
	 (make-linear-horizontal-gradient point-1 point-2
					  :color-1 color-1 :color-2 color-2 :steps steps
					  :table table :repeat repeat))
	(t (make-linear-general-gradient point-1 point-2
					  :color-1 color-1 :color-2 color-2 :steps steps
					  :table table :fixnum-xy fixnum-xy :repeat repeat))))))

(defun make-linear-vertical-gradient (point-1 point-2 &key color-1 color-2 steps table repeat)
  (let ((color-table (if table
			 table
			 (precompute-color-table color-1 color-2 steps))))
    (destructuring-bind (x1 y1) point-1
      (declare (ignore x1))
      (destructuring-bind (x2 y2) point-2
	(declare (ignore x2))
	(let ((d (abs (- y2 y1)))
	      (last-step (1- (array-dimension color-table 0))))
	 (values #'(lambda (x y)
		     (declare (ignore x))
		     (let ((d1 (abs (- y y1)))
			   (d2 (abs (- y y2))))
		       (ecase repeat
			 (:pad
			  (cond ((> d1 d) (aref color-table last-step))
				((> d2 d) (aref color-table 0))
				(t (aref color-table (round (* last-step (/ d1 d)))))))
			 (:repeat
			  (aref color-table (mod (round (* last-step (/ d1 d))) (1+ last-step))))
			 (:reflect
			  (aref color-table
				(let ((reflect-index (mod (round (* last-step (/ d1 d))) (* 2 (1+ last-step)))))
				  (if (> reflect-index last-step)
				      (- (* 2 last-step) reflect-index)
				      reflect-index)))))))
		 color-table))))))

(defun make-linear-horizontal-gradient (point-1 point-2 &key color-1 color-2 steps table repeat)
  (let ((color-table (if table
			 table
			 (precompute-color-table color-1 color-2 steps))))
    (destructuring-bind (x1 y1) point-1
      (declare (ignore y1))
      (destructuring-bind (x2 y2) point-2
	(declare (ignore y2))
	(let ((d (abs (- x2 x1)))
	      (last-step (1- (array-dimension color-table 0))))
	 (values #'(lambda (x y)
		     (declare (ignore y))
		     (let ((d1 (abs (- x x1)))
			   (d2 (abs (- x x2))))
		       (ecase repeat
			 (:pad
			  (cond ((> d1 d) (aref color-table last-step))
				((> d2 d) (aref color-table 0))
				(t (aref color-table (round (* last-step (/ d1 d)))))))
			 (:repeat
			  (aref color-table (mod (round (* last-step (/ d1 d))) (1+ last-step))))
			 (:reflect
			  (aref color-table
				(let ((reflect-index (mod (round (* last-step (/ d1 d))) (* 2 (1+ last-step)))))
				  (if (> reflect-index last-step)
				      (- (* 2 last-step) reflect-index)
				      reflect-index)))))))
		 color-table))))))

(defun make-linear-general-gradient (point-1 point-2 &key color-1 color-2 steps table fixnum-xy repeat)
  (let ((color-table (if table
			 table
			 (precompute-color-table color-1 color-2 steps))))
    (destructuring-bind (x1 y1) point-1
      (destructuring-bind (x2 y2) point-2
	(let ((A (cond
		   ((and (zerop x1)(zerop y1)) (/ y2 x2))
		   ((and (zerop x2)(zerop y2)) (/ y1 x1))
		   ((and (not (zerop y2))(not (zerop x2))(= (/ y1 y2)(/ x1 x2))) 1)
		   (t (/ (- y1 y2)(- (* y2 x1)(* x2 y1))))))
	      (B (cond
		   ((or (and (zerop x1)(zerop y1))
			(and (zerop x2)(zerop y2))) -1)
		   ((and (not (zerop y2))(not (zerop x2))(= (/ y1 y2)(/ x1 x2))) -1)
		   (t (/ (- x2 x1)(- (* y2 x1)(* x2 y1))))))
	      (d (dist x1 y1 x2 y2))
	      (last-step (1- (array-dimension color-table 0))))
	  (let ((AB (coerce (/ A B) 'single-float))
		(C1 (coerce (/ (* A B) (+ (* A A) (* B B))) 'single-float))
		(C2 (coerce (- y1 (* (/ B A) x1)) 'single-float))
		(C4 (coerce (- y2 (* (/ B A) x2)) 'single-float))
		(C3 (- (/ (* A A) (+ (* A A)(* B B))))))
	    (let ((C3+ (coerce (1+ C3) 'single-float))
		  (C5 (coerce (* C3 C2) 'single-float))
		  (C6 (coerce (* C3 C4) 'single-float)))
	      #+sbcl(declare (sb-ext:muffle-conditions sb-ext:compiler-note))
	      (values
	       (if fixnum-xy
		   (flet ((odist (fx fy sx sy)
			    (declare (fixnum fx fy)
				     (single-float sx sy)
				     (optimize (speed 3)))
			    (let ((xd (- fx sx))
				  (yd (- fy sy)))
			      (sqrt (coerce (+ (* xd xd) (* yd yd)) 'single-float)))))
		    #'(lambda (x y)
			(declare (fixnum x y))
			(let ((V1 (coerce (+ (* AB x) y) 'single-float)))
			  (declare (optimize (speed 3)))
			  (let ((d1 (odist x y (* C1 (- V1 C2)) (- (* C3+ V1) C5)))
				(d2 (odist x y (* C1 (- V1 C4)) (- (* C3+ V1) C6))))
			    (ecase repeat
			      (:pad
			       (cond ((> d1 d) (aref color-table last-step))
				     ((> d2 d) (aref color-table 0))
				     (t (aref color-table (round (* last-step (/ d1 d)))))))
			      (:repeat
			       (aref color-table (mod (round (* last-step (/ d1 d))) (1+ last-step))))
			      (:reflect
			       (aref color-table
				     (let ((reflect-index (mod (round (* last-step (/ d1 d))) (* 2 (1+ last-step)))))
				       (if (> reflect-index last-step)
					   (- (* 2 last-step) reflect-index)
					   reflect-index)))))))))
		   #'(lambda (x y)
		       (declare (inline dist))
		       (let ((V1 (coerce (+ (* AB x) y) 'single-float)))
			 (declare (optimize (speed 3)))
			 (let ((d1 (dist x y (* C1 (- V1 C2)) (- (* C3+ V1) C5)))
			       (d2 (dist x y (* C1 (- V1 C4)) (- (* C3+ V1) C6))))
			   (ecase repeat
			     (:pad
			      (cond ((> d1 d) (aref color-table last-step))
				    ((> d2 d) (aref color-table 0))
				    (t (aref color-table (round (* last-step (/ d1 d)))))))
			     (:repeat
			      (aref color-table (mod (round (* last-step (/ d1 d))) (1+ last-step))))
			     (:reflect
			      (aref color-table
				    (let ((reflect-index (mod (round (* last-step (/ d1 d))) (* 2 (1+ last-step)))))
				      (if (> reflect-index last-step)
					  (- (* 2 last-step) reflect-index)
					  reflect-index)))))))))
	       color-table))))))))

(defun make-radial-gradient (circle-1 circle-2
			     &key (color-1 '(0 0 0 255)) (color-2 '(255 255 255 255))
			          (steps 500) (table nil) (fixnum-xy nil) (repeat :pad))
  (declare (ignore fixnum-xy))
  (let ((color-table (if table
			 table
			 (precompute-color-table color-1 color-2 steps))))
   (destructuring-bind (x1 y1 r1) circle-1
     (destructuring-bind (x2 y2 r2) circle-2
       (let ((last-step (1- (array-dimension color-table 0)))
	     (cdx (- x2 x1))
	     (cdy (- y2 y1))
	     (dr (- r2 r1)))
	 (let ((A (+ (* cdx cdx) (* cdy cdy) (- (* dr dr)))))
	   (values
	    #'(lambda (x y)
		(let ((pdx (- x x1))
		      (pdy (- y y1)))
		  (let ((B (* -2 (+ (* pdx cdx) (* pdy cdy) (* r1 dr))))
			(C (+ (* pdx pdx) (* pdy pdy) (- (* r1 r1)))))
		    (let ((delta (- (* B B) (* 4 A C))))
		      (let ((d (if (not (minusp delta))
				   (/ (- (- B) (sqrt delta)) (* 2 A))
				   0)))
			(ecase repeat
			  (:pad
			   (cond ((> d 1) (aref color-table last-step))
				 ((< d 0) (aref color-table 0))
				 (t (aref color-table (round (* last-step d))))))
			  (:repeat
			   (aref color-table (mod (round (* last-step d)) (1+ last-step))))
			  (:reflect
			   (aref color-table
				 (let ((reflect-index (mod (round (* last-step d)) (1+ (* 2 last-step)))))
				   (if (> reflect-index last-step)
				       (1- (- (* 2 (1+ last-step)) reflect-index))
				       reflect-index))))))))))
	    color-table)))))))
