(defpackage :color-gradients
  (:use :common-lisp)
  (:export #:make-linear-gradient #:make-radial-gradient #:make-angular-gradient
	   #:make-multistop-table
	   #:*steps-per-stop*))
