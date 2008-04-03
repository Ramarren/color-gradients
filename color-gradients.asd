(asdf:defsystem color-gradients
  :version "0"
  :description "Color gradients"
  :author "Ramarren"
  :licence "BSD-style"
  :depends-on ()
  :components ((:static-file "README")
	       (:file "package")
	       (:file "gradients" :depends-on ("package"))))

