(asdf:defsystem gradients
  :version "0"
  :description "Color gradients"
  :author "Ramarren"
  :licence "BSD-style"
  :depends-on (:iterate)
  :components ((:file "package")
	       (:file "gradients" :depends-on ("package"))))

