(asdf:defsystem #:aergia
  :description "Uses LXC to run short-lived containers and run tests on it."
  :author "Florian Margaine <florian@margaine.com>"
  :licence "MIT License"
  :serial t
  :depends-on ("external-program" ;; run shell commands
	       "cl-ppcre" ;; parse shell commands outputs
	       "getopt" ;; parse CLI options
	       "cl-ansi-text") ;; terminal colors
  :in-order-to ((asdf:test-op (asdf:test-op #:aergia-test)))
  :components ((:module "src"
			:components
			((:file "package")
			 (:file "lxc")
			 (:file "aergia")))))
