(asdf:defsystem #:aergia-test
  :description "Test package for aergia"
  :author "Florian Margaine <florian@margaine.com>"
  :license "MIT License"
  :serial t
  :depends-on ("aergia" "fiveam")
  :components ((:module "test"
			:components
			((:file "test-suites")
			 (:file "aergia")))))
