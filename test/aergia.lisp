(in-package #:aergia-test)

(5am:in-suite aergia)

(5am:test cat
  (5am:is-true (string= "foobarbaz" (aergia::cat "foo" "bar" "baz"))))

(5am:test cat-with-spaces
  (5am:is-true (string= "foo bar baz" (aergia::cat-with-spaces "foo" "bar" "baz"))))

(5am:test get-arg
  (5am:is-true (string= "bar" (aergia::get-arg '(("foo" . "bar")) "foo")))
  (5am:is-false (string= "bar" (aergia::get-arg '(("bar" . "foo")) "foo"))))

(5am:test has-none-arg
  (5am:is-true (aergia::has-none-arg '(("foo")) "foo")))

(5am:test clean-stars
  (5am:is-true (string= "foo" (aergia::clean-stars "*foo*"))))

(5am:test help
  (5am:is-true (stringp (aergia::help))))
