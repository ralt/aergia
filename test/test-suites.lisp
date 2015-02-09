(defpackage #:aergia-test
  (:use :cl))

(in-package #:aergia-test)

(5am:def-suite aergia)

(defmethod asdf:perform ((op asdf:test-op) (system (eql (asdf:find-system :aergia-test))))
  (5am:run! 'aergia))
