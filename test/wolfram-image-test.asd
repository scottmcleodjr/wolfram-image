;;;; wolfram-image-test.asd

(asdf:defsystem #:wolfram-image-test
  :description "Test cases for wolfram-images"
  :author "S M McLeod <s.mcleodjr@gmail.com>"
  :license  "TBD"
  :version "0.0.1"
  :depends-on ("imago" "lisp-unit" "wolfram-image")
  :serial t
  :components ((:file "package-test")
               (:file "wolfram-image-test")))
