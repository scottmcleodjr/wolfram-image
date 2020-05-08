;;;; wolfram-image.asd

(asdf:defsystem #:wolfram-image
  :description "Creates images of Wolfram Automata"
  :author "S M McLeod <s.mcleodjr@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :depends-on ("alexandria" "imago")
  :serial t
  :components ((:file "package")
               (:file "wolfram-image")))
