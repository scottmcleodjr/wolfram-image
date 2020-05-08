;;;; package.lisp

(defpackage #:wolfram-image
  (:use #:cl
        #:imago)
  (:import-from #:alexandria #:iota)
  (:export #:make-image))
