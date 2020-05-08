;;;; wolfram-image-test.lisp

(in-package #:wolfram-image-test)


(define-test test-make-image-with-rule-30-and-default-pattern
  (let ((test-image (make-image :cells-wide 5 :cells-high 2
                                :cell-width 1 :cell-height 1)))
    (assert-equal (image-pixel test-image 0 0) +blue+)
    (assert-equal (image-pixel test-image 1 0) +blue+)
    (assert-equal (image-pixel test-image 2 0) +white+)
    (assert-equal (image-pixel test-image 3 0) +blue+)
    (assert-equal (image-pixel test-image 4 0) +blue+)
    (assert-equal (image-pixel test-image 0 1) +blue+)
    (assert-equal (image-pixel test-image 1 1) +white+)
    (assert-equal (image-pixel test-image 2 1) +white+)
    (assert-equal (image-pixel test-image 3 1) +white+)
    (assert-equal (image-pixel test-image 4 1) +blue+)))


(define-test test-make-image-with-rule-110-and-custom-pattern
  (let ((test-image (make-image :cells-wide 5 :cells-high 2 :rule 110
                                :cell-width 1 :cell-height 1
                                :initial-gen #(nil t nil t nil))))
    (assert-equal (image-pixel test-image 0 0) +blue+)
    (assert-equal (image-pixel test-image 1 0) +white+)
    (assert-equal (image-pixel test-image 2 0) +blue+)
    (assert-equal (image-pixel test-image 3 0) +white+)
    (assert-equal (image-pixel test-image 4 0) +blue+)
    (assert-equal (image-pixel test-image 0 1) +white+)
    (assert-equal (image-pixel test-image 1 1) +white+)
    (assert-equal (image-pixel test-image 2 1) +white+)
    (assert-equal (image-pixel test-image 3 1) +white+)
    (assert-equal (image-pixel test-image 4 1) +blue+)))


(define-test test-make-image-with-custom-cell-width
  (let ((test-image (make-image :cells-wide 2 :cells-high 2
                                :cell-width 2 :cell-height 1
                                :initial-gen #(t nil))))
    (assert-equal (image-pixel test-image 0 0) +white+)
    (assert-equal (image-pixel test-image 1 0) +white+)
    (assert-equal (image-pixel test-image 2 0) +blue+)
    (assert-equal (image-pixel test-image 3 0) +blue+)
    (assert-equal (image-pixel test-image 0 1) +blue+)
    (assert-equal (image-pixel test-image 1 1) +blue+)
    (assert-equal (image-pixel test-image 2 1) +white+)
    (assert-equal (image-pixel test-image 3 1) +white+)))


(define-test test-make-image-with-custom-cell-height
  (let ((test-image (make-image :cells-wide 2 :cells-high 2
                                :cell-width 1 :cell-height 2
                                :initial-gen #(t nil))))
    (assert-equal (image-pixel test-image 0 0) +white+)
    (assert-equal (image-pixel test-image 1 0) +blue+)
    (assert-equal (image-pixel test-image 0 1) +white+)
    (assert-equal (image-pixel test-image 1 1) +blue+)
    (assert-equal (image-pixel test-image 0 2) +blue+)
    (assert-equal (image-pixel test-image 1 2) +white+)
    (assert-equal (image-pixel test-image 0 3) +blue+)
    (assert-equal (image-pixel test-image 1 3) +white+)))


(define-test test-make-image-background-color-set-correctly
  (let* ((test-color (make-color 1 2 3))
         (test-image (make-image :cells-wide 3 :cells-high 1
                                 :cell-width 1 :cell-height 1
                                 :background-color test-color)))
    (assert-equal (image-pixel test-image 0 0) test-color)
    (assert-equal (image-pixel test-image 1 0) +white+)
    (assert-equal (image-pixel test-image 2 0) test-color)))


(define-test test-make-image-foreground-color-set-correctly
  (let* ((test-color (make-color 1 2 3))
         (test-image (make-image :cells-wide 3 :cells-high 1
                                 :cell-width 1 :cell-height 1
                                 :foreground-color test-color)))
    (assert-equal (image-pixel test-image 0 0) +blue+)
    (assert-equal (image-pixel test-image 1 0) test-color)
    (assert-equal (image-pixel test-image 2 0) +blue+)))


(define-test test-make-image-throws-error-if-initial-gen-length-does-not-match-width
  (assert-error 'simple-error (make-image :initial-gen #(1 2 3)))
  (assert-error 'simple-error (make-image :initial-gen #(1 2 3) :cells-wide 2)))
