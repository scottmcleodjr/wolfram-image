;;;; wolfram-image.lisp

(in-package #:wolfram-image)


(defmacro draw-filled-box (image corner-x corner-y width height color)
  (let ((p (gensym))
        (x (gensym))
        (y (gensym)))
    `(do-region-pixels (,image ,p ,x ,y ,corner-x ,corner-y ,width ,height)
       (setf ,p ,color))))


(defun next-cell-value (gen index rule)
  ; Set left and right values to match center on edges
  (let ((left  (if (> index 0) 
                   (svref gen (1- index))
                   (svref gen index)))
        (cell  (svref gen index))
        (right (if (< index (1- (length gen))) 
                   (svref gen (1+ index))
                   (svref gen index))))
    (< 0 (cond 
           ((and left cell right) (logand rule 128))
           ((and left cell)       (logand rule  64))
           ((and left right)      (logand rule  32))
           (left                  (logand rule  16))
           ((and cell right)      (logand rule   8))
           (cell                  (logand rule   4))
           (right                 (logand rule   2))
           (t                     (logand rule   1))))))


(defun next-generation (gen rule)
  (map 'vector 
       (lambda (i) (next-cell-value gen i rule))
       (iota (length gen))))


(defun first-generation (width initial-gen)
  (if initial-gen
      (make-array width :initial-contents initial-gen)
      (let ((gen (make-array width :initial-element nil)))
        (setf (svref gen (floor width 2)) t)
        gen)))


; These default values give a reasonable default image output
(defun make-image (&key (rule 30) (initial-gen nil)
                        (cells-wide 400) (cells-high 200)
                        (cell-width 3) (cell-height 3)
                        (foreground-color +white+) 
                        (background-color +blue+))
  (when (and initial-gen
             (not (eq (length initial-gen) cells-wide)))
        (error (format nil "There are ~A elements in :INITIAL-GEN, but :CELLS-WIDE is ~A"
                       (length initial-gen) cells-wide)))
  (let* ((image-width  (* cells-wide cell-width))
         (image-height (* cells-high cell-height))
         (init (first-generation cells-wide initial-gen))
         (image (make-instance 'imago:rgb-image :width image-width 
                               :height image-height)))
    (draw-filled-box image 0 0 image-width image-height background-color)
    (do ((r 0 (1+ r))
         (gen init (next-generation gen rule)))
        ((eq r cells-high) nil)
      (dotimes (n cells-wide)
        (when (svref gen n)
              (draw-filled-box image (* n cell-width) (* r cell-height)
                               cell-width cell-height foreground-color))))
    image))
