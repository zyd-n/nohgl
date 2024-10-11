(in-package #:nohgl)

(defun model-matrix (&key (translate (vec 0 0 0)) (scale (vec 1 1 1)))
  (m* (meye 4)
      (mtranslation translate)
      (mscaling scale)))
