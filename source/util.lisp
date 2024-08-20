(in-package #:nohgl)

(defconstant SIZE-OF-FLOAT (cffi:foreign-type-size :float))

(defun gfill (type &rest args)
  (let ((arr (gl:alloc-gl-array type (length args))))
    (dotimes (i (length args) arr)
      (setf (gl:glaref arr i)
            (elt args i)))))

(defmacro with-uniform-location (uniform-name vao &body body)
  `(let ((,(intern (symbol-name 'uniform-location) *package*)
           (gl:get-uniform-location (program (get-vao ,vao)) ,uniform-name)))
     (progn (gl:use-program (program (get-vao ,vao)))
            ,@body)))
