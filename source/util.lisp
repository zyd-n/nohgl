(in-package #:nohgl)

(defconstant SIZE-OF-FLOAT (cffi:foreign-type-size :float))
(defparameter *main-dir* (format nil "~A" (asdf:system-source-directory (asdf:find-system "nohgl"))))

(defvar *built-as-executable* nil)

(defun use-relative-dir ()
  (setf *main-dir* (uiop/os:getcwd))
  (setf *asset-dir* (format nil "~Aassets/" *main-dir*)))

(defun gfill (type &rest args)
  (let ((arr (gl:alloc-gl-array type (length args))))
    (dotimes (i (length args) arr)
      (setf (gl:glaref arr i)
            (elt args i)))))

(defun vfill (type v)
  (let ((arr (gl:alloc-gl-array type (length v))))
    (dotimes (i (length v) arr)
      (setf (gl:glaref arr i)
            (elt v i)))))

(defmacro with-uniform-location (uniform-name vao &body body)
  `(let ((,(intern (symbol-name 'uniform-location) *package*)
           (gl:get-uniform-location (program (get-vao ,vao)) ,uniform-name)))
     (progn (gl:use-program (program (get-vao ,vao)))
            ,@body)))

(defun radian (degree)
  (* degree (/ pi 180)))

(defun degree (radian)
  (* radian (/ 180 pi)))

(defmacro limit (n &key limiters min max)
  (let ((min-fn (gensym))
        (max-fn (gensym)))
    `(multiple-value-bind (,min-fn ,max-fn) ,limiters
       (cond ((< ,n ,min) (funcall ,min-fn))
             ((> ,n ,max) (funcall ,max-fn))
             (t ,n)))))
