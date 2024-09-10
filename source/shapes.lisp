(in-package #:nohgl)

(defstruct (vertex (:constructor vertex))
  pos
  uv)

(defclass shape () ()
  (:documentation
   "Empty class used to check if something is a shape or do general operations on shapes"))

;; (defun shape-p (obj))

(defun mapv (function vecs)
  (dotimes (i (length vecs))
    (loop for n across (varr (elt vecs i))
          do (funcall function n))))

(defmacro vertex-union (vertices)
  `(let ((vertex-objects (list ,@(loop for vertex in vertices collect vertex)))
         (array (make-array 0 :fill-pointer 0)))
     (loop for vertex in vertex-objects
           do (mapv (lambda (point) (vector-push-extend point array))
                    (list (vertex-pos vertex) (vertex-uv vertex)))
           finally (return array))))

;; One not so nice thing about this is that our exported classes aren't listed
;; in the package.lisp file. It's on us to manually add them.
(defmacro define-shape (name indices &rest vertices)
  `(progn (defclass ,name (shape)
            ((indices :initform ',indices :accessor indices)
             (attributes :initform (vertex-union ,vertices) :accessor attributes)))
          (export ',name)))

(define-shape z-facing-quad (2 1 0 3 2 0)
  (vertex :pos (vec3 -1.0 -1.0 +0.0) :uv (vec2 +0.0 +0.0))
  (vertex :pos (vec3 -1.0 +1.0 +0.0) :uv (vec2 +0.0 +1.0))
  (vertex :pos (vec3 +1.0 +1.0 +0.0) :uv (vec2 +1.0 +1.0))
  (vertex :pos (vec3 +1.0 -1.0 +0.0) :uv (vec2 +1.0 +0.0)))

(define-shape cube (0 1 3 1 2 3)
  ;; Face 1
  (vertex :pos (vec3 -0.5 -0.5 -0.5) :uv (vec2 +0.0 +0.0))
  (vertex :pos (vec3 +0.5 -0.5 -0.5) :uv (vec2 +1.0 +0.0))
  (vertex :pos (vec3 +0.5 +0.5 -0.5) :uv (vec2 +1.0 +1.0))
  (vertex :pos (vec3 +0.5 +0.5 -0.5) :uv (vec2 +1.0 +1.0))
  (vertex :pos (vec3 -0.5 +0.5 -0.5) :uv (vec2 +0.0 +1.0))
  (vertex :pos (vec3 -0.5 -0.5 -0.5) :uv (vec2 +0.0 +0.0))
  ;; Face 2
  (vertex :pos (vec3 -0.5 -0.5 +0.5) :uv (vec2 +0.0 +0.0))
  (vertex :pos (vec3 +0.5 -0.5 +0.5) :uv (vec2 +1.0 +0.0))
  (vertex :pos (vec3 +0.5 +0.5 +0.5) :uv (vec2 +1.0 +1.0))
  (vertex :pos (vec3 +0.5 +0.5 +0.5) :uv (vec2 +1.0 +1.0))
  (vertex :pos (vec3 -0.5 +0.5 +0.5) :uv (vec2 +0.0 +1.0))
  (vertex :pos (vec3 -0.5 -0.5 +0.5) :uv (vec2 +0.0 +0.0))
  ;; Face 3
  (vertex :pos (vec3 -0.5 +0.5 +0.5) :uv (vec2 +1.0 +0.0))
  (vertex :pos (vec3 -0.5 +0.5 -0.5) :uv (vec2 +1.0 +1.0))
  (vertex :pos (vec3 -0.5 -0.5 -0.5) :uv (vec2 +0.0 +1.0))
  (vertex :pos (vec3 -0.5 -0.5 -0.5) :uv (vec2 +0.0 +1.0))
  (vertex :pos (vec3 -0.5 -0.5 +0.5) :uv (vec2 +0.0 +0.0))
  (vertex :pos (vec3 -0.5 +0.5 +0.5) :uv (vec2 +1.0 +0.0))
  ;; Face 4
  (vertex :pos (vec3 +0.5 +0.5 +0.5) :uv (vec2 +1.0 +0.0))
  (vertex :pos (vec3 +0.5 +0.5 -0.5) :uv (vec2 +1.0 +1.0))
  (vertex :pos (vec3 +0.5 -0.5 -0.5) :uv (vec2 +0.0 +1.0))
  (vertex :pos (vec3 +0.5 -0.5 -0.5) :uv (vec2 +0.0 +1.0))
  (vertex :pos (vec3 +0.5 -0.5 +0.5) :uv (vec2 +0.0 +0.0))
  (vertex :pos (vec3 +0.5 +0.5 +0.5) :uv (vec2 +1.0 +0.0))
  ;; Face 5
  (vertex :pos (vec3 -0.5 -0.5 -0.5) :uv (vec2 +0.0 +1.0))
  (vertex :pos (vec3 +0.5 -0.5 -0.5) :uv (vec2 +1.0 +1.0))
  (vertex :pos (vec3 +0.5 -0.5 +0.5) :uv (vec2 +1.0 +0.0))
  (vertex :pos (vec3 +0.5 -0.5 +0.5) :uv (vec2 +1.0 +0.0))
  (vertex :pos (vec3 -0.5 -0.5 +0.5) :uv (vec2 +0.0 +0.0))
  (vertex :pos (vec3 -0.5 -0.5 -0.5) :uv (vec2 +0.0 +1.0))
  ;; Face 6
  (vertex :pos (vec3 -0.5 +0.5 -0.5) :uv (vec2 +0.0 +1.0))
  (vertex :pos (vec3 +0.5 +0.5 -0.5) :uv (vec2 +1.0 +1.0))
  (vertex :pos (vec3 +0.5 +0.5 +0.5) :uv (vec2 +1.0 +0.0))
  (vertex :pos (vec3 +0.5 +0.5 +0.5) :uv (vec2 +1.0 +0.0))
  (vertex :pos (vec3 -0.5 +0.5 +0.5) :uv (vec2 +0.0 +0.0))
  (vertex :pos (vec3 -0.5 +0.5 -0.5) :uv (vec2 +0.0 +1.0)))
