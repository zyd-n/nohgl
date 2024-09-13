(in-package #:nohgl)

;; All of this is horribly wasteful. I don't really do anything with this
;; struct aside from take it apart and pack the values into an
;; array. Ultimately, vertices (attributes) are lists of numbers. Trying to
;; make the interface more descriptive might be a losing battle if this is the
;; result.
;;
;; Therefore,
;; FIXME: God, please.

(defstruct (vertex (:constructor vertex))
  pos
  uv)

(defclass shape () ()
  (:documentation
   "Empty class used to check if something is a shape or do general operations on shapes"))

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

(define-shape half-size-upfacing-plane (0 1 2 2 3 0)
  (vertex :pos (vec3 -0.5 +0.0 +0.5) :uv (vec2 0.0 1.0))
  (vertex :pos (vec3 +0.5 +0.0 +0.5) :uv (vec2 1.0 1.0))
  (vertex :pos (vec3 +0.5 +0.0 -0.5) :uv (vec2 1.0 0.0))
  (vertex :pos (vec3 -0.5 +0.0 -0.5) :uv (vec2 0.0 0.0)))

(define-shape cube (0 1 2 2 3 0 4 5 6 6 7 4 8 9 10 10 11 8 12 13 14 14 15 12 16 17 18 18 19 16 20 21 22 22 23 20)
  (vertex :pos (vec3 -0.5 +0.5 -0.5) :uv (vec2 +0.0 +1.0))
  (vertex :pos (vec3 -0.5 -0.5 -0.5) :uv (vec2 +0.0 +0.0))
  (vertex :pos (vec3 +0.5 -0.5 -0.5) :uv (vec2 +1.0 +0.0))
  (vertex :pos (vec3 +0.5 +0.5 -0.5) :uv (vec2 +1.0 +1.0))

  (vertex :pos (vec3 -0.5 +0.5 +0.5) :uv (vec2 +0.0 +1.0))
  (vertex :pos (vec3 -0.5 -0.5 +0.5) :uv (vec2 +0.0 +0.0))
  (vertex :pos (vec3 +0.5 -0.5 +0.5) :uv (vec2 +1.0 +0.0))
  (vertex :pos (vec3 +0.5 +0.5 +0.5) :uv (vec2 +1.0 +1.0))

  (vertex :pos (vec3 -0.5 -0.5 +0.5) :uv (vec2 +0.0 +1.0))
  (vertex :pos (vec3 -0.5 -0.5 -0.5) :uv (vec2 +0.0 +0.0))
  (vertex :pos (vec3 -0.5 +0.5 -0.5) :uv (vec2 +1.0 +0.0))
  (vertex :pos (vec3 -0.5 +0.5 +0.5) :uv (vec2 +1.0 +1.0))

  (vertex :pos (vec3 +0.5 -0.5 -0.5) :uv (vec2 +0.0 +1.0))
  (vertex :pos (vec3 +0.5 -0.5 +0.5) :uv (vec2 +0.0 +0.0))
  (vertex :pos (vec3 +0.5 +0.5 +0.5) :uv (vec2 +1.0 +0.0))
  (vertex :pos (vec3 +0.5 +0.5 -0.5) :uv (vec2 +1.0 +1.0))

  (vertex :pos (vec3 -0.5 -0.5 +0.5) :uv (vec2 +0.0 +1.0))
  (vertex :pos (vec3 +0.5 -0.5 +0.5) :uv (vec2 +0.0 +0.0))
  (vertex :pos (vec3 +0.5 -0.5 -0.5) :uv (vec2 +1.0 +0.0))
  (vertex :pos (vec3 -0.5 -0.5 -0.5) :uv (vec2 +1.0 +1.0))

  (vertex :pos (vec3 -0.5 +0.5 +0.5) :uv (vec2 +0.0 +1.0))
  (vertex :pos (vec3 +0.5 +0.5 +0.5) :uv (vec2 +0.0 +0.0))
  (vertex :pos (vec3 +0.5 +0.5 -0.5) :uv (vec2 +1.0 +0.0))
  (vertex :pos (vec3 -0.5 +0.5 -0.5) :uv (vec2 +1.0 +1.0)))
