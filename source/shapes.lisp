(in-package #:nohgl)

(defclass shape () ()
  (:documentation
   "Empty class used to check if something is a shape or do general operations on shapes"))

(defmacro define-shape (name indices &rest vertices)
  `(let* ((verts (append ,@vertices))
          (array (make-array (length verts) :initial-contents verts)))
     (defclass ,name (shape)
       ((indices :initform ',indices :accessor indices)
        (attributes :initform array :accessor attributes)))
     (export ',name)))

(defmacro attribs (&key pos normal uv)
  `'(,@pos ,@normal ,@uv))

(define-shape wall-mesh (2 1 0 3 2 0)
  (attribs :pos (-1.0 -1.0 +0.0) :uv (+0.0 +0.0))
  (attribs :pos (-1.0 +1.0 +0.0) :uv (+0.0 +1.0))
  (attribs :pos (+1.0 +1.0 +0.0) :uv (+1.0 +1.0))
  (attribs :pos (+1.0 -1.0 +0.0) :uv (+1.0 +0.0)))

(define-shape floor-mesh (0 1 2 2 3 0)
  (attribs :pos (-0.5 +0.0 +0.5) :normal (0.0 1.0 0.0) :uv (0.0 1.0))
  (attribs :pos (+0.5 +0.0 +0.5) :normal (0.0 1.0 0.0) :uv (1.0 1.0))
  (attribs :pos (+0.5 +0.0 -0.5) :normal (0.0 1.0 0.0) :uv (1.0 0.0))
  (attribs :pos (-0.5 +0.0 -0.5) :normal (0.0 1.0 0.0) :uv (0.0 0.0)))

(define-shape cube-mesh (0 1 2 2 3 0 4 5 6 6 7 4 8 9 10 10 11 8 12 13 14 14 15 12 16 17 18 18 19 16 20 21 22 22 23 20)
  (attribs :pos (-0.5 +0.5 -0.5) :normal (0.0 0.0 -1.0) :uv (+0.0 +1.0))
  (attribs :pos (-0.5 -0.5 -0.5) :normal (0.0 0.0 -1.0) :uv (+0.0 +0.0))
  (attribs :pos (+0.5 -0.5 -0.5) :normal (0.0 0.0 -1.0) :uv (+1.0 +0.0))
  (attribs :pos (+0.5 +0.5 -0.5) :normal (0.0 0.0 -1.0) :uv (+1.0 +1.0))

  (attribs :pos (-0.5 +0.5 +0.5) :normal (0.0 0.0 1.0) :uv (+0.0 +1.0))
  (attribs :pos (-0.5 -0.5 +0.5) :normal (0.0 0.0 1.0) :uv (+0.0 +0.0))
  (attribs :pos (+0.5 -0.5 +0.5) :normal (0.0 0.0 1.0) :uv (+1.0 +0.0))
  (attribs :pos (+0.5 +0.5 +0.5) :normal (0.0 0.0 1.0) :uv (+1.0 +1.0))

  (attribs :pos (-0.5 -0.5 +0.5) :normal (-1.0 0.0 0.0) :uv (+0.0 +1.0))
  (attribs :pos (-0.5 -0.5 -0.5) :normal (-1.0 0.0 0.0) :uv (+0.0 +0.0))
  (attribs :pos (-0.5 +0.5 -0.5) :normal (-1.0 0.0 0.0) :uv (+1.0 +0.0))
  (attribs :pos (-0.5 +0.5 +0.5) :normal (-1.0 0.0 0.0) :uv (+1.0 +1.0))

  (attribs :pos (+0.5 -0.5 -0.5) :normal (1.0 0.0 0.0) :uv (+0.0 +1.0))
  (attribs :pos (+0.5 -0.5 +0.5) :normal (1.0 0.0 0.0) :uv (+0.0 +0.0))
  (attribs :pos (+0.5 +0.5 +0.5) :normal (1.0 0.0 0.0) :uv (+1.0 +0.0))
  (attribs :pos (+0.5 +0.5 -0.5) :normal (1.0 0.0 0.0) :uv (+1.0 +1.0))

  (attribs :pos (-0.5 -0.5 +0.5) :normal (0.0 -1.0 0.0) :uv (+0.0 +1.0))
  (attribs :pos (+0.5 -0.5 +0.5) :normal (0.0 -1.0 0.0) :uv (+0.0 +0.0))
  (attribs :pos (+0.5 -0.5 -0.5) :normal (0.0 -1.0 0.0) :uv (+1.0 +0.0))
  (attribs :pos (-0.5 -0.5 -0.5) :normal (0.0 -1.0 0.0) :uv (+1.0 +1.0))

  (attribs :pos (-0.5 +0.5 +0.5) :normal (0.0 1.0 0.0) :uv (+0.0 +1.0))
  (attribs :pos (+0.5 +0.5 +0.5) :normal (0.0 1.0 0.0) :uv (+0.0 +0.0))
  (attribs :pos (+0.5 +0.5 -0.5) :normal (0.0 1.0 0.0) :uv (+1.0 +0.0))
  (attribs :pos (-0.5 +0.5 -0.5) :normal (0.0 1.0 0.0) :uv (+1.0 +1.0)))
