(in-package #:nohgl.s5)

(defmethod init-options ()
  (gl:viewport 225 150 450 300)
  (gl:clear-color .09 .09 .09 0))

(defun start-s5 ()
  (start 's5 :title "nohgl - s5" :width 900 :height 600))

(defvao 'v1
  :vertex-shader (pathname "shaders/s5.vert")
  :fragment-shader (pathname "shaders/s5.frag")
  :verts (gfill :float
                ;;   <pos>          <color>
                +0.0 +1.0 +0.0  +1.0 +0.0 +0.0
                -1.0 -1.0 +0.0  +0.0 +1.0 +0.0
                +1.0 -1.0 +0.0  +0.0 +0.0 +1.0))

(defmethod format-vertex-attribs ()
  (let ((size-of-float (cffi:foreign-type-size :float)))
    (gl:vertex-attrib-pointer 0 3 :float :false (* 6 size-of-float) 0)
    (gl:enable-vertex-attrib-array 0)
    (gl:vertex-attrib-pointer 1 3 :float :false (* 6 size-of-float) (* 3 size-of-float))
    (gl:enable-vertex-attrib-array 1)))

(defun draw-vertex (vao-store &optional (vertex-count 3) (offset 0))
  (gl:use-program (program (get-vao vao-store)))
  (gl:bind-vertex-array (vao (get-vao vao-store)))
  (gl:draw-arrays :triangles offset vertex-count))

(define-render s5 ()
  (gl:clear :color-buffer)
  (draw-vertex 'v1 3))
