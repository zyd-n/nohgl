(in-package #:nohgl.two-triangles)

(defun start-triangles ()
  (start 'two-triangles :title "nohgl - Two Triangles" :width 900 :height 600))

(defvao 'v1
  :vertex-shader "shaders/hello.vert"
  :fragment-shader "shaders/hello.frag"
  :verts (gfill :float
                -1.0 +1.0 +0.0
                -1.0 -1.0 +0.0
                +0.0 +1.0 +0.0
                +0.0 +1.0 +0.0
                +1.0 -1.0 +0.0
                +1.0 +1.0 +0.0))

(defun draw-vertex (vao-store &optional (vertex-count 3) (offset 0))
  (gl:use-program (program (get-vao vao-store)))
  (gl:bind-vertex-array (vao (get-vao vao-store)))
  (gl:draw-arrays :triangles offset vertex-count))

(define-render two-triangles ()
  (gl:clear :color-buffer)
  (draw-vertex 'v1 6)
  (glfw:swap-buffers *g*))
