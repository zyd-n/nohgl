(in-package #:nohgl.hello-shader)

(defun hello-shaders ()
  (start 'hello-shaders :title "Hello, Shaders!" :width 900 :height 600))

(defvao 'v1
  :vertex-shader "shaders/hello.vert"
  :fragment-shader "shaders/hello.frag"
  ;; :uniforms '("gScale")
  :verts (gfill :float 0.5 0.5 0.0 0.5 -0.5 0.0 -0.5 -0.5 0.0 -0.5 0.5 0.0)
  :indices (gfill :unsigned-int 0 1 3 1 2 3))

(defun draw-vertex (vao-store &optional (vertex-count 3) (offset 0))
  (gl:use-program (program (get-vao vao-store)))
  (gl:bind-vertex-array (vao (get-vao vao-store)))
  (%gl:draw-elements :triangles vertex-count :unsigned-int offset))

(define-render hello-shaders ()
  (gl:clear :color-buffer)
  (draw-vertex 'v1 6)
  (glfw:swap-buffers *g*))
