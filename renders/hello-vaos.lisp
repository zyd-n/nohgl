(in-package #:nohgl.hello-vaos)

;; This render draws two different VAO objects, along with a different
;; fragment shader, to the same context.

(defun start-vaos ()
  (start 'hello-vaos :title "nohgl - Hello VAOs" :width 900 :height 600))

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

(defvao 'v2
  :vertex-shader "shaders/hello.vert"
  :fragment-shader "shaders/v2.frag"
  :verts (gfill :float
                -0.2 -1.0 +0.0
                +0.2 -1.0 +0.0
                +0.0 +1.0 +0.0))

(defun draw-vertex (vao-store &optional (vertex-count 3) (offset 0))
  (gl:use-program (program (get-vao vao-store)))
  (gl:bind-vertex-array (vao (get-vao vao-store)))
  (gl:draw-arrays :triangles offset vertex-count))

(define-render hello-vaos ()
  (gl:clear :color-buffer)
  (draw-vertex 'v1 6)
  (draw-vertex 'v2)
  (glfw:swap-buffers *g*))
