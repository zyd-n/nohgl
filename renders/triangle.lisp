(in-package #:nohgl.triangle)

;; This render draws a basic triangle.

(defun start-triangle ()
  (start 'triangle :title "nohgl - A basic triangle" :width 900 :height 600))

(defvao 'v1
  :vertex-shader (pathname "shaders/hello.vert")
  :fragment-shader (pathname "shaders/hello.frag")
  :verts (gfill :float
                +0.0 +1.0 +0.0
                -1.0 -1.0 +0.0
                +1.0 -1.0 +0.0))

(defun draw-vertex (vao-store &optional (vertex-count 3) (offset 0))
  (gl:use-program (program (get-vao vao-store)))
  (gl:bind-vertex-array (vao (get-vao vao-store)))
  (gl:draw-arrays :triangles offset vertex-count))

(define-render triangle ()
  (gl:clear :color-buffer)
  (draw-vertex 'v1 3)
  (glfw:swap-buffers *g*))
