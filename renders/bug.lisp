(in-package #:nohgl.bug)

(defun start-bug ()
  (start 'bug :title "nohgl - buggy C-c C-c" :width 900 :height 600))

(defun draw-vertex (vao-store &optional (vertex-count 3) (offset 0))
  (gl:use-program (program (get-vao vao-store)))
  (gl:bind-vertex-array (vao (get-vao vao-store)))
  (gl:draw-arrays :triangles offset vertex-count))

(define-render bug ()
  (gl:clear :color-buffer)
  (draw-vertex 'v1 6)
  (glfw:swap-buffers *g*))

;;; Red

(defvao 'v1
  :vertex-shader "shaders/hello.vert"
  :fragment-shader "shaders/red.frag"
  :verts (gfill :float
                -1.0 +1.0 +0.0
                -1.0 -1.0 +0.0
                +0.0 +1.0 +0.0
                +0.0 +1.0 +0.0
                +1.0 -1.0 +0.0
                +1.0 +1.0 +0.0))

;;; Yellow

(defvao 'v1
  :vertex-shader "shaders/hello.vert"
  :fragment-shader "shaders/yellow.frag"
  :verts (gfill :float
                -1.0 +1.0 +0.0
                -1.0 -1.0 +0.0
                +0.0 +1.0 +0.0
                +0.0 +1.0 +0.0
                +1.0 -1.0 +0.0
                +1.0 +1.0 +0.0))

;;; Blue

(defvao 'v1
  :vertex-shader "shaders/hello.vert"
  :fragment-shader "shaders/blue.frag"
  :verts (gfill :float
                -1.0 +1.0 +0.0
                -1.0 -1.0 +0.0
                +0.0 +1.0 +0.0
                +0.0 +1.0 +0.0
                +1.0 -1.0 +0.0
                +1.0 +1.0 +0.0))
