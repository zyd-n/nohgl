(in-package #:nohgl.rectangle)

;; This render draws two triangles (that form a rectangle) using indices to
;; control which set of vertices are drawn.

(defun start-rectangle ()
  (start 'rectangle :title "nohgl - A basic rectangle" :width 900 :height 600))

(defvao 'v1
  :vertex-shader (pathname "shaders/hello.vert")
  :fragment-shader (pathname "shaders/hello.frag")
  :verts (gfill :float
                +0.5 +0.5 +0.0
                +0.5 -0.5 +0.0
                -0.5 -0.5 +0.0
                -0.5 +0.5 +0.0)
  :indices (gfill :unsigned-int 0 1 3 1 2 3))

(defun draw-vertex (vao-store &optional (vertex-count 3) (offset 0))
  (gl:use-program (program (get-vao vao-store)))
  (gl:bind-vertex-array (vao (get-vao vao-store)))
  (%gl:draw-elements :triangles vertex-count :unsigned-int offset))

(define-render rectangle ()
  (gl:clear :color-buffer)
  (draw-vertex 'v1 6)
  (glfw:swap-buffers *g*))
