(defpackage #:nohgl.rectangle
  (:use #:cl #:org.shirakumo.fraf.math #:nohgl)
  (:local-nicknames (#:glfw #:org.shirakumo.fraf.glfw))
  (:export #:start-render))

(in-package #:nohgl.rectangle)

;; This render draws two triangles (that form a rectangle) using indices to
;; control which set of vertices are drawn.

(defmethod init-options ()
  (gl:viewport 0 0 900 600)
  (gl:clear-color .09 .09 .09 0))

(defmethod format-vertex-attribs ()
  (default-format))

(defvao 'v1
  :vertex-shader
  (shader-s "hello.vert")
  :fragment-shader
  (shader-s "hello.frag")
  :verts
  (gfill :float
   +0.5 +0.5 +0.0
   +0.5 -0.5 +0.0
   -0.5 -0.5 +0.0
   -0.5 +0.5 +0.0)
  :indices
  (gfill :unsigned-int 0 1 3 1 2 3))

(defun draw-vertex (vao-store &optional (vertex-count 3) (offset 0))
  (gl:use-program (program (get-vao vao-store)))
  (gl:bind-vertex-array (vao (get-vao vao-store)))
  (%gl:draw-elements :triangles vertex-count :unsigned-int offset))

(define-render rectangle ()
  (gl:clear :color-buffer)
  (draw-vertex 'v1 6))

(defun start-render ()
  (start 'rectangle :title "nohgl - A basic rectangle" :width 900 :height 600))
