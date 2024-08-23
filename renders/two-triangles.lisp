(defpackage #:nohgl.two-triangles
  (:use #:cl #:org.shirakumo.fraf.math #:nohgl)
  (:local-nicknames (#:glfw #:org.shirakumo.fraf.glfw))
  (:export #:start-render))

(in-package #:nohgl.two-triangles)

;; This render draws two triangles using the same VAO but simply with more
;; vertices (as opposed to two VAOs)

(defvao 'v1
  :vertex-shader (shader-s "hello.vert")
  :fragment-shader (shader-s "hello.frag")
  :verts (gfill :float
                -1.0 +1.0 +0.0
                -1.0 -1.0 +0.0
                +0.0 +1.0 +0.0
                +0.0 +1.0 +0.0
                +1.0 -1.0 +0.0
                +1.0 +1.0 +0.0))

(defmethod init-options ()
  (gl:viewport 0 0 900 600)
  (gl:clear-color .09 .09 .09 0))

(defmethod format-vertex-attribs ()
  (default-format))

(defun draw-vertex (vao-store &optional (vertex-count 3) (offset 0))
  (gl:use-program (program (get-vao vao-store)))
  (gl:bind-vertex-array (vao (get-vao vao-store)))
  (gl:draw-arrays :triangles offset vertex-count))

(define-render two-triangles ()
  (gl:clear :color-buffer)
  (draw-vertex 'v1 6))

(defun start-render ()
  (start 'two-triangles :title "nohgl - Two Triangles" :width 900 :height 600))
