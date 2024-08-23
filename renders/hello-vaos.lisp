(defpackage #:nohgl.hello-vaos
  (:use #:cl #:org.shirakumo.fraf.math #:nohgl)
  (:local-nicknames (#:glfw #:org.shirakumo.fraf.glfw))
  (:export #:start-render))

(in-package #:nohgl.hello-vaos)

;; This render draws two different VAO objects, along with a different
;; fragment shader, to the same context.

(defmethod init-options ()
  (gl:viewport 0 0 900 600)
  (gl:clear-color .09 .09 .09 0))

(defmethod format-vertex-attribs ()
  (default-format))

(defvao 'v1
  :vertex-shader "#version 330 core

layout (location = 0) in vec3 Position;

void main()
{
  gl_Position = vec4(Position.x, Position.y, Position.z, 1.0);
}
"
  :fragment-shader "#version 330 core

out vec4 FragColor;

void main()
{
  FragColor = vec4(1.0f, 0.0f, 0.0f, 0.0f);
}
"
  :verts (gfill :float
                -1.0 +1.0 +0.0
                -1.0 -1.0 +0.0
                +0.0 +1.0 +0.0
                +0.0 +1.0 +0.0
                +1.0 -1.0 +0.0
                +1.0 +1.0 +0.0))

(defvao 'v2
  :vertex-shader (shader-s "hello.vert")
  :fragment-shader (shader-s "v2.frag")
  :verts (gfill :float
                -0.2 +1.0 +0.0
                -0.6 -0.9 +0.0
                +1.0 -1.0 +0.0))

(defun draw-vertex (vao-store &optional (vertex-count 3) (offset 0))
  (gl:use-program (program (get-vao vao-store)))
  (gl:bind-vertex-array (vao (get-vao vao-store)))
  (gl:draw-arrays :triangles offset vertex-count))

(define-render hello-vaos ()
  (gl:clear :color-buffer)
  (draw-vertex 'v1 6)
  (draw-vertex 'v2))

(defun start-render ()
  (start 'hello-vaos :title "nohgl - Hello VAOs" :width 900 :height 600))
