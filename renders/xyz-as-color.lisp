(defpackage #:nohgl.xyz-as-color
  (:use #:cl #:org.shirakumo.fraf.math #:nohgl)
  (:local-nicknames (#:glfw #:org.shirakumo.fraf.glfw))
  (:export #:start-render))

(in-package #:nohgl.xyz-as-color)

(defvao 'v1
  :vertex-shader
  "#version 330 core

   layout (location = 0) in vec3 position;

   out vec4 vertexPos;

   void main()
   {
     gl_Position = vec4(position, 1.0);
     vertexPos = gl_Position;
   }"
  :fragment-shader
  "#version 330 core

   out vec4 FragColor;
   in vec4 vertexPos;

   void main()
   {
     FragColor = vertexPos;
   }"
  :verts
  (gfill :float
   +0.0 +1.0 +0.0
   -1.0 -1.0 +0.0
   +1.0 -1.0 +0.0))

(defmethod init-options ()
  (gl:viewport 225 150 450 300)
  (gl:clear-color .09 .09 .09 0))

(defmethod format-vertex-attribs ()
  (let ((size-of-float (cffi:foreign-type-size :float)))
    (gl:vertex-attrib-pointer 0 3 :float :false (* 3 size-of-float) 0)
    (gl:enable-vertex-attrib-array 0)))

(defun draw-vertex (vao-store &optional (vertex-count 3) (offset 0))
  (gl:use-program (program (get-vao vao-store)))
  (gl:bind-vertex-array (vao (get-vao vao-store)))
  (gl:draw-arrays :triangles offset vertex-count))

(define-render xyz ()
  (gl:clear :color-buffer)
  (draw-vertex 'v1 3))

(defun start-render ()
  (start 'xyz :title "nohgl - Positions (xyzw) as color" :width 900 :height 600))
