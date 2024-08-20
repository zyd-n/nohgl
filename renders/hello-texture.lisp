(in-package #:nohgl.hello-texture)

(defmethod init-options ()
  (gl:viewport 0 0 1920 1080)
  (gl:clear-color .09 .09 .09 0))

(defun start-texture ()
  (start 'hello-texture :title "nohgl - a texture" :width 900 :height 600))

(defvao 'v1
  :vertex-shader
  "#version 330 core

   layout (location = 0) in vec3 position;
   layout (location = 1) in vec3 color;
   layout (location = 2) in vec2 tex;

   out vec3 fcolor;
   out vec2 ftex;

   void main()
   {
     gl_Position = vec4(position, 1.0);
     fcolor = color;
     ftex = tex;
   }"
  :fragment-shader
  "#version 330 core

   out vec4 FragColor;

   in vec3 fcolor;
   in vec2 ftex;

   uniform sampler2D uftex;

   void main()
   {
     FragColor = texture(uftex, ftex) * vec4(fcolor, 1.0);
   }"
  :verts (gfill :float
                ;;  <pos>        <color>        <texture>
                ;; ---------------------------------------
                +0.5 +0.5 +0.0   1.0 0.0 0.0    1.0 1.0
                +0.5 -0.5 +0.0   0.0 1.0 0.0    1.0 0.0
                -0.5 -0.5 +0.0   0.0 0.0 1.0    0.0 0.0
                -0.5 +0.5 +0.0   1.0 1.0 0.0    0.0 1.0)
  :indices (gfill :unsigned-int 0 1 3 1 2 3)
  :texture #p"assets/gang.png"
  :uniforms '("uftex"))


(defmethod format-vertex-attribs ()
  (let ((size-of-float (cffi:foreign-type-size :float)))
    (gl:vertex-attrib-pointer 0 3 :float :false (* 8 size-of-float) 0)
    (gl:enable-vertex-attrib-array 0)
    (gl:vertex-attrib-pointer 1 3 :float :false (* 8 size-of-float) (* 3 size-of-float))
    (gl:enable-vertex-attrib-array 1)
    (gl:vertex-attrib-pointer 2 2 :float :false (* 8 size-of-float) (* 6 size-of-float))
    (gl:enable-vertex-attrib-array 2)))

(defun draw-vertex (vao-store &optional (vertex-count 3) (offset 0))
  (gl:use-program (program (get-vao vao-store)))
  (gl:bind-vertex-array (vao (get-vao vao-store)))
  (%gl:draw-elements :triangles vertex-count :unsigned-int offset))

(define-render hello-texture ()
  (gl:clear :color-buffer)
  (with-uniform-location "uftex" 'v1
    (%gl:uniform-1i uniform-location 0))
  (gl:bind-texture :texture-2d (texture (get-vao 'v1)))
  (draw-vertex 'v1 6))
