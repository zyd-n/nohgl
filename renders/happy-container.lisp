(defpackage #:nohgl.happy-container
  (:use #:cl #:org.shirakumo.fraf.math #:nohgl)
  (:local-nicknames (#:glfw #:org.shirakumo.fraf.glfw))
  (:export #:start-render))

(in-package #:nohgl.happy-container)

;;; Initialization Options

(defmethod init-options ()
  (gl:viewport 0 0 1920 1080)
  (gl:clear-color .09 .09 .09 0))

;;; Texture Formats

(define-texture-format 2d-rgba (width height data)
  (let ((texture-id (gl:gen-texture)))
    (gl:bind-texture :texture-2d texture-id)
    (gl:tex-parameter :texture-2d :texture-wrap-s :repeat)
    (gl:tex-parameter :texture-2d :texture-wrap-t :clamp-to-edge)
    (gl:tex-parameter :texture-2d :texture-min-filter :nearest)
    (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
    (gl:tex-image-2d :texture-2d 0 :rgb width height 0 :rgba :unsigned-byte data :raw t)
    (gl:generate-mipmap :texture-2d)
    texture-id))

(define-texture-format 2d-rgb (width height data)
  (let ((texture-id (gl:gen-texture)))
    (gl:bind-texture :texture-2d texture-id)
    (gl:tex-parameter :texture-2d :texture-wrap-s :repeat)
    (gl:tex-parameter :texture-2d :texture-wrap-t :repeat)
    (gl:tex-parameter :texture-2d :texture-min-filter :linear-mipmap-linear)
    (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
    (gl:tex-image-2d :texture-2d 0 :rgb width height 0 :rgb :unsigned-byte data :raw t)
    (gl:generate-mipmap :texture-2d)
    texture-id))

;;; Vertex Attribute Format

(defmethod format-vertex-attribs ()
  (let ((size-of-float (cffi:foreign-type-size :float)))
    (gl:vertex-attrib-pointer 0 3 :float :false (* 8 size-of-float) 0)
    (gl:enable-vertex-attrib-array 0)
    (gl:vertex-attrib-pointer 1 3 :float :false (* 8 size-of-float) (* 3 size-of-float))
    (gl:enable-vertex-attrib-array 1)
    (gl:vertex-attrib-pointer 2 2 :float :false (* 8 size-of-float) (* 6 size-of-float))
    (gl:enable-vertex-attrib-array 2)))

;;; VAOs

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

   uniform sampler2D texture0;
   uniform sampler2D texture1;

   void main()
   {

     // We can change the x value of the vec2 to -1 to horizontally flip/reverse
     // 'texture1', aka awesomeface.
     FragColor = mix(texture(texture0, ftex), texture(texture1, (vec2(-1.0, 1.0) * ftex)), 0.4);
   }"
  :verts
  (gfill :float
   ;;  <pos>        <color>        <texture>
   ;; ---------------------------------------
   +0.5 +0.5 +0.0   1.0 0.0 0.0    1.0 1.0
   +0.5 -0.5 +0.0   0.0 1.0 0.0    1.0 0.0
   -0.5 -0.5 +0.0   0.0 0.0 1.0    0.0 0.0
   -0.5 +0.5 +0.0   1.0 1.0 0.0    0.0 1.0)
  :indices
  (gfill :unsigned-int 0 1 3 1 2 3)
  :textures
  '(("container.png" container 2d-rgb)
    ("awesomeface.png" awesome-face 2d-rgba))
  :uniforms
  '("texture0" "texture1"))

;;; Render/Draw code

(defun draw-vertex (vao-store &optional (vertex-count 3) (offset 0))
  (gl:use-program (program (get-vao vao-store)))
  (gl:bind-vertex-array (vao (get-vao vao-store)))
  (%gl:draw-elements :triangles vertex-count :unsigned-int offset))

(defmethod nohgl:register-texture-units ((vao-store store) (name (eql 'v1)))
  (with-slots (program) vao-store
    (%gl:uniform-1i (gl:get-uniform-location program "texture0") 0)
    (%gl:uniform-1i (gl:get-uniform-location program "texture1") 1)))

(define-render happy-container
   ((y 1.0)
    (grow t))
  (let ((v (varr (vec2 1.0 y))))
    (gl:clear :color-buffer)
    (gl:active-texture :texture0)
    (gl:bind-texture :texture-2d (get-texture 'container 'v1))
    (gl:active-texture :texture1)
    (gl:bind-texture :texture-2d (get-texture 'awesome-face 'v1))
    (cffi:with-pointer-to-vector-data (p v)
      (with-uniform-location "stretch" 'v1
        (%gl:uniform-2fv uniform-location 1 p)))
    (draw-vertex 'v1 6)))

;;; Start

(defun start-render ()
  (start 'happy-container :title "nohgl - awesome" :width 1920 :height 1080))
