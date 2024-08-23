(defpackage #:nohgl.bocchi
  (:use #:cl #:org.shirakumo.fraf.math #:nohgl)
  (:local-nicknames (#:glfw #:org.shirakumo.fraf.glfw))
  (:export #:start-render))

(in-package #:nohgl.bocchi)

;;; Initialization Options

(defmethod init-options ()
  (gl:viewport 0 0 1920 1080)
  (gl:clear-color .09 .09 .09 0))

;;; Texture Formats

(define-texture-format 2d-rgba (width height data)
  (let ((texture-id (gl:gen-texture)))
    (gl:bind-texture :texture-2d texture-id)
    (gl:tex-parameter :texture-2d :texture-wrap-s :repeat)
    (gl:tex-parameter :texture-2d :texture-wrap-t :repeat)
    (gl:tex-parameter :texture-2d :texture-min-filter :linear-mipmap-linear)
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

   uniform sampler2D uftex;

   void main()
   {
     FragColor = texture(uftex, ftex) * vec4(fcolor, 1.0);
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
  '(("bag.png" bag 2d-rgb)
    ("in-the-bag.png" in-the-bag 2d-rgb))
  :uniforms
  '("uftex"))

;;; Render/Draw code

(defun draw-vertex (vao-store &optional (vertex-count 3) (offset 0))
  (gl:use-program (program (get-vao vao-store)))
  (gl:bind-vertex-array (vao (get-vao vao-store)))
  (%gl:draw-elements :triangles vertex-count :unsigned-int offset))

(defun cycle-textures (textures)
  (let ((popped (pop textures)))
    (append textures `(,popped))))

(defun current-texture (textures)
  (first textures))

(defmacro increment-frame (n)
  `(incf ,n))

(define-render bocchi
   ((texture-pool '(in-the-bag bag))
    (texture-frames 0))
  (labels ((cycle-texture (&key (frames 0))
             (when (>= texture-frames frames)
               (shiftf texture-pool (cycle-textures texture-pool))
               (setf texture-frames 0))))
    (cycle-texture :frames 33)
    (gl:clear :color-buffer)
    (with-uniform-location "uftex" 'v1
      (%gl:uniform-1i uniform-location 0))
    (gl:bind-texture :texture-2d (get-texture (current-texture texture-pool) 'v1))
    (draw-vertex 'v1 6)
    (increment-frame texture-frames)))

;;; Start

(defun start-render ()
  (start 'bocchi :title "nohgl - a bocchi the rock horror story" :width 1920 :height 1080))
