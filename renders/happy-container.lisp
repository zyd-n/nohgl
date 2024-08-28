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

   uniform mat4 transform;

   void main()
   {
     gl_Position = transform * vec4(position, 1.0);
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
     FragColor = mix(texture(texture0, ftex), texture(texture1, (vec2(1.0, 1.0) * ftex)), 0.9);
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
    ("awesomeface.png" awesome-face 2d-rgba)
    ("yuno.png" yuno 2d-rgb))
  :uniforms
  '("texture0" "texture1" "transform"))

;;; Render/Draw code

;; We should probably make `v' explicit: have the user pass in a name to
;; reference the store.
;;
;; (with-vao (foo-bar v1)
;;   (gl:do-some-stuff v1))
(defmacro with-vao (store &body body)
  `(let ((v (get-vao ',store)))
     (gl:use-program (program v))
     (gl:bind-vertex-array (vao v))
     ,@body))

(defun draw-vertex (vao-store &optional (vertex-count 3) (offset 0))
  (gl:use-program (program (get-vao vao-store)))
  (gl:bind-vertex-array (vao (get-vao vao-store)))
  (%gl:draw-elements :triangles vertex-count :unsigned-int offset))

(defmethod nohgl:register-texture-units ((vao-store store) (name (eql 'v1)))
  (with-slots (program) vao-store
    (%gl:uniform-1i (gl:get-uniform-location program "texture0") 0)
    (%gl:uniform-1i (gl:get-uniform-location program "texture1") 1)))

;; Really limited. We need a way to pass in texture units as proper objects
;; with all their needed information (e.g, :texture-2d).
(defun bind-textures (vao &rest textures)
  (let ((texture-count (length textures)))
    (if (> texture-count 16)
        (error "Maximum number of textures is 16, got ~s instead with object:~%~s"
               texture-count textures)
        (dotimes (n texture-count)
          (gl:active-texture (intern (format nil "~s~d" 'texture n)
                                     (find-package :keyword)))
          (gl:bind-texture :texture-2d (get-texture (elt textures n) vao))))))

(define-render happy-container ()
  (let* ((axis (vec3 0.0 0.0 1.0))
         (scale (vec3 0.5 -0.5 0.0))
         (angle (glfw:time))
         (transform (meye 4)))
    (gl:clear :color-buffer)
    (bind-textures 'v1 'container 'yuno)
    (with-vao v1
      (cffi:with-pointer-to-vector-data (p (marr (nmscale (nmrotate transform axis angle) scale)))
        (with-uniform-location "transform" 'v1
          (%gl:uniform-matrix-4fv uniform-location 1 :false p)))
      (%gl:draw-elements :triangles 6 :unsigned-int 0))))

;;; Start

(defun start-render ()
  (start 'happy-container :title "nohgl - awesome" :width 1920 :height 1080))
