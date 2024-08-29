(defpackage #:nohgl.3d
  (:use #:cl #:org.shirakumo.fraf.math #:nohgl)
  (:local-nicknames (#:glfw #:org.shirakumo.fraf.glfw))
  (:export #:start-render))

(in-package #:nohgl.3d)

;;; Initialization Options

(defmethod init-options ()
  (gl:viewport 0 0 800 600)
  (gl:clear-color .09 .09 .09 0)
  (gl:enable :depth-test))

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

(defmethod nohgl:register-texture-units ((vao-store store) (name (eql 'v1)))
  (with-slots (program) vao-store
    (%gl:uniform-1i (gl:get-uniform-location program "texture0") 0)
    (%gl:uniform-1i (gl:get-uniform-location program "texture1") 1)))

;;; Vertex Attribute Format

(defmethod format-vertex-attribs ()
  (let ((size-of-float (cffi:foreign-type-size :float)))
    (gl:vertex-attrib-pointer 0 3 :float :false (* 5 size-of-float) 0)
    (gl:enable-vertex-attrib-array 0)
    (gl:vertex-attrib-pointer 1 2 :float :false (* 5 size-of-float) (* 3 size-of-float))
    (gl:enable-vertex-attrib-array 1)))

;;; VAOs

(defvao 'v1
  :vertex-shader
  "#version 330 core

   layout (location = 0) in vec3 position;
   layout (location = 1) in vec2 itexture_coord;

   out vec2 otexture_coord;

   uniform mat4 model;
   uniform mat4 view;
   uniform mat4 projection;

   void main()
   {
     gl_Position = projection * view * model * vec4(position, 1.0);
     otexture_coord = vec2(itexture_coord.x, itexture_coord.y);
   }"
  :fragment-shader
  "#version 330 core

   out vec4 FragColor;

   in vec2 otexture_coord;

   uniform sampler2D texture0;
   uniform sampler2D texture1;

   void main()
   {
     FragColor = mix(texture(texture0, otexture_coord), texture(texture1, otexture_coord), 0.4);
   }"
  :verts
  (gfill :float
   -0.5 -0.5 -0.5 +0.0 +0.0
   +0.5 -0.5 -0.5 +1.0 +0.0
   +0.5 +0.5 -0.5 +1.0 +1.0
   +0.5 +0.5 -0.5 +1.0 +1.0
   -0.5 +0.5 -0.5 +0.0 +1.0
   -0.5 -0.5 -0.5 +0.0 +0.0

   -0.5 -0.5 +0.5 +0.0 +0.0
   +0.5 -0.5 +0.5 +1.0 +0.0
   +0.5 +0.5 +0.5 +1.0 +1.0
   +0.5 +0.5 +0.5 +1.0 +1.0
   -0.5 +0.5 +0.5 +0.0 +1.0
   -0.5 -0.5 +0.5 +0.0 +0.0

   -0.5 +0.5 +0.5 +1.0 +0.0
   -0.5 +0.5 -0.5 +1.0 +1.0
   -0.5 -0.5 -0.5 +0.0 +1.0
   -0.5 -0.5 -0.5 +0.0 +1.0
   -0.5 -0.5 +0.5 +0.0 +0.0
   -0.5 +0.5 +0.5 +1.0 +0.0

   +0.5 +0.5 +0.5 +1.0 +0.0
   +0.5 +0.5 -0.5 +1.0 +1.0
   +0.5 -0.5 -0.5 +0.0 +1.0
   +0.5 -0.5 -0.5 +0.0 +1.0
   +0.5 -0.5 +0.5 +0.0 +0.0
   +0.5 +0.5 +0.5 +1.0 +0.0

   -0.5 -0.5 -0.5 +0.0 +1.0
   +0.5 -0.5 -0.5 +1.0 +1.0
   +0.5 -0.5 +0.5 +1.0 +0.0
   +0.5 -0.5 +0.5 +1.0 +0.0
   -0.5 -0.5 +0.5 +0.0 +0.0
   -0.5 -0.5 -0.5 +0.0 +1.0

   -0.5 +0.5 -0.5 +0.0 +1.0
   +0.5 +0.5 -0.5 +1.0 +1.0
   +0.5 +0.5 +0.5 +1.0 +0.0
   +0.5 +0.5 +0.5 +1.0 +0.0
   -0.5 +0.5 +0.5 +0.0 +0.0
   -0.5 +0.5 -0.5 +0.0 +1.0)
  :indices
  (gfill :unsigned-int 0 1 3 1 2 3)
  :textures
  '(("container.png" container 2d-rgb)
    ("awesomeface.png" awesome-face 2d-rgba))
  :uniforms
  '("texture0" "texture1" "model" "view" "projection"))

;;; Render/Draw code

;; Super limited. We need a way to pass in texture units as proper objects
;; with all their needed information (e.g, :texture-2d).
(defun bind-textures (vao &rest textures)
  ;; KLUDGE: Without setting the package manually, for some reason we can't
  ;; intern keywords in the keyword package properly. Should investigate
  ;; *why*. Note, this does work without the manual package setting if we're
  ;; *in* the 3d package at the REPL. Very weird.
  (let ((*package* (find-package :nohgl.3d))
        (texture-count (length textures)))
    (if (> texture-count 16)
        (error "Maximum number of textures is 16, got ~s instead with object:~%~s"
               texture-count textures)
        (dotimes (n texture-count)
          (gl:active-texture (intern (format nil "~s~d" 'texture n)
                                     (find-package :keyword)))
          (gl:bind-texture :texture-2d (get-texture (elt textures n) vao))))))

(defun upload-uniforms (vao uniforms-with-data)
  (loop for (uniform data) in uniforms-with-data
        do (let ((uniform-location (gl:get-uniform-location (program (get-vao vao)) uniform)))
             (cffi:with-pointer-to-vector-data (ptr (marr data))
               (%gl:uniform-matrix-4fv uniform-location 1 :false ptr)))))

(defmacro uf-pairs (pairs)
  `(list ,@(loop for (name data) in pairs
                 collect `(list ,name ,data))))

;;; Render

;; Loads of API inconsistency. Sometimes we pass in just the symbol name of a
;; vao (e.g: 'v1), other time we pass in the vao object itself.
(define-render 3d ()
  (let* ((model (m* (meye 4) (mrotation (vec3 0.5 1.0 0.0) (* (glfw:time) 1.5))))
         (view (m* (meye 4) (mtranslation (vec3 0.0 0.0 -3.0))))
         (projection (mperspective 35.0 (/ 800 600) 0.1 100.0))
         (v (get-vao 'v1)))
    (gl:clear :color-buffer :depth-buffer)
    (bind-textures 'v1 'container 'awesome-face)
    (gl:use-program (program v))
    (upload-uniforms 'v1 (uf-pairs (("model" model) ("view" view) ("projection" projection))))
    (gl:bind-vertex-array (vao v))
    (gl:draw-arrays :triangles 0 36)))

;;; Start

(defun start-render ()
  (start '3d :title "nohgl - 3d" :width 800 :height 600))
