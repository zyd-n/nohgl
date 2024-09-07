(defpackage #:nohgl.circular-yunos
  (:use #:cl #:org.shirakumo.fraf.math #:nohgl)
  (:local-nicknames (#:glfw #:org.shirakumo.fraf.glfw))
  (:export #:start-render))

(in-package #:nohgl.circular-yunos)

;;; Initialization Options

(defmethod init-options ()
  (let ((camera (camera (current-context))))
    (gl:viewport 0 0 (glfw:width (current-context)) (glfw:height (current-context)))
    (gl:clear-color .09 .09 .09 0)
    (gl:enable :depth-test)
    (setf (mouse-x camera) (/ (glfw:width (current-context)) 2))
    (setf (mouse-y camera) (/ (glfw:height (current-context)) 2))
    ;; TODO: Implement a way to toggle the cursor. Perhaps a double click?
    (setf (glfw:input-mode :cursor (current-context)) :cursor-disabled)))

;;; Texture Formats

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

;; TODO: Write elisp function to quickly change shader from its string form to
;; (shader-s ...) with a file. We prefer strings when messing with shader code
;; a lot but we don't need them as strings once we've settled on something.

;; TODO: Write elisp function to recompile shader/render when in a shader file
;; so its at least somewhat tolerable when interactively writing a shader from
;; a file rather than string.
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
     FragColor = mix(texture(texture0, otexture_coord), texture(texture1, otexture_coord), 0.5);
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
    ("yuno.png" yuno 2d-rgb))
  :uniforms
  '("texture0" "texture1" "model" "view" "projection"))

;;; Render Helpers

;; Super limited. We need a way to pass in texture units as proper objects
;; with all their needed information (e.g, :texture-2d).
(defun bind-textures (vao &rest textures)
  ;; KLUDGE: Without setting the package manually, for some reason we can't
  ;; intern keywords in the keyword package properly. Should investigate
  ;; *why*. Note, this does work without the manual package setting if we're
  ;; *in* the circular-yunos package at the REPL. Very weird.
  (let ((*package* (find-package :nohgl.circular-yunos))
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

(defun rotate-yunos (positions &key (only-some nil))
  (dotimes (i (length positions))
    (let* ((identity-matrix (meye 4))
           (translation-matrix (mtranslation (elt positions i)))
           (rotation-angle (and only-some (if (zerop (mod i 2)) (glfw:time) 1.0)))
           (rotation-matrix (mrotation (vec3 1.0 0.3 0.5)
                                       (* (radian 37.0)
                                          (if (zerop i)
                                              (+ 0.9 1.0)
                                              (+ 0.9 i))
                                          (or rotation-angle (glfw:time)))))
           (model (m* identity-matrix translation-matrix rotation-matrix)))
      (upload-uniforms 'v1 (uf-pairs (("model" model))))
      (gl:draw-arrays :triangles 0 36))))

(defun vec3r (dt count &optional (radius 0.1) (just-update t))
  (let ((vecs '()))
    (dotimes (n count vecs)
      (unless just-update
        (incf dt 0.796))
      (push (vec3 (* radius (cos dt))
                  (* radius (sin dt))
                  0.0)
            vecs))))

(defparameter *yuno-positions*
  (vec3r 0.01 8))

;; These radius macros would be better suited as local macros but it would
;; clutter the body of our render loop. Hence why these `top-level' macros
;; reference symbols only bound in the body of our render.

(defmacro update-radius-direction (radius)
  `(progn
     (when (>= ,radius 4.9)
       (setf radius-direction :negative))
     (when (<= radius 1.5)
       (setf radius-direction :positive))))

(defun radius-direction+ (direction)
  (if (eq :negative direction) NIL T))

(defmacro radius+ (n)
  `(incf radius ,n))

(defmacro radius- (n)
  `(decf radius ,n))

(defun update-yuno-distances (yunos)
  (setf *yuno-positions* yunos))

;; TODO: Rename to something better
(defmacro stepn+ (n)
  `(incf stepn ,n))

(defun update-camera ()
  (with-accessors ((position camera-position) (target camera-target) (speed camera-speed) (up camera-up))
      (camera (current-context))
    (loop for key in *key-stack*
          do (case key
               (:w (setf position (v+ position (v* target (* (dt) speed)))))
               (:a (setf position (v- position (v* (vunit (vc target up)) (* (dt) speed)))))
               (:s (setf position (v- position (v* target (* (dt) speed)))))
               (:d (setf position (v+ position (v* (vunit (vc target up)) (* (dt) speed)))))))))

;;; Render

;; Loads of API inconsistency. Sometimes we pass in just the symbol name of a
;; vao (e.g: 'v1), others times vao object itself, and still others we use a
;; getter: (get-vao 'v1)

(define-render circular-yunos
   ((stepn 0.0)
    (radius 0.1)
    (radius-direction :positive) )
  (with-slots (camera-position camera-target camera-up) (camera (current-context))
    (let ((view (mlookat camera-position (v+ camera-position camera-target) camera-up))
          (projection (mperspective 35.0 (/ (glfw:width *g*) (glfw:height *g*)) 0.1 100.0)))
      (gl:clear :color-buffer :depth-buffer)
      (bind-textures 'v1 'container 'yuno)
      (gl:use-program (program (get-vao 'v1)))
      (upload-uniforms 'v1 (uf-pairs (("view" view) ("projection" projection))))
      (update-radius-direction radius)
      (rotate-yunos *yuno-positions*)
      (rotate-yunos (list (vec3 +0.0 +0.0 +0.0)))
      (stepn+ 0.01)
      (update-yuno-distances (vec3r stepn 8 radius nil))
      (gl:bind-vertex-array (vao (get-vao 'v1)))
      (if (radius-direction+ radius-direction)
          (radius+ 0.01)
          (radius- 0.01))
      (update-camera)
      (maybe-double-click))))

;;; Start

(defun start-render ()
  (start 'circular-yunos :title "nohgl - circular yunos" :width 800 :height 600))
