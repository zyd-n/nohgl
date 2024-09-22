(defpackage #:nohgl.basic-lighting
  (:use #:cl #:org.shirakumo.fraf.math #:nohgl)
  (:local-nicknames (#:glfw #:org.shirakumo.fraf.glfw))
  (:export #:start-render))

(in-package #:nohgl.basic-lighting)

;;; Initialization Options

(defmethod init-options ()
  (let ((camera (camera (current-context))))
    (gl:viewport 0 0 (glfw:width (current-context)) (glfw:height (current-context)))
    (gl:clear-color .09 .09 .09 0)
    (gl:enable :depth-test)
    (setf (mouse-x camera) (/ (glfw:width (current-context)) 2))
    (setf (mouse-y camera) (/ (glfw:height (current-context)) 2))
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

(defmethod nohgl:format-vertex-attribs ()
  (let ((size-of-float (cffi:foreign-type-size :float)))
    ;; position
    (gl:vertex-attrib-pointer 0 3 :float :false (* 8 size-of-float) 0)
    (gl:enable-vertex-attrib-array 0)
    ;; normal
    (gl:vertex-attrib-pointer 1 3 :float :false (* 8 size-of-float) (* size-of-float 3))
    (gl:enable-vertex-attrib-array 1)
    ;; texture
    (gl:vertex-attrib-pointer 2 2 :float :false (* 8 size-of-float) (* size-of-float 6))
    (gl:enable-vertex-attrib-array 2)))

;;; VAOs

;; Below demonstrates a need to share shaders across VAOs.

(defvao 'v1
  :vertex-shader
  "#version 330 core

   layout (location = 0) in vec3 iPos;
   layout (location = 1) in vec3 iNormal;
   layout (location = 2) in vec2 iTexture;

   uniform mat4 model;
   uniform mat4 view;
   uniform mat4 projection;
   // uniform vec3 lightPos;

   out vec3 FragPos;
   out vec3 Normal;
   // out vec3 LightPos;

   void main()
   {
     FragPos = vec3(model * vec4(iPos, 1.0));
     Normal = iNormal;
     gl_Position = projection * view * model * vec4(iPos, 1.0);


     // LightPos = vec3(view * vec4(lightPos, 1.0));
   }"
  :fragment-shader
  "#version 330 core

   in vec3 FragPos;
   in vec3 Normal;
   // in vec3 LightPos;

   uniform vec3 objectColor;
   uniform vec3 lightColor;
   uniform vec3 lightPos;

   out vec4 FragColor;

   void main()
   {

     // Ambient
     float ambientStrength = 0.1;
     vec3 ambient = ambientStrength * lightColor;

     // Diffuse
     vec3 norm = normalize(Normal);
     vec3 lightDir = normalize(lightPos - FragPos);
     float diff = max(dot(norm, lightDir), 0.0);
     //float diff = dot(norm, lightDir);
     vec3 diffuse = diff * lightColor;

     vec3 result = (ambient + diffuse) * objectColor;
     FragColor = vec4(result, 1.0);
   }"
  :verts (make-instance 'cube)
  :uniforms '("model" "view" "projection" "objectColor" "lightColor" "lightPos"))

(defvao 'floor
  :vertex-shader
  "#version 330 core

   layout (location = 0) in vec3 position;
   layout (location = 1) in vec3 normal;
   layout (location = 2) in vec2 uv;

   uniform mat4 model;
   uniform mat4 view;
   uniform mat4 projection;

   out vec2 UV;
   out vec3 FragPos;
   out vec3 Normal;

   void main()
   {
     FragPos = vec3(model * vec4(position, 1.0));
     Normal = normal;
     UV = vec2(uv.x, uv.y);
     gl_Position = projection * view * model * vec4(position, 1.0);
   }"
  :fragment-shader
  "#version 330 core

   out vec4 FragColor;

   in vec2 UV;
   in vec3 FragPos;
   in vec3 Normal;

   uniform float r;
   uniform float g;
   uniform float b;

   uniform vec3 objectColor;
   uniform vec3 lightColor;
   uniform vec3 lightPos;

   void main()
   {
     // Ambient
     float ambientStrength = 0.1;
     vec3 ambient = ambientStrength * lightColor;

     // Diffuse
     vec3 norm = normalize(Normal);
     vec3 lightDir = normalize(lightPos - FragPos);
     float diff = max(dot(norm, lightDir), 0.0);
     //float diff = dot(norm, lightDir);
     vec3 diffuse = diff * lightColor;

     vec3 result = (ambient + diffuse) * objectColor;
     // FragColor = vec4(result, 1.0);
     FragColor = vec4(vec3(r, g, b) * result, 0.0f);
   }"
  :verts (make-instance 'half-size-upfacing-plane)
  :uniforms '("model" "view" "projection" "r" "g" "b" "objectColor" "lightColor" "lightPos"))

(defvao 'light
  :vertex-shader
  "#version 330 core

   layout (location = 0) in vec3 position;

   uniform mat4 model;
   uniform mat4 view;
   uniform mat4 projection;

   void main()
   {
     gl_Position = projection * view * model * vec4(position, 1.0);
   }"
  :fragment-shader
  "#version 330 core

   out vec4 FragColor;

   void main()
   {
     FragColor = vec4(1.0); // set all 4 vector values to 1.0
   }"
  :verts (make-instance 'cube)
  :uniforms '("model" "view" "projection"))

;;; Render Helpers

(defun bind-textures (vao &rest textures)
  (let ((*package* (find-package :nohgl.basic-lighting))
        (texture-count (length textures)))
    (if (> texture-count 16)
        (error "Maximum number of textures is 16, got ~s instead with object:~%~s"
               texture-count textures)
        (dotimes (n texture-count)
          (gl:active-texture (intern (format nil "~s~d" 'texture n)
                                     (find-package :keyword)))
          (gl:bind-texture :texture-2d (id (get-texture (elt textures n) vao)))))))

(defun upload-uniforms (vao uniforms-with-data)
  (loop for (uniform data) in uniforms-with-data
        do (let ((uniform-location (gl:get-uniform-location (program (get-vao vao)) uniform)))
             (typecase data
               (float (gl:uniformf uniform-location data))
               (vec3 (cffi:with-pointer-to-vector-data (ptr (varr data))
                       (%gl:uniform-3fv uniform-location 1 ptr)))
               (mat4 (cffi:with-pointer-to-vector-data (ptr (marr data))
                       (%gl:uniform-matrix-4fv uniform-location 1 :false ptr)))))))

(defmacro uf-pairs (pairs)
  `(list ,@(loop for (name data) in pairs
                 collect `(list ,name ,data))))

(defun update-camera ()
  (with-accessors ((position camera-position) (target camera-target) (speed camera-speed) (up camera-up))
      (camera (current-context))
    (loop for key in (nohgl:input-stack)
          do (case key
               (:w (setf position (v+ position (v* target (* (dt) speed)))))
               (:a (setf position (v- position (v* (vunit (vc target up)) (* (dt) speed)))))
               (:s (setf position (v- position (v* target (* (dt) speed)))))
               (:d (setf position (v+ position (v* (vunit (vc target up)) (* (dt) speed)))))
               (:p (nohgl:print-camera))
               (:right (nohgl:print-camera))
               (:button-5 (nohgl:move-down))
               (:left-shift (nohgl:move-down))
               (:space (nohgl:move-up))))))

(defun make-checker-board (size colors)
  (labels ((draw (x z c)
             (let ((model (m* (meye 4) (mtranslation (vec3 (float x) -0.5009 (float z))))))
               (destructuring-bind (r g b) c
                 (upload-uniforms 'floor (uf-pairs (("model" model) ("r" r) ("g" g) ("b" b))))
                 (gl:bind-vertex-array (vao (get-vao 'floor)))
                 (%gl:draw-elements :triangles 6 :unsigned-int 0)))))
    (let ((current-color (first colors))
          (last-color (second colors)))
      (loop for x from (- size) to size
            do (loop for z from (- size) to size
                     do (draw x z current-color)
                        (rotatef current-color last-color))))))

;;; Render

(define-render basic-lighting
   ((light-pos (vec3 1.2 1.0 2.0))
    (light-color (vec3 1.0 1.0 1.0))
    (object-color (vec3 1.0 0.5 0.31)))
  (with-slots (camera-position camera-target camera-up fov) (camera (current-context))
    (let ((view (mlookat camera-position (v+ camera-position camera-target) camera-up))
          (projection (mperspective fov (/ (glfw:width *g*) (glfw:height *g*)) 0.1 100.0)))
      (labels ((make-cube ()
                 (let ((model (m* (meye 4) (mtranslation (vec 0 0.0 0.0)))))
                   (gl:use-program (program (get-vao 'v1)))
                   (upload-uniforms 'v1 (uf-pairs (("model" model) ("view" view) ("projection" projection)
                                                   ("lightColor" light-color)
                                                   ("lightPos" light-pos)
                                                   ("objectColor" object-color))))
                   (gl:bind-vertex-array (vao (get-vao 'v1)))
                   (%gl:draw-elements :triangles 36 :unsigned-int 0)))
               (make-light ()
                 (let ((model (m* (meye 4) (mtranslation light-pos) (mscaling (vec3 0.2 0.2 0.2)))))
                   (gl:use-program (program (get-vao 'light)))
                   (upload-uniforms 'light (uf-pairs (("model" model) ("view" view) ("projection" projection))))
                   (gl:bind-vertex-array (vao (get-vao 'light)))
                   (%gl:draw-elements :triangles 36 :unsigned-int 0)))
               (make-floor ()
                 (gl:use-program (program (get-vao 'floor)))
                 (upload-uniforms 'floor (uf-pairs (("view" view)
                                                    ("projection" projection)
                                                    ("lightColor" light-color)
                                                    ("lightPos" light-pos)
                                                    ("objectColor" object-color))))
                 (make-checker-board 10 '((0.32 0.25 0.33) (0.41 0.39 0.37)))))
        (gl:clear :color-buffer :depth-buffer)
        (make-cube)
        (make-light)
        (make-floor)
        (update-camera)
        (maybe-double-click)))))

;;; Start

(defun start-render ()
  #+deployed
  (use-relative-dir)
  (start 'basic-lighting :title "nohgl - basic lighting" :width 800 :height 600))
