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

(defvao 'cube
  :vertex-shader
  "#version 330 core

   layout (location = 0) in vec3 iPosition;
   layout (location = 1) in vec3 iNormal;
   layout (location = 2) in vec2 iUV;

   uniform mat4 model;
   uniform mat4 view;
   uniform mat4 projection;

   out vec3 FragPos;
   out vec3 Normal;

   void main()
   {
     FragPos = vec3(model * vec4(iPosition, 1.0));
     Normal = mat3(transpose(inverse(model))) * iNormal;
     // UV = vec2(iUV.x, iUV.y);

     gl_Position = projection * view * vec4(iPosition, 1.0);
   }
"
  :fragment-shader
  "#version 330 core

   struct Material {
     vec3 ambient;
     vec3 diffuse;
     vec3 specular;
     float shine;
   };

   struct Light {
     vec3 position;
     vec3 ambient;
     vec3 diffuse;
     vec3 specular;
   };

   in vec3 FragPos;
   in vec3 Normal;

   out vec4 FragColor;

   uniform Material material;
   uniform Light light;
   // uniform vec3 viewPos;

   void main()
   {

     // Ambient
     // float ambientStrength = 0.1;
     vec3 ambient = light.ambient * material.ambient;

     // Diffuse
     vec3 norm = normalize(Normal);
     vec3 lightDir = normalize(light.position - FragPos);
     float diff = max(dot(norm, lightDir), 0.0);
     vec3 diffuse = light.diffuse * (diff * material.diffuse);

     // Specular
     // float specularStrength = 0.5;
     vec3 viewDir = normalize(FragPos);
     // vec3 viewDir = normalize(viewPos - FragPos);
     vec3 reflectDir = reflect(-lightDir, norm);
     float spec = pow(max(dot(viewDir, reflectDir), 0.0), material.shine);
     vec3 specular = light.specular * (spec * material.specular);

     vec3 result = ambient + diffuse + specular;
     FragColor = vec4(result, 1.0);
   }
"
  :verts (make-instance 'cube-mesh)
  :uniforms '("model"
              "view"
              "projection"
              ;; "viewPos"
              "material.ambient"
              "material.diffuse"
              "material.specular"
              "material.shine"
              "light.position"
              "light.ambient"
              "light.diffuse"
              "light.specular"))

(defvao 'checker-board
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

   uniform vec3 object_color;
   uniform vec3 light_color;
   uniform vec3 light_pos;

   void main()
   {
     // Ambient
     float ambientStrength = 0.1;
     vec3 ambient = ambientStrength * light_color;

     // Diffuse
     vec3 norm = normalize(Normal);
     vec3 lightDir = normalize(light_pos - FragPos);
     float diff = max(dot(norm, lightDir), 0.0);
     //float diff = dot(norm, lightDir);
     vec3 diffuse = diff * light_color;

     vec3 result = (ambient + diffuse) * object_color;
     // FragColor = vec4(result, 1.0);
     FragColor = vec4(vec3(r, g, b) * result, 0.0f);
   }"
  :verts (make-instance 'floor-mesh)
  :uniforms '("model" "view" "projection" "r" "g" "b" "object_color" "light_color" "light_pos"))

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
  :verts (make-instance 'cube-mesh)
  :uniforms '("model" "view" "projection"))

;;; GPU Data

(define-gpu-struct light ()
  ((light.position (vec 1.2 1.0 2.0))
   (light.diffuse  (v* +white+ (vec 0.5 0.5 0.5)))
   (light.ambient  (v* light.diffuse 0.2))
   (light.specular (vec 1 1 1))))

(define-gpu-struct material ()
  ((material.ambient)
   (material.diffuse)
   (material.specular)
   (material.shine)))

(define-shaded-object cube ((:struct light) (:struct material))
  ((material.ambient :initform (vec 1.0 0.5 0.31))
   (material.diffuse :initform (vec 1.0 0.5 0.31))
   (material.specular :initform (vec 0.5 0.5 0.5))
   (material.shine :initform 32.0)))

(define-shaded-object light () ())

(define-shaded-object checker-board ()
  ((light-color :initform (vec 1.0 1.0 1.0))
   (light-pos :initform (vec 1.2 1.0 2.0))
   (object-color :initform (vec 1.0 0.5 0.31))
   (r :initform 0 :accessor r)
   (g :initform 0 :accessor g)
   (b :initform 0 :accessor b)))

;;; Render Helpers

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

(alexandria:define-constant +board-colors+ '((0.32 0.25 0.33) (0.41 0.39 0.37))
  :test 'equal)

(defun render-checker-board (instance size &optional (colors +board-colors+))
  (let ((current-color (first colors))
        (last-color (second colors)))
    (loop for x from (- size) to size
          do (loop for z from (- size) to size
                   do (setf (model instance) (model-matrix :translate (vec (float x) -0.5009 (float z))))
                      (destructuring-bind (r g b) current-color
                        (setf (r instance) r)
                        (setf (g instance) g)
                        (setf (b instance) b))
                      (render instance :count 6)
                      (rotatef current-color last-color)))))

;;; Render State

(add-state 'render-loop 'cube
  (make-shaded-object 'cube :vao 'cube))

(add-state 'render-loop 'board
  (make-shaded-object 'checker-board :vao 'checker-board))

(add-state 'render-loop 'light
  (make-shaded-object 'light
    :vao 'light
    :model (model-matrix :translate (vec 1.2 1.0 2.0)
                         :scale (vec .2 .2 .2))))

;;; Render Functions

(add-hook 'render-loop
  (lambda (render-state)
    (render (state-of 'cube render-state) :count 36)
    (render (state-of 'light render-state) :count 36)))

(add-hook 'render-loop
  (lambda (render-state)
    (render-checker-board (state-of 'board render-state) 10)))

;;; Render

(define-render basic-lighting ()
  (gl:clear :color-buffer :depth-buffer)
  (run-hooks 'render-loop)
  (update-camera)
  (maybe-double-click))

;;; Start

(defun start-render ()
  #+deployed
  (use-relative-dir)
  (start 'basic-lighting :title "nohgl - basic lighting" :width 800 :height 600))
