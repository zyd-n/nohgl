(defpackage #:nohgl.basic-lighting
  (:use #:cl #:org.shirakumo.fraf.math #:nohgl)
  (:local-nicknames (#:glfw #:org.shirakumo.fraf.glfw))
  (:export #:start-render))

(in-package #:nohgl.basic-lighting)

;;; Initialization Options

(defun setup (state)
  (declare (ignorable state))
  (gl:viewport 0 0 (glfw:width (current-context)) (glfw:height (current-context)))
  (gl:clear-color .09 .09 .09 0)
  (gl:enable :depth-test)
  (setf (glfw:input-mode :cursor (current-context)) :cursor-disabled))

(add-hook 'init 'setup)

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
  (shader-s "cube.vert")
  :fragment-shader
  (shader-s "cube.frag")
  :verts
  (make-instance 'cube-mesh)
  :uniforms
  '("model"
    "view"
    "projection"
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
  (shader-s "checker-board.vert")
  :fragment-shader
  (shader-s "checker-board.frag")
  :verts
  (make-instance 'floor-mesh)
  :uniforms
  '("model" "view" "projection" "r" "g" "b" "object_color" "light_color" "light_pos"))

(defvao 'light
  :vertex-shader
  (shader-s "light.vert")
  :fragment-shader
  (shader-s "light.frag")
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
  (with-accessors ((location location) (target target) (rate rate) (up up))
      (camera (current-context))
    (loop for key in (nohgl:input-stack)
          do (case key
               (:w (setf location (v+ location (v* target (* (dt) rate)))))
               (:a (setf location (v- location (v* (vunit (vc target up)) (* (dt) rate)))))
               (:s (setf location (v- location (v* target (* (dt) rate)))))
               (:d (setf location (v+ location (v* (vunit (vc target up)) (* (dt) rate)))))
               (:p (nohgl:print-camera))
               (:right (nohgl:print-camera))
               (:button-5 (nohgl:move-down))
               (:left-shift (nohgl:move-down))
               (:space (nohgl:move-up))))))

(alexandria:define-constant +board-colors+ '((0.32 0.25 0.33) (0.41 0.39 0.37))
  :test 'equal)

;; Its gross that we compute this every single loop. Particularly the model
;; matrix. We should be able to do all of this just once. We can also use a
;; vector for the rgb uniform.
(defun make-checker-board (instance size &optional (colors +board-colors+))
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
    :model (model-matrix :translate (vec 1.2 1.0 2.0) :scale (vec .2 .2 .2))))

;;; Render Functions

(defun render-main-objects (render-state)
  (render (get-state 'cube render-state) :count 36)
  (render (get-state 'light render-state) :count 36))

(defun render-board (render-state)
  (make-checker-board (get-state 'board render-state) 10))

(add-hook 'render-loop 'render-main-objects)
(add-hook 'render-loop 'render-board)

;;; Render

(define-render basic-lighting ()
  (update-camera)
  (maybe-double-click))

;;; Start

(defun start-render ()
  #+deployed
  (use-relative-dir)
  (start 'basic-lighting :title "nohgl - basic lighting" :width 800 :height 600))
