(in-package #:cl-user)

(defpackage #:nohgl
  (:use #:cl #:org.shirakumo.fraf.math)
  (:local-nicknames (#:glfw #:org.shirakumo.fraf.glfw))
  (:export
   ;; main
   #:*g*
   #:store
   #:start
   #:init-options
   #:define-render
   #:render
   #:quit
   #:compile-shaders
   #:initialize-vao
   ;; vao
   #:defvao
   #:vao
   #:vbo
   #:ebo
   #:verts
   #:indices
   #:program
   #:vertex-shader
   #:fragment-shader
   #:get-vao
   #:format-vertex-attribs
   #:default-format
   #:vaos
   ;; textures
   #:texture
   #:width
   #:height
   #:data
   #:define-texture-format
   #:register-texture-units
   #:get-texture
   #:asset
   #:*asset-dir*
   #:*texture-formats*
   #:id
   ;; util
   #:gfill
   #:with-uniform-location
   #:debug-with-time
   #:shader-s
   #:*shader-dir*
   #:*main-dir*
   #:*built-as-executable*
   #:use-relative-dir
   #:degree
   #:radian
   #:limit
   ;; context
   #:current-context
   #:context-exists-p
   ;; time
   #:dt
   ;; camera
   #:camera
   #:location
   #:target
   #:up
   #:right
   #:view
   #:yaw
   #:pitch
   #:rate
   #:sens
   #:fov
   ;; mouse-location
   #:x
   #:y
   ;; input
   #:*key-stack*
   #:maybe-double-click
   #:print-camera
   #:move-up
   #:move-down
   #:input-stack
   ;; shapes
   #:define-shape
   #:cube-mesh
   #:floor-mesh
   #:wall-mesh
   ;; gpu types
   #:define-gpu-struct
   #:make-gpu-struct
   #:gpu-structs
   ;; gpu objects
   #:define-shaded-object
   #:make-shaded-object
   #:shaded-objects
   #:model
   ;; colors
   #:+black+
   #:+white+
   #:+red+
   #:+green+
   #:+blue+
   ;; transforms
   #:model-matrix
   ;; hooks
   #:add-hook
   #:remove-hook
   #:hooks-of
   #:run-hooks
   #:render-loop
   #:init
   ;; state
   #:add-state
   #:get-state
   #:remove-state
   #:state-of))


