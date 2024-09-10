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
   ;; shapes
   #:shape
   #:attributes
   #:z-facing-quad
   #:cube
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
   #:camera-position
   #:camera-target
   #:camera-up
   #:camera-right
   #:camera-view
   #:camera-yaw
   #:camera-pitch
   #:camera-speed
   #:camera-sens
   #:mouse-x
   #:mouse-y
   #:*key-stack*
   ;; input
   #:maybe-double-click))


