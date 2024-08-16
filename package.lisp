(in-package #:cl-user)

(defpackage #:nohgl
  (:use #:cl #:org.shirakumo.fraf.math)
  (:local-nicknames (#:glfw #:org.shirakumo.fraf.glfw))
  (:export
   ;; Main
   #:*g*
   #:start
   #:define-render
   #:gfill
   #:quit
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
   #:get-vao))

(defpackage #:nohgl.triangle
  (:use #:cl #:org.shirakumo.fraf.math #:nohgl)
  (:local-nicknames (#:glfw #:org.shirakumo.fraf.glfw))
  (:export #:start-triangle))

(defpackage #:nohgl.rectangle
  (:use #:cl #:org.shirakumo.fraf.math #:nohgl)
  (:local-nicknames (#:glfw #:org.shirakumo.fraf.glfw))
  (:export #:start-rectangle))

(defpackage #:nohgl.two-triangles
  (:use #:cl #:org.shirakumo.fraf.math #:nohgl)
  (:local-nicknames (#:glfw #:org.shirakumo.fraf.glfw))
  (:export #:start-triangles))
