(in-package #:cl-user)

(asdf:defsystem nohgl
  :author "zyd"
  :license "Public Domain"
  :description "A personal library and testbed for OpenGL theatrics."
  :serial T
  :depends-on
  (#:alexandria
   #:uiop
   #:glfw
   #:cl-opengl
   #:local-time
   #:3d-math
   #:livesupport
   #:pngload
   #:static-vectors
   #:closer-mop)
  :components
  ((:module "source"
    :components
    ((:file "package")
     (:file "util")
     (:file "conditions")
     (:file "context")
     (:file "hooks")
     (:file "camera")
     (:file "input-stack")
     (:file "input")
     (:file "time")
     (:file "uniforms")
     (:file "render")
     (:file "shapes")
     (:file "shader")
     (:file "texture")
     (:file "color")
     (:file "transforms")
     (:file "defclass-star")
     (:file "gpu-types")
     (:file "gpu-object")
     (:file "init")))))

(asdf:defsystem nohgl/tools
  :description "System for tools that assist in either development of the project itself or additional but non-critical features."
  :serial T
  :depends-on
  (#:alexandria
   #:deptree
   #:uiop)
  :components
  ((:module "tools"
    :components
    ((:file "package")
     (:file "credit")))))

(asdf:defsystem nohgl-basic-lighting
  :serial T
  :depends-on (#:nohgl)
  :components
  ((:module "renders"
    :components
    ((:file "basic-lighting")))))
