(in-package #:cl-user)

(asdf:defsystem nohgl
  :author "zyd"
  :license "Public Domain"
  :description "A personal library and testbed for OpenGL theatrics."
  :serial T
  :depends-on (:alexandria
               :uiop
               :glfw
               :cl-opengl
               :local-time
               :3d-math
               :livesupport
               :pngload
               :static-vectors
               :closer-mop)
  :components
  ((:module "source"
    :components ((:file "package")
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

(asdf:defsystem nohgl-circular-yunos
  :serial T
  :depends-on (:nohgl)
  :defsystem-depends-on (:deploy)
  :build-operation "deploy-op"
  :build-pathname "nohgl"
  :entry-point "nohgl.circular-yunos:start-render"
  :components ((:module "renders"
                :components ((:file "circular-yunos")))))

(asdf:defsystem nohgl-basic-lighting
  :serial T
  :depends-on (#:nohgl)
  :components
  ((:module "renders"
    :components ((:file "basic-lighting")))))
