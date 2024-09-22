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
               :static-vectors)
  :components ((:module "source"
                :components ((:file "package")
                             (:file "util")
                             (:file "conditions")
                             (:file "context")
                             (:file "input")
                             (:file "time")
                             (:file "render")
                             (:file "camera")
                             (:file "shapes")
                             (:file "shader")
                             (:file "texture")
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
