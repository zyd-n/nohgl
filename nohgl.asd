(in-package #:cl-user)

(asdf:defsystem nohgl
  :author "zyd"
  :license "Public Domain"
  :description "A personal library and testbed for OpenGL theatrics."
  :serial T
  :depends-on (:alexandria
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
                             (:file "shader")
                             (:file "texture")
                             (:file "init")))
               (:module "renders"
                :depends-on ("source")
                :components ((:file "rectangle")
                             (:file "triangle")
                             (:file "two-triangles")
                             (:file "hello-vaos")
                             (:file "rgb-vertices")
                             (:file "xyz-as-color")
                             (:file "hello-texture")))))
