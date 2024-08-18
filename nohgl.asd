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
               :livesupport)
  :components ((:file "package")
               (:file "common")
               (:module "renders"
                :depends-on ("package" "common")
                :components ((:file "rectangle")
                             (:file "triangle")
                             (:file "two-triangles")
                             (:file "hello-vaos")
                             (:file "rgb-vertices")
                             (:file "xyz-as-color")))))
