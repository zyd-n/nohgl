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
                             (:file "camera")
                             (:file "shapes")
                             (:file "shader")
                             (:file "texture")
                             (:file "init")))))
