(in-package #:cl-user)

(asdf:defsystem nohgl
  :author "zyd"
  :license "Public Domain"
  :description "A personal library and testbed for OpenGL theatrics."
  :serial T
  :depends-on (:alexandria
               :serapeum
               :glfw
               :cl-opengl
               :closer-mop
               :local-time
               :3d-math)
  :components ((:file "package")
               (:file "common")
               (:file "documentation")
               (:module "renders"
                :depends-on ("package" "common")
                :components ((:file "rectangle")
                             (:file "triangle")
                             (:file "two-triangles")
                             (:file "bug")))))
