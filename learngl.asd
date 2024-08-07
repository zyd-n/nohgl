(in-package #:cl-user)

(asdf:defsystem learngl
  :author "zyd"
  :license "Public Domain"
  :description "Learn OpenGL"
  :serial T
  :depends-on (:alexandria
               :serapeum
               :glfw
               :cl-opengl
               :cl-autowrap)
  :components ((:file "package")
               (:file "glew")
               (:file "common")
               (:file "basic-window")
               (:file "dot")))
