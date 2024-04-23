(in-package #:cl-user)

(asdf:defsystem learngl
  :author "zyd"
  :license "Public Domain"
  :description "Learn OpenGL"
  :serial T
  :depends-on (:alexandria
               :serapeum
               :glfw
               :cl-opengl)
  :components ((:file "package")
               (:file "window")))
