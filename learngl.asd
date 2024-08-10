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
               :closer-mop)
  :components ((:file "package")
               (:file "common")
               (:file "documentation")
               (:file "hello-shader")))
