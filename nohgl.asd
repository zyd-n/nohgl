(in-package #:cl-user)

(asdf:defsystem nohgl
  :author "zyd"
  :license "Public Domain"
  :description "A library and testbed for OpenGL shenanigans."
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
