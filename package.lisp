(in-package #:cl-user)

(defpackage #:nohgl
  (:use #:cl)
  (:local-nicknames (#:alex #:alexandria)
                    (#:sera #:serapeum)
                    (#:glfw #:org.shirakumo.fraf.glfw)
                    (#:glfw.c #:org.shirakumo.fraf.glfw.cffi))
  (:export #:start
           #:define-render
           #:quit))
