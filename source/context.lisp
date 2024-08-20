(in-package #:nohgl)

(defvar *g* nil)

(defclass g (glfw:window)
  ((glfw:title :initform "nohgl")
   (context-version-major :initform 3)
   (opengl-profile :initform :opengl-core-profile)
   (user-quits :initform nil :accessor should-quit)))

(defun quit ()
  (with-slots (quit) *g*
    (setf (should-quit *g*) t)))
