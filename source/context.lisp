(in-package #:nohgl)

(defvar *g* nil)

(defun current-context ()
  *g*)

(defun (setf current-context) (value)
  (setf *g* value))

(defun context-exists-p ()
  (current-context))

(defclass g (glfw:window)
  ((glfw:title :initform "nohgl")
   (context-version-major :initform 3)
   (opengl-profile :initform :opengl-core-profile)
   (user-quits :initform nil :accessor should-quit)
   (clock :initform nil :accessor clock)
   (mouse-location :initform NIL :accessor mouse-location)
   (mouse-events :initform `(,(make-instance 'double-click)) :accessor mouse-events)
   (out-of-focus :initform nil :accessor out-of-focus)))

(defmethod initialize-instance :after ((context g) &key)
  (setf (clock context) (time-by (nsec 1))))

(defun quit ()
  (with-slots (quit) (current-context)
    (setf (should-quit (current-context)) t)))
