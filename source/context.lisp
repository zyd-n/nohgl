(in-package #:nohgl)

(defvar *g* nil)

;; TODO: VAOs should be a slot in g. This way we can have package local vaos
;; and vaos specific to a context. This will fix our initialization problem
;; to: we can just update the vaos on *g* in the main loop. No need to look at
;; everyone's vaos

;; Also, aren't we using our abstractions wrong? What I mean is the `store'. A
;; store should be a collection of many similar types of objects. But
;; currently it is in fact just a `shader' object. What a `store' ought to be
;; is a collection of shaders, such that when we (defvao ...), a shader gets
;; added to the `store' class (redefined on the fly?)

(defclass g (glfw:window)
  ((glfw:title :initform "nohgl")
   (context-version-major :initform 3)
   (opengl-profile :initform :opengl-core-profile)
   (user-quits :initform nil :accessor should-quit)))

(defun quit ()
  (with-slots (quit) *g*
    (setf (should-quit *g*) t)))
