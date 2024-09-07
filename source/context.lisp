(in-package #:nohgl)

(defvar *g* nil)

(defun current-context ()
  *g*)

(defun (setf current-context) (value)
  (setf *g* value))

(defun context-exists-p ()
  (current-context))

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
   (user-quits :initform nil :accessor should-quit)
   (clock :initform nil :accessor clock)
   ;; This will hold all mouse-events, a list of mouse-event
   ;; objects. Currently will just hold `double-click'.
   (mouse-events :initform `(,(make-instance 'double-click)) :accessor mouse-events)))

(defmethod initialize-instance :after ((context g) &key)
  (setf (clock context) (time-by (nsec 1))))

(defun quit ()
  (with-slots (quit) (current-context)
    (setf (should-quit (current-context)) t)))
