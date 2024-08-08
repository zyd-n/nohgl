(in-package #:learngl)

(setf (documentation (find-class 'g) t)
      "Represents a (g)raphical program and is responsible for setting GLFW window options. All our renders are subclasses of g.")


(setf (documentation 'define-render 'function)
      "Macro responsible for creating renders: OpenGL programs. Any OpenGL command goes here (aside from shader initialization code). Allows one to create local variables that are initialized upon compile and then updated for every iteration of the main loop during run-time.

(define-render some-kind-of-name
    ((local1 'v1)
     (local2 'v2))
  (* 4 2)
  (gl:clear-color 1 0 0 1))

")

;; (setf (documentation 'make-accessor 'function))

;; (setf (documentation 'make-binding 'function))

(setf (documentation 'parse-bindings 'function)
      "Create a list of binding objects, which are the local variables/slot values defined by a render.")

(setf (documentation '+defclass 'function)
      "Create a class named after our render and populate it with slots from the bindings.

Return a form of:
(defclass NAME (g) (BINDINGS...))")

(setf (documentation '+prepare 'function)
      "Act as an initializer for the render instanced passed in.

Return a form of:

(defmethod prepare ((render NAME) &key BINDINGS..)
  (setf BINDING1)
  (setf BINDING2)
  ...)")

(setf (documentation '+draw 'function)
      "Create a draw method used to execute some body of OpenGL iteration of the main loop.

Return a form of:

(defmethod draw ((render NAME))
  (with-accessors ((BINDING1 <NAME>-<BINDING1>)
                   ...)
      render
    BODY))")
