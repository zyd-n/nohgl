(in-package #:learngl)

(setf (documentation 'clean-buffer 'function) "Clean/clear out the buffer")
(setf (documentation 'quit 'function) "Quit the program.")
(setf (documentation 'read-file 'function) "Return the contents of FILE as a string. Accepts a string or pathname.")
(setf (documentation 'make-gl-array 'function) "Allocate a GL array for vertices.")
(setf (documentation 'process-input 'function) "Allow input events to be sent to the window.")

(setf (documentation (find-class 'g) t)
      "Represents a (g)raphical program and is responsible for setting GLFW window options. All our renders are subclasses of g.")

(setf (documentation 'create-vertex-buffer 'function)
      "Initialize the vertex buffer: contains the coordinates of the object we want to create and allocates memory for the GPU.")

(setf (documentation 'define-render 'function)
      "Macro responsible for creating renders: OpenGL programs. Any OpenGL command goes here (aside from shader initialization code). Allows one to create local variables that are initialized upon compile and then updated for every iteration of the main loop during run-time, as well as pass options to the GLFW backend.

(define-render some-kind-of-name
    (:options (title \"hello shaders, once again <3\"))
    (:locals (c 0.0))
  (gl:clear-color c 0 0 1)
  (format t \"yippee\"))

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
      "Act as an initializer for the render instance passed in.

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
