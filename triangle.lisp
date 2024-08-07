(in-package #:learngl)

(defvar *window* nil)
(defvar *die* nil)
(defvar *vbo-handle* nil)

(defun kill-window? ()
  *die*)

(defun kill-window ()
  "Kills the main window."
  (setf *die* t))

(defun shutdown ()
  "Destroy the glfw window context."
  (glfw:destroy *window*)
  (glfw:shutdown)
  (setf *window* nil)
  (setf *die* nil))

(defun clean-buffer ()
  "Clean/clear out the buffer"
  (setf *vbo-handle* nil))

(defclass main-window (glfw:window)
  ((glfw:title :initform "LearnGL")
   (context-version-major :initform 3)
   (opengl-profile :initform :opengl-core-profile)))

(defun make-gl-array (&rest args)
  "Allocate a GL array for vertices. Must be a length that is a multiple of 3 (a
vertex has three points). Converts integers to floats."
  (let ((arr (gl:alloc-gl-array :float (length args)))
        (args (mapcar (lambda (n) (/ n 1.0)) args)))
    (dotimes (i (length args) arr)
      (setf (gl:glaref arr i)
            (elt args i)))))

(defun create-vertex-buffer ()
  "Initialize the vertex buffer: contains the coordinates of the object we want
to create and allocated the memory for the GPU."
  (let ((verts (make-gl-array -1 -1 0 0 1 0 1 -1 0)))
    ;; Allocate/reserve an unused handle in the namespace.
    (setf *vbo-handle* (gl:gen-buffer))
    ;; Create an object (an array buffer) and assocate or bind it to our
    ;; handle. This informs our opengl driver that we plan to populate it with
    ;; vertex attributes (positions, textures, colors etc).
    (gl:bind-buffer :array-buffer *vbo-handle*)
    ;; Finally, we actually load the position of our vertex into the vertex
    ;; buffer object. Notice the first argument: it is the target to which we
    ;; bound our handle. We don't have to specify our handle again because
    ;; OpenGL already knows which handle is currently bound to the
    ;; :array-buffer target.
    (gl:buffer-data :array-buffer :static-draw verts)))

(defun init ()
  "Setup glfw/gl window context and start glew."
  (glfw:init)
  (glfw:make-current (setf *window* (make-instance 'main-window)))
  (gl:viewport 0 0 800 600)
  ;; (glew:glew-init)
  ;; Set the color of the window when we clear it.
  (gl:clear-color 1.0 0.0 0.0 0.0)
  (create-vertex-buffer))

(defun process-input ()
  "Allows for input events to be sent to the window."
  (glfw:poll-events :timeout 0.03))

(defun render ()
  (gl:clear :color-buffer)
  (gl:bind-buffer :array-buffer *vbo-handle*)
  ;; 0 = position
  ;; All this does is enable a kind of attribute we can use. In this case,
  ;; position.
  (gl:enable-vertex-attrib-array 0)
  ;; This informs OpenGL about the kind of data (our vertex) we're passing and
  ;; how to interpret it.
  (gl:vertex-attrib-pointer 0 3 :float 0 0 0)
  ;; Treat the data as point (shape primitive); Index of the first vertex to
  ;; draw; The number of vertices to draw.
  (gl:draw-arrays :triangles 0 3)
  (gl:disable-vertex-attrib-array 0)
  (glfw:swap-buffers *window*))

(defun main-loop ()
  (unwind-protect
       (let ((c 0))
         (loop until (kill-window?)
               do (process-input)
                  (gl:clear-color 5 c .2 .6)
                  (setf c (if (>= c 1.0) 0.0
                              (+ c (/ 1.0 256.0))))
                  (render)
                  (sleep 0.03)
                  (restart-case (swank::process-requests t)
                    (continue () :report "Main Loop: Continue"))))
    (shutdown)
    (clean-buffer)
    (format t "~%Killed window.")))

(defun launch ()
  (unless *window*
    (init)
    (main-loop)))
