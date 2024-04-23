(in-package #:learngl)

(defvar *window*         NIL)
(defvar *die*            NIL)
(defvar *commands*       NIL)
(defvar *shader-program* NIL)
(defvar *vao*            NIL)    
(defvar *vbo*            NIL)
(defvar *vert->gl* NIL)

(defvar *vert* #(-0.5 -0.5 0.0
                 0.5 -0.5 0.0
                 0.0 0.5 0.0))

(defparameter *vertex-shader-source*
  "#version 330 core
layout (location = 0) in vec3 pos;
void main()
{
  gl_Position = vec4(pos.x, pos.y, pos.z, 1.0);
}")

(defparameter *fragment-shader-source*
  "#version 330 core
out vec4 FragColor;
void main()
{
  FragColor = vec4(1.0f, 0.5f, 0.2f, 1.0f);
}")

(defclass main-window (glfw:window)
  ((glfw:title :initform "LearnGL")
   (context-version-major :initform 3)
   (opengl-profile :initform :opengl-core-profile)))

(defmacro set-draw-commands (&body body)
  `(progn (setf *commands* (lambda ()
                             ,@body))
          (when *window* (glfw:swap-buffers *window*))))

(defun draw-color (&optional (commands *commands*))
  (funcall commands))

(defun kill-window? ()
  *die*)

(defun kill-window ()
  (setf *die* T))

(defun compile-shader ()
  (let ((vertex-shader (gl:create-shader :vertex-shader))
        (fragment-shader (gl:create-shader :fragment-shader)))
    (gl:shader-source  vertex-shader *vertex-shader-source*)
    (gl:compile-shader vertex-shader)

    (gl:shader-source  fragment-shader *fragment-shader-source*)
    (gl:compile-shader fragment-shader)

    (setf *shader-program* (gl:create-program))

    (gl:attach-shader *shader-program* vertex-shader)
    (gl:attach-shader *shader-program* fragment-shader)
    (gl:link-program  *shader-program*)
    
    (gl:delete-shader vertex-shader)
    (gl:delete-shader fragment-shader)

    (setf *vert->gl*
          (loop with gl-array = (gl:alloc-gl-array :float (length *vert*))
                for i from 0 below (length *vert*)
                do (setf (gl:glaref gl-array i)
                         (elt *vert* i))
                finally (return gl-array)))))

(defun init-buffer ()
  (setf *vao* (gl:gen-vertex-array)
        *vbo* (gl:gen-buffer))
  (gl:bind-vertex-array *vao*)
  (gl:bind-buffer :array-buffer *vbo*)
  (gl:buffer-data :array-buffer :static-draw *vert->gl*)
  (gl:vertex-attrib-pointer 0 3 :float 0 (* 3 (cffi:foreign-type-size :float)) 0)
  (gl:enable-vertex-attrib-array 0)
  (gl:bind-buffer :array-buffer 0)
  (gl:bind-vertex-array 0))

(defun shutdown ()
  (glfw:destroy *window*)
  (glfw:shutdown)
  (setf *window* NIL)
  (setf *die* NIL))

(defun kill-buffer ()
  (gl:use-program 0)
  (gl:bind-vertex-array 0)
  
  (gl:delete-vertex-arrays (list *vao*))
  (gl:delete-buffers (list *vbo*))
  (gl:delete-program *shader-program*)

  (setf *vao* NIL)
  (setf *vbo* NIL)
  (setf *shader-program* NIL))

(defun create-window (&rest keys)
  (unless *window*
    (glfw:init)
    (glfw:make-current (setf *window* (apply #'make-instance 'main-window keys)))
    (gl:viewport 0 0 800 600)
    (compile-shader)
    (init-buffer)
    (format T "Launching window~%")
    (unwind-protect
         (loop initially (slynk-mrepl:send-prompt)
               until (kill-window?)
               do (with-simple-restart (abort "Skip this game loop.")
                    (glfw:poll-events :timeout 0.03)
                    (draw-color)
                    (gl:use-program *shader-program*)
                    (gl:bind-vertex-array *vao*)
                    (gl:draw-arrays :triangles 0 3)
                    (gl:bind-vertex-array 0)
                    (glfw:swap-buffers *window*)
                    (sleep 0.03)
                    (with-simple-restart (abort "Abort this REPL evaluation")
                      (slynk:process-requests T))))
      (kill-buffer)
      (shutdown)
      (format T "~%Killed window."))))

(defmethod glfw:key-changed ((window main-window) key scan-code action modifiers)
  (when (eq key :escape)
    (kill-window)))

(set-draw-commands
  (gl:clear-color 0.3 0.4 0.6 1.0)
  (gl:clear :color-buffer))

;; (defmethod glfw:char-entered ((window main-window) code-point))
;; (defmethod glfw:key-state (key (window main-window)))
;; (defmethod glfw:window-resized ((window main-window) width height))
;; (defmethod glfw:window-moved ((window main-window) xpos ypos))
