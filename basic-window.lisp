(in-package #:learngl)

(defvar *window* nil)
(defvar *die* nil)
(defvar *buffer-handle* nil)

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

(defclass main-window (glfw:window)
  ((glfw:title :initform "LearnGL")
   (context-version-major :initform 3)
   (opengl-profile :initform :opengl-core-profile)))

(defun init ()
  "Setup glfw/gl window context and start glew."
  (glfw:init)
  (glfw:make-current (setf *window* (make-instance 'main-window)))
  (gl:viewport 0 0 800 600)
  (glew:glew-init)
  ;; Set the color of the window when we clear it.
  (gl:clear-color 1.0 0.0 0.0 0.0))

(defun process-input ()
  "Allows for input events to be sent to the window."
  (glfw:poll-events :timeout 0.03))

(defun render ()
  (gl:clear :color-buffer)
  (glfw:swap-buffers *window*))

(defun main-loop ()
  (unwind-protect
       (let ((c 0))
         (loop until (kill-window?)
               do (process-input)
                  (gl:clear-color c c c c)
                  (setf c (if (>= c 1.0) 0.0
                              (+ c (/ 1.0 256.0))))
                  (render)
                  (sleep 0.03)
                  (restart-case (swank::process-requests t)
                    (continue () :report "Main Loop: Continue"))))
    (shutdown)
    (format t "~%Killed window.")))

(defun launch ()
  (unless *window*
    (init)
    (main-loop)))
