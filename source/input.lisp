(in-package #:nohgl)

;; Maybe this file should be renamed to events.lisp?

(defmethod glfw:key-changed ((window g) key scan-code action modifiers)
  (when (eq key :escape)
    (quit)))

(defmethod glfw:window-resized ((window g) width height)
  (gl:viewport 0 0 width height))

(defun process-input ()
  (glfw:poll-events))
