(in-package #:nohgl)

(defmethod glfw:key-changed ((window g) key scan-code action modifiers)
  (when (eq key :escape)
    (quit)))

(defun process-input ()
  (glfw:poll-events))
