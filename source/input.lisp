(in-package #:nohgl)

;; Maybe this file should be renamed to events.lisp?

;; TODO: Make it so that when you press escape, we change the input mode of
;; the cursor to :cursor-normal. However, when we click the window, it should
;; change back to :cursor-disabled.

(defmethod glfw:window-resized ((window g) width height)
  (gl:viewport 0 0 width height))

(defun process-input ()
  (glfw:poll-events))
