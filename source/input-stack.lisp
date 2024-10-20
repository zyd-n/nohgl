(in-package #:nohgl)

(defvar *input-stack* '())

(defun input-stack ()
  *input-stack*)

(defun (setf input-stack) (value)
  (setf *input-stack* value))

(defvar *keybinds* '(:w :a :s :d :p :left-control :space :button-5 :left-shift :right))

(defun current-keybinds ()
  *keybinds*)

(defun (setf current-keybinds) (value)
  (setf *keybinds* value))

(defun update-input-stack (key)
  (labels ((key-active? (k)
             (or (eq (ignore-errors (glfw:key-state k (current-context))) :press)
                 (eq (ignore-errors (glfw:key-state k (current-context))) :repeat)
                 (eq (ignore-errors (glfw:mouse-button-state k (current-context))) :press)))
           (keybinding? (key keybinding)
             (eq key keybinding)))
    (loop for keybinding in (current-keybinds)
          do (when (keybinding? key keybinding)
               (if (key-active? keybinding)
                   (unless (member keybinding (input-stack))
                     (push key (input-stack)))
                   (setf (input-stack) (remove keybinding (input-stack))))))))

(defmethod glfw:key-changed ((window g) key scan-code action modifiers)
  (update-input-stack key)
  (when (eq key :escape) (quit)))
