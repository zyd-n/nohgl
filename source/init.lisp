(in-package #:nohgl)

(defgeneric init-options ())

(defun init-defaults ()
  (gl:viewport 0 0 (glfw:width (current-context)) (glfw:height (current-context)))
  (gl:clear-color .09 .09 .09 0)
  (setf (glfw:input-mode :cursor (current-context)) :cursor-disabled))

(defmethod init-options ()
  (init-defaults))

(defun init (render-name options)
  (glfw:init)
  (glfw:make-current (setf (current-context) (apply #'make-instance render-name options)))
  (prepare (current-context))
  (init-options)
  (initialize-vaos *vaos*))

(defun shutdown ()
  (glfw:destroy (current-context))
  (glfw:shutdown)
  (setf (current-context) nil))

(defun main ()
  (with-slots (user-quits clock) (current-context)
    (loop :until user-quits
          :do (livesupport:continuable
                (forward-time clock)
                (draw (current-context))
                (process-input)
                (glfw:swap-buffers (current-context))
                (update-vaos *vaos*)
                (livesupport:update-repl-link)))))

;; TODO: Add function to manually free resources
(defun start (render-name &rest options)
  (unless (context-exists-p)
    (unwind-protect
         (progn (livesupport:setup-lisp-repl)
                (init render-name options)
                (main))
      (shutdown)
      (format t "~%Killed window."))))
