(in-package #:nohgl)

(defun init (render-name options)
  (livesupport:setup-lisp-repl)
  (glfw:init)
  (glfw:make-current (setf (current-context) (apply #'make-instance render-name options)))
  (setf (mouse-location (current-context)) (make-instance 'mouse-location))
  (prepare (current-context))
  (run-hooks 'init)
  (initialize-vaos (vaos)))

(defun shutdown ()
  (glfw:destroy (current-context))
  (glfw:shutdown)
  (setf (current-context) nil))

;; Note how we always execute the remaining draw code after we run the
;; hooks. Will we run into a situation where we care about this ordering? What
;; to do about it..
(defun render-objects (context)
  (gl:clear :color-buffer :depth-buffer)
  (run-hooks 'render-loop)
  (draw context))

(defun main-loop ()
  (with-slots (user-quits clock) (current-context)
    (loop :until user-quits
          :do (forward-time clock)
              (render-objects (current-context))
              (process-input)
              (glfw:swap-buffers (current-context))
              (update-vaos (vaos))
              (livesupport:update-repl-link))))

(defun start (render-name &rest options)
  (if (context-exists-p)
      (warn 'context-already-exists)
      (unwind-protect (progn (init render-name options)
                             (main-loop))
        (shutdown)
        (format t "~%Killed window."))))
