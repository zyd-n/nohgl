(in-package #:nohgl)

(defgeneric init-options ())

(defun init-defaults ()
  (gl:viewport 0 0 (glfw:width *g*) (glfw:height *g*))
  (gl:clear-color .09 .09 .09 0))

(defmethod init-options ()
  (init-defaults))

(defun init (render-name options)
  (glfw:init)
  (glfw:make-current (setf *g* (apply #'make-instance render-name options)))
  (prepare *g*)
  (init-options)
  (initialize-vaos *vaos*))

(defun shutdown ()
  (glfw:destroy *g*)
  (glfw:shutdown)
  (setf *g* nil))

(defun main ()
  (with-slots (user-quits) *g*
    (let ((clock (time-by (nsec 1))))
      (loop :until user-quits
            :do (livesupport:continuable
                  (forward-time clock)
                  (process-input)
                  (draw *g*)
                  (glfw:swap-buffers *g*)
                  (update-vaos *vaos*)
                  (livesupport:update-repl-link))))))

;; TODO: Add function to manually free resources
(defun start (render-name &rest options)
  (unless *g*
    (unwind-protect
         (progn (livesupport:setup-lisp-repl)
                (init render-name options)
                (main))
      (shutdown)
      (format t "~%Killed window."))))
