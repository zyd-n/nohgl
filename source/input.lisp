;; Maybe this file should be renamed to events.lisp?

(in-package #:nohgl)

(defmethod glfw:window-resized ((window g) width height)
  (gl:viewport 0 0 width height))

;; We might want different types of mouse-events in the future.
(defclass mouse-event () ())

(defclass double-click (mouse-event)
  ((click-stack :initform 0 :accessor click-stack)
   (interval :initform nil :accessor interval)))

(defun get-mouse-event (name)
  (find-if (lambda (event) (eq name (class-name (class-of event))))
           (mouse-events (current-context))))

(defun get-cursor-state ()
  (case (glfw:input-mode :cursor (current-context))
    (212993 :cursor-normal)
    (212995 :cursor-disabled)
    (212994 :cursor-hidden)))

(defun notify-event (event-name)
  (let ((event (get-mouse-event event-name)))
    (unless event (error "Event ~s either does not exist or is not bound to the current context." event))
    (incf (click-stack event))))

(defun disable/enable-cursor ()
  (case (get-cursor-state)
    (:cursor-normal (setf (glfw:input-mode :cursor (current-context)) :cursor-disabled))
    (:cursor-disabled (setf (glfw:input-mode :cursor (current-context)) :cursor-normal))))

(defun maybe-double-click ()
  (let ((double-click (get-mouse-event 'double-click)))
    (with-accessors ((click-stack click-stack) (interval interval)) double-click
      (labels ((reset-double-click ()
                 (setf click-stack 0)
                 (setf interval nil)))
        (case click-stack
          (1 (unless interval
               (setf interval (local-time:timestamp+ (local-time:now) 1 :sec))))
          ;; This could be generalized to doing whatever action is registered
          ;; to a double click rather than this hard coded action.
          (2
           (disable/enable-cursor)
           (reset-double-click)))
        (when (and interval (local-time:timestamp>= (local-time:now) interval))
          (reset-double-click))))))

(defmethod glfw:mouse-button-changed ((window g) button action modifiers)
  (when (and (eq button :left) (eq action :press))
    (notify-event 'double-click)))

(defun process-input ()
  (glfw:poll-events))
