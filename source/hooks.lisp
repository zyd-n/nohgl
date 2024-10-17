(in-package #:nohgl)

;;; Hooks

(defclass hook ()
  ((state :initarg :state :initform (make-hash-table) :accessor state)
   (hooks :initarg :hooks :initform NIL :accessor hooks)))

;; Should our hooks be a global variable or a slot in our main instance class?
(defvar *hooks* (make-hash-table))

(defun current-hooks ()
  *hooks*)

(defun (setf current-hooks) (value)
  (setf *hooks* value))

(defun make-hook (hook instance)
  (setf (gethash hook (current-hooks)) instance))

(defun find-hook (hook)
  (gethash hook (current-hooks)))

(defun (setf find-hook) (hook function)
  (setf (gethash hook (current-hooks)) function))

(defun add-hook (hook function)
  (push function (hooks (find-hook hook))))

(defun remove-hook (hook index)
  (with-accessors ((hooks hooks)) (find-hook hook)
    (setf hooks (remove (elt hooks index) hooks))))

(defun get-hooks (hook)
  (hooks (find-hook hook)))

(defun run-hooks (hook)
  (mapc
   (lambda (function)
     (funcall function (get-state hook)))
   (get-hooks hook)))

;;; State

(defun get-state (state-name)
  (state (find-hook state-name)))

(defun state-of (object-name state)
  (gethash object-name state))

(defun add-state (state-name object-name object)
  (let ((ht (get-state state-name)))
    (setf (gethash object-name ht) object)))

(defun remove-state (state-name object-name)
  (let ((ht (get-state state-name)))
    (setf (gethash object-name ht) nil)))

;;; Make hooks

(defclass render-loop (hook) ())

(make-hook 'render-loop (make-instance 'render-loop))


