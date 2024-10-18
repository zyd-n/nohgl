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
  (with-accessors ((hooks hooks)) (find-hook hook)
    (setf hooks (append (remove function hooks) (list function)))))

(defun remove-hook (hook function)
  (with-accessors ((hooks hooks)) (find-hook hook)
    (setf hooks (remove function hooks))))

(defun hooks-of (hook)
  (hooks (find-hook hook)))

(defun run-hooks (hook)
  (mapc
   (lambda (function)
     (funcall function (state-of hook)))
   (hooks (find-hook hook))))

;;; State

(defun state-of (hook)
  (state (find-hook hook)))

(defun get-state (object-name state)
  (gethash object-name state))

(defun add-state (hook object-name object)
  (let ((ht (state-of hook)))
    (setf (gethash object-name ht) object)))

(defun remove-state (hook object-name)
  (let ((ht (state-of hook)))
    (setf (gethash object-name ht) NIL)))

;;; Make hooks

(defclass render-loop (hook) ())

(make-hook 'render-loop (make-instance 'render-loop))


