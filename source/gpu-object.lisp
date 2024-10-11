(in-package #:nohgl)

(defvar *shaded-objects* (make-hash-table :test 'equal))

(defun shaded-objects ()
  *shaded-objects*)

(defun (setf shaded-objects) (value)
  (setf *shaded-objects* value))

(defun make-shaded-object (name &rest initargs &key &allow-other-keys)
  (alexandria:if-let ((class-name (gethash name (shaded-objects))))
    (apply #'make-instance class-name initargs)
    (error 'shaded-object-not-found :object-name name)))

(defmacro define-shaded-object (name types uniforms)
  (let ((class-name (read-from-string (format nil "shaded-~s" name))))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defclass ,class-name
           ,(loop for class in types
                  collect (if (listp class)
                              (find-gpu-class (first class) (second class))
                              class))
         ((vao :initarg :vao :initform nil :accessor vao)
          (model :initarg :model :initform (model-matrix) :accessor model)
          ,@uniforms))
       (let ((class (find-class ',class-name)))
         (setf (gethash ',name (shaded-objects))
               (class-name class))
         (closer-mop:finalize-inheritance class)))))
