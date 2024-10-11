(in-package #:nohgl)

(defclass gpu-struct () ()
  (:documentation "A class used for uniforms that are structs. Meant to be subclassed rather than directly used, this way we can define methods that operate on all gpu-structs, if needed."))

(defvar *gpu-structs* (make-hash-table :test 'equal))

(defun gpu-structs ()
  *gpu-structs*)

(defun (setf gpu-structs) (value)
  (setf *gpu-structs* value))

(defun setup-gpu-slot (slot-form)
  (let* ((slot-name (first slot-form)))
    `(,slot-name :initarg ,(alexandria:make-keyword slot-name)
                 :initform ,(second slot-form)
                 :accessor ,slot-name)))

(defgeneric find-gpu-class (type class-name))

(defmethod find-gpu-class (type class-name)
  (declare (ignorable class-name))
  (error 'gpu-type-does-not-exist :gpu-type type))

(defmethod find-gpu-class ((type (eql :struct)) class-name)
  (alexandria:if-let ((real-class (gethash class-name (gpu-structs))))
    real-class
    (error 'gpu-struct-not-found :struct-name class-name)))

(defmacro define-gpu-struct (name superclasses slots)
  "Defines a glsl-like struct to map uniform values."
  (let ((class-name (read-from-string (format nil "gpu-struct/~s" name))))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defclass* ,class-name (gpu-struct ,@superclasses)
         ,(loop for slot-form in slots collect (setup-gpu-slot slot-form)))
       (let ((class (find-class ',class-name)))
         (setf (gethash ',name (gpu-structs))
               (class-name class))
         (closer-mop:finalize-inheritance class)))))

(defun make-gpu-struct (name &rest initargs &key &allow-other-keys)
  (alexandria:if-let ((class-name (gethash name (gpu-structs))))
    (apply #'make-instance class-name initargs)
    (error 'gpu-struct-not-found :struct-name name)))
