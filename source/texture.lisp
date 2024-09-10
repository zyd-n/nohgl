(in-package #:nohgl)

(defvar *texture-formats* (make-hash-table :test 'equal))

(defparameter *asset-dir* (format nil "~Aassets/" *main-dir*))

(defclass texture ()
  ((name :initarg :name :initform nil :accessor name)
   (data :initarg :data :initform nil :accessor data)
   (width :initarg :width :initform nil :accessor width)
   (height :initarg :height :initform nil :accessor height)
   (format :initarg :format :initform nil :accessor texture-format)
   (source :initarg :source :initform nil :accessor texture-source)
   (id :initform nil :accessor id)))

(defun asset (filename)
  (pathname (format nil "~a~a" *asset-dir* filename)))

(defun get-texture-format (name)
  (symbol-function name))

;; (defun add-texture-format (name format)
;;   (setf (gethash name *texture-formats*) format))

(defmacro define-texture-format (name slots &body body)
  `(defun ,name (texture)
     (with-slots ,slots texture
       ,@body)))

(defun register-texture (name source format)
  (let ((png (pngload:load-file (asset source) :flip-y t :flatten t :static-vector t)))
    (make-instance 'texture :name name
                            :width (pngload:width png)
                            :height (pngload:height png)
                            :data (pngload:data png)
                            :source source
                            :format format)))

(defgeneric initialize-textures (vao textures))
(defmethod initialize-textures ((vao store) textures)
  (loop for (source name format) in textures
        do (push (register-texture name source format)
                 (textures vao))))

(defgeneric reinitialize-textures (vao))
(defmethod reinitialize-textures ((vao store))
  (with-accessors ((textures textures)) vao
    (setf textures (loop for texture in textures
                         collect (with-slots (source name format) texture
                                   (register-texture name source format))))))

(defgeneric generate-textures (vao))
(defmethod generate-textures ((vao store))
  (loop for texture in (textures vao)
        do (with-accessors ((id id)) texture
             (setf id (funcall (get-texture-format (texture-format texture)) texture)))))

(defun get-texture (name vao)
  (find-if (lambda (texture)
             (string= (symbol-name name)
                      (symbol-name (name texture))))
           (textures (get-vao vao))))
