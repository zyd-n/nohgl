(in-package #:nohgl)

(defvar *vaos* (make-hash-table :test 'equal))

(defclass store ()
  ((vao :accessor vao)
   (vbo :accessor vbo)
   (ebo :accessor ebo)
   (name :initarg :name :accessor name)
   (program :accessor program)
   (uniforms :accessor uniforms)
   (indices :initarg :indices :initform nil :accessor indices)
   (verts :initarg :verts :accessor verts :initform (error 'vao-without-verts))
   (vertex-shader :initarg :vertex-shader :accessor vertex-shader :initform (error 'vao-without-vertex-shader))
   (fragment-shader :initarg :fragment-shader :accessor fragment-shader :initform (error 'vao-without-fragment-shader))
   (update :initform nil :accessor update)
   (texture :initarg :texture :initform nil :accessor texture)))

(defgeneric read-shader (source))
(defgeneric register-vao (vao name))
(defgeneric update-vao (vao))
(defgeneric register-uniforms (vao uniforms))
(defgeneric initialize-uniforms (vao uniforms))
(defgeneric format-vertex-attribs ())

(defmethod read-shader ((source string)) source)

(defmethod read-shader ((source pathname))
  (with-output-to-string (output)
    (with-open-file (stream source)
      (loop :for line := (read-line stream nil)
            :while line
            :do (format output "~a~%" line)))))

(defmethod register-vao ((vao store) name)
  (setf (gethash name *vaos*) vao))

(defmethod update-vao ((vao store))
  (with-slots (name) vao
    (compile-shaders vao)
    (initialize-vao vao)
    (free-vao (get-vao name))
    (register-vao vao name)))

(defun update-vaos (vaos)
  (loop :for vao being the hash-value of vaos
        :do (let ((vao+ (update vao)))
              (when vao+ (update-vao vao+)))))

(defun get-vao (vao-name)
  (gethash vao-name *vaos*))

(defun free-vao (vao-store)
  (with-slots (vao vbo ebo program) vao-store
    (let ((vaos (list vao))
          (buffers (list vbo ebo)))
      (gl:delete-vertex-arrays vaos)
      (gl:delete-buffers buffers)
      (gl:delete-program program))))

(defun free (vaos)
  (loop :for vao-store being the hash-value of vaos
        :do (free-vao vao-store)))

(defmethod register-uniforms ((vao store) uniforms)
  (loop for uniform being the hash-key of uniforms
        do (let ((uniform-location (gl:get-uniform-location (program vao) uniform)))
             (if (= uniform-location -1)
                 (error 'uniform-location-error :uniform uniform)
                 (setf (gethash uniform (uniforms vao)) uniform-location)))))

(defmethod initialize-uniforms ((vao store) uniforms)
  (let ((ht (make-hash-table :test 'equal)))
    (dolist (uniform uniforms)
      (setf (gethash uniform ht) nil))
    (setf (uniforms vao) ht)))

(defun get-uniform (uniform-name vao-store)
  (gethash uniform-name (uniforms (get-vao vao-store))))

(defun list-of-strings-p (list)
  (and (consp list) (every #'stringp list)))

(deftype list-of-strings ()
  `(satisfies list-of-strings-p))

(declaim (ftype (function (symbol &key
                                  (:verts gl:gl-array)
                                  (:vertex-shader (or string pathname))
                                  (:fragment-shader (or string pathname))
                                  (:uniforms list-of-strings)
                                  (:indices gl:gl-array)
                                  (:texture pathname)))
                defvao))
(defun defvao (name &key (verts (error 'vao-without-verts))
                         (vertex-shader (error 'vao-without-vertex-shader))
                         (fragment-shader (error 'vao-without-fragment-shader))
                         uniforms
                         indices
                         texture)
  (let ((current-store (get-vao name))
        (vao-store (make-instance 'store :verts verts
                                         :vertex-shader (read-shader vertex-shader)
                                         :fragment-shader (read-shader fragment-shader)
                                         :indices indices
                                         :name name
                                         :texture (when texture (make-texture texture)))))
    (initialize-uniforms vao-store uniforms)
    (if (and current-store *g*)
        (setf (update current-store) vao-store)
        (register-vao vao-store name))))


(defmethod format-vertex-attribs ()
  (gl:vertex-attrib-pointer 0 3 :float :false (* 3 (cffi:foreign-type-size :float)) 0)
  (gl:enable-vertex-attrib-array 0))

(defmethod initialize-vao ((vao-store store))
  (with-accessors ((vao vao) (vbo vbo) (ebo ebo) (verts verts) (indices indices) (texture texture)) vao-store
    (setf vao (gl:gen-vertex-array)
          vbo (gl:gen-buffer)
          ebo (gl:gen-buffer))
    (when texture (setf texture (generate-texture texture)))
    (gl:bind-vertex-array vao)
    (gl:bind-buffer :array-buffer vbo)
    (gl:buffer-data :array-buffer :static-draw verts)
    (gl:bind-buffer :element-array-buffer ebo)
    (when indices (gl:buffer-data :element-array-buffer :static-draw indices))
    (format-vertex-attribs)
    ;; unbind
    (gl:bind-buffer :array-buffer 0)
    (gl:bind-vertex-array 0)))

(defun initialize-vaos (vaos)
  (loop for vao being the hash-value of vaos
        do (compile-shaders vao)
           (initialize-vao vao)))

(defun add-shader (program src type)
  (let ((shader (gl:create-shader type)))
    (assert (not (zerop shader)))
    (gl:shader-source shader src)
    (gl:compile-shader shader)
    (unwind-protect (assert (gl:get-shader shader :compile-status))
      (let ((shader-info (gl:get-shader-info-log shader)))
        (unless (zerop (length shader-info))
          (format t "~&[Shader Info]~%----------~%~a" shader-info))))
    (gl:attach-shader program shader)
    shader))

(defun check-program (program condition status)
  (if (null (gl:get-program program status))
      (error condition :shader-log (gl:get-program-info-log program))
      (gl:get-program-info-log program)))

(defmethod compile-shaders ((vao-store store))
  (with-slots (vertex-shader fragment-shader program uniforms) vao-store
    (setf (program vao-store) (gl:create-program))
    (when (zerop program)
      (error "An error occured when creating program object."))
    (let ((vertex-shader (add-shader program vertex-shader :vertex-shader))
          (fragment-shader (add-shader program fragment-shader :fragment-shader)))
      (gl:link-program program)
      (check-program program 'shader-link-error :link-status)
      (register-uniforms vao-store uniforms)
      (gl:validate-program program)
      (check-program program 'invalid-shader-program :validate-status)
      (gl:use-program program)
      (gl:delete-shader vertex-shader)
      (gl:delete-shader fragment-shader))))
