(in-package #:nohgl)

;;; Globals

(defvar *g* nil)
(defvar *vaos* (make-hash-table :test 'equal))
(defvar *world-stats* nil)

;;; g(raphical programs)

(defclass g (glfw:window)
  ((glfw:title :initform "nohgl")
   (context-version-major :initform 3)
   (opengl-profile :initform :opengl-core-profile)
   (user-quits :initform nil :accessor should-quit)))

;;; Bindings

(defclass binding ()
  ((name :initarg :name :accessor binding-name)
   (prefix :initarg :prefix :accessor binding-prefix)
   (package :initarg :package :accessor binding-package)
   (initform :initarg :initform :accessor binding-initform)
   (initarg :initarg :initarg :accessor binding-initarg)
   (accessor :initarg :accessor :accessor binding-accessor)))

(defun make-accessor (name prefix package)
  (let ((symbol (alexandria:symbolicate prefix '#:- name)))
    (if package
        (intern (symbol-name symbol) (symbol-package prefix))
        symbol)))

(defun make-binding (name &key (prefix nil)
                               (package nil)
                               (initform nil)
                               (initarg (alexandria:make-keyword name))
                               (accessor (make-accessor name prefix package)))
  (make-instance 'binding :name name
                          :prefix prefix
                          :package package
                          :initform initform
                          :initarg initarg
                          :accessor accessor))

(defun parse-bindings (prefix binding-forms)
  (loop :for (name value) in binding-forms
        :collect (make-binding name :prefix prefix :initform value)))

;;; Render

(defun +defclass (name bindings)
  `(defclass ,name (g)
     (,@(loop :for binding in bindings
              :collect `(,(binding-name binding)
                         :initarg ,(binding-initarg binding)
                         :accessor ,(binding-accessor binding))))))

(defun +prepare (name bindings)
  `(defmethod prepare ((render ,name)
                       &key ,@(loop for b in bindings
                                    collect `((,(binding-initarg b) ,(binding-name b)) ,(binding-initform b)))
                       &allow-other-keys)
     (setf ,@(loop for b in bindings
                   collect `(,(binding-accessor b) render)
                   collect (binding-name b)))))

(defun +draw (name bindings body)
  `(defmethod draw ((render ,name))
     (with-accessors (,@(loop for b in bindings
                              collect `(,(binding-name b) ,(binding-accessor b))))
         render
       ,@body)))

(defmacro define-render (name locals &body body)
  (let ((bindings (parse-bindings name locals)))
    `(progn ,(+defclass name bindings)
            ,(+prepare name bindings)
            ,(+draw name bindings body)
            (make-instances-obsolete ',name)
            (find-class ',name))))

;;; Exit

(defun quit ()
  (with-slots (quit) *g*
    (setf (should-quit *g*) t)))

(defun shutdown ()
  "Destroy the glfw window context."
  (glfw:destroy *g*)
  (glfw:shutdown)
  (setf *g* nil))

(defun free (vaos)
  (loop :for vao-store being the hash-value of vaos
        :do (free-vao vao-store)))

(defun free-vao (vao-store)
  (with-slots (vao vbo ebo program) vao-store
    (let ((vaos (list vao))
          (buffers (list vbo ebo)))
      (gl:delete-vertex-arrays vaos)
      (gl:delete-buffers buffers)
      (gl:delete-program program))))

;;; Utility

(defgeneric read-shader (source))

(defmethod read-shader ((source string)) source)

(defmethod read-shader ((source pathname))
  (with-output-to-string (output)
    (with-open-file (stream source)
      (loop :for line := (read-line stream nil)
            :while line
            :do (format output "~a~%" line)))))

(defun gfill (type &rest args)
  (let ((arr (gl:alloc-gl-array type (length args))))
    (dotimes (i (length args) arr)
      (setf (gl:glaref arr i)
            (elt args i)))))

;;; Input

(defmethod glfw:key-changed ((window g) key scan-code action modifiers)
  (when (eq key :escape)
    (quit)))

(defun process-input ()
  (glfw:poll-events))

;;; Shaders

(define-condition shader-link-error (error)
  ((shader-log :initarg :shader-log :initform nil :reader shader-log))
  (:report (lambda (condition stream)
             (format stream "Error linking shader program:~%~a" (shader-log condition)))))

(define-condition invalid-shader-program (error)
  ((shader-log :initarg :shader-log :initform nil :reader shader-log))
  (:report (lambda (condition stream)
             (format stream "Invalid shader program:~%~a" (shader-log condition)))))

(define-condition uniform-location-error (error)
  ((uniform :initarg :uniform :initform nil :reader uniform))
  (:report (lambda (condition stream)
             (format stream "Error getting uniform location of ~s" (uniform condition)))))

(define-condition vao-without-verts (error)
  ((uniform :initarg :uniform :initform nil :reader uniform))
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Must supply vertices in order to draw primitives. Did you forget to pass in :verts?"))))

(define-condition vao-without-vertex-shader (error)
  ((uniform :initarg :uniform :initform nil :reader uniform))
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Must supply a vertex shader (source-file) when creating a VAO instance. Did you forget to pass in :vertex-shader?"))))

(define-condition vao-without-fragment-shader (error) ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Must supply a fragment shader (source-file) when creating a VAO instance. Did you forget to pass in :fragment-shader?"))))

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
   (update :initform nil :accessor update)))

(defgeneric register-vao (vao name))
(defmethod register-vao ((vao store) name)
  (setf (gethash name *vaos*) vao))

(defgeneric update-vao (vao))
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

(defgeneric register-uniforms (vao uniforms))
(defmethod register-uniforms ((vao store) uniforms)
  (loop for uniform being the hash-key of uniforms
        do (let ((uniform-location (gl:get-uniform-location (program vao) uniform)))
             (if (= uniform-location -1)
                 (error 'uniform-location-error :uniform uniform)
                 (setf (gethash uniform (uniforms vao)) uniform-location)))))

(defgeneric initialize-uniforms (vao uniforms))
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

(declaim (ftype (function (symbol &key (:verts gl:gl-array) (:vertex-shader (or string pathname)) (:fragment-shader (or string pathname)) (:uniforms list-of-strings) (:indices gl:gl-array))) defvao))
(defun defvao (name &key (verts (error 'vao-without-verts))
                         (vertex-shader (error 'vao-without-vertex-shader))
                         (fragment-shader (error 'vao-without-fragment-shader))
                         uniforms indices)
  (let ((current-store (get-vao name))
        (vao-store (make-instance 'store :verts verts
                                         :vertex-shader (read-shader vertex-shader)
                                         :fragment-shader (read-shader fragment-shader)
                                         :indices indices
                                         :name name)))
    (initialize-uniforms vao-store uniforms)
    (if (and current-store *g*)
        (setf (update current-store) vao-store)
        (register-vao vao-store name))))


(defmethod initialize-vao ((vao-store store))
  (with-slots (vao vbo ebo verts indices) vao-store
    (setf (vao vao-store) (gl:gen-vertex-array)
          (vbo vao-store) (gl:gen-buffer)
          (ebo vao-store) (gl:gen-buffer))
    (gl:bind-vertex-array vao)
    (gl:bind-buffer :array-buffer vbo)
    (gl:buffer-data :array-buffer :static-draw verts)
    (gl:bind-buffer :element-array-buffer ebo)
    (when indices (gl:buffer-data :element-array-buffer :static-draw indices))
    (gl:vertex-attrib-pointer 0 3 :float :false (* 3 (cffi:foreign-type-size :float)) 0)
    (gl:enable-vertex-attrib-array 0)
    ;; unbind
    (gl:bind-buffer :array-buffer 0)
    (gl:bind-vertex-array 0)))

(defun initialize-vaos (vaos)
  (maphash (lambda (k v)
             (declare (ignore k))
             (compile-shaders v)
             (initialize-vao v))
           vaos))

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
    (let ((vs (add-shader program vertex-shader :vertex-shader))
          (fs (add-shader program fragment-shader :fragment-shader)))
      (gl:link-program program)
      (check-program program 'shader-link-error :link-status)
      (register-uniforms vao-store uniforms)
      (gl:validate-program program)
      (check-program program 'invalid-shader-program :validate-status)
      (gl:use-program program)
      (gl:delete-shader vs)
      (gl:delete-shader fs))))

;;; Time Step

(defclass world-clock ()
  ((duration :initform (error "Must supply a duration of some kind.") :initarg :duration :accessor duration)
   (ticks :reader ticks :initform 0)
   (frames :reader frames :initform 0.0)))

(defun nsec (n)
  (local-time:timestamp+ (local-time:now) n :sec))

(defun time-by (duration)
  (make-instance 'world-clock :duration duration))

(defgeneric forward-time (clock))
(defmethod forward-time ((clock world-clock))
  (with-slots (duration ticks frames) clock
    (when (local-time:timestamp>= (local-time:now) duration)
      (setf (duration clock) (nsec 1))
      (incf ticks)
      (log-time clock)
      (decf frames frames))
    (incf frames))
  clock)

(defgeneric log-time (clock))
(defmethod log-time ((clock world-clock))
  (when *world-stats*
    (with-slots (ticks frames) clock
      (format t "Tick: ~s~%FPS: ~s~%Miliseconds: ~s~%--------------------~%~%" ticks frames (/ 1000.0 frames)))))

(defun debug-with-time (&optional (enable t))
  (setf *world-stats* enable))

;;; Init

(defun init (render-name options)
  (glfw:init)
  (glfw:make-current (setf *g* (apply #'make-instance render-name options)))
  (prepare *g*)
  (gl:viewport 0 0 (glfw:width *g*) (glfw:height *g*))
  (gl:clear-color .09 .09 .09 0)
  (initialize-vaos *vaos*))

(defun main ()
  (with-slots (user-quits) *g*
    (let ((clock (time-by (nsec 1))))
      (loop :until user-quits
            :do (forward-time clock)
                (process-input)
                (draw *g*)
                (update-vaos *vaos*)
                (livesupport:update-repl-link)))))

(defun start (render-name &rest options)
  (unless *g*
    (unwind-protect (progn (livesupport:setup-lisp-repl)
                           (init render-name options)
                           (main))
      (shutdown)
      (free *vaos*)
      (format t "~%Killed window."))))
