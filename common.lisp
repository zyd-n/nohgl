(in-package #:nohgl)

;;; Globals

(defvar *g* nil)
(defvar *vbo-handle* nil)
(defvar *vs-source* "shaders/hello.vert")
(defvar *fs-source* "shaders/hello.frag")
(defvar gscale-location)
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

(defun clean-buffer ()
  (setf *vbo-handle* nil))

;;; OpenGL Types

(declaim (ftype (function (&rest single-float) gl:gl-array)))
(defun make-gl-array (&rest args)
  (let ((arr (gl:alloc-gl-array :float (length args))))
    (dotimes (i (length args) arr)
      (setf (gl:glaref arr i)
            (elt args i)))))

;;; Utility

(defun read-file (file)
  (let ((src (pathname file)))
    (with-output-to-string (output)
      (with-open-file (stream src)
        (loop :for line := (read-line stream nil)
              :while line
              :do (format output "~a~%" line))))))

(defun fill-array (&rest args)
  (let ((arr (make-array (length args))))
    (dotimes (i (length args) arr)
      (setf (aref arr i) (elt args i)))))

;;; Conditions

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

;;; Input

(defmethod glfw:key-changed ((window g) key scan-code action modifiers)
  (when (eq key :escape)
    (quit)))

(defun process-input ()
  (glfw:poll-events))

;;; Shaders

(defun create-vertex-buffer ()
  (let ((verts (make-gl-array -1.0 -1.0 +0.0
                              +0.0 +1.0 +0.0
                              +1.0 -1.0 +0.0
                              +0.0 -1.0 +0.0
                              -1.0 +1.0 +0.0
                              +1.0 +1.0 +0.0)))
    ;; Allocate/reserve an unused handle in the namespace.
    (setf *vbo-handle* (gl:gen-buffer))
    ;; Create an object (an array buffer) and assocate or bind it to our
    ;; handle. This informs our opengl driver that we plan to populate it with
    ;; vertex attributes (positions, textures, colors etc).
    (gl:bind-buffer :array-buffer *vbo-handle*)
    ;; Finally, we actually load the position of our vertex into the vertex
    ;; buffer object. Notice the first argument: it is the target to which we
    ;; bound our handle. We don't have to specify our handle again because
    ;; OpenGL already knows which handle is currently bound to the
    ;; :array-buffer target.
    (gl:buffer-data :array-buffer :static-draw verts)))

(defun add-shader (program src type)
  (let ((shader (gl:create-shader type)))
    (assert (not (zerop shader)))
    (gl:shader-source shader src)
    (gl:compile-shader shader)
    (unwind-protect (assert (gl:get-shader shader :compile-status))
      (let ((shader-info (gl:get-shader-info-log shader)))
        (unless (zerop (length shader-info))
          (format t "~&[Shader Info]~%----------~%~a" shader-info))))
    (gl:attach-shader program shader)))

(defun check-program (program condition status)
  (if (null (gl:get-program program status))
      (error condition :shader-log (gl:get-program-info-log program))
      (gl:get-program-info-log program)))

(defun compile-shaders ()
  (let ((program (gl:create-program)))
    (assert (not (zerop program)))
    (add-shader program (read-file *vs-source*) :vertex-shader)
    (add-shader program (read-file *fs-source*) :fragment-shader)
    (gl:link-program program)
    (check-program program 'shader-link-error :link-status)
    ;; We must place the call to `get-uniform-location' during link time, aka
    ;; after `gl:link-program'.
    (when (= -1 (setf gscale-location (gl:get-uniform-location program "gScale")))
      (error 'uniform-location-error :uniform "gScale"))
    (gl:validate-program program)
    (check-program program 'invalid-shader-program :validate-status)
    (gl:use-program program)))

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
  (gl:viewport 0 0 900 600)
  (gl:clear-color 1.0 0.0 0.0 0.0)
  (create-vertex-buffer)
  (compile-shaders))

(defun main ()
  (with-slots (user-quits) *g*
    (let ((clock (time-by (nsec 1))))
      (loop :until user-quits
            :do (forward-time clock)
                (process-input)
                (draw *g*)
                (restart-case (swank::process-requests t)
                  (continue () :report "Main Loop: Continue"))))))

(defun start (render-name &rest options)
  (unless *g*
    (unwind-protect (progn (init render-name options)
                           (main))
      (shutdown)
      (clean-buffer)
      (format t "~%Killed window."))))

