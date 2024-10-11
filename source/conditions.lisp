(in-package #:nohgl)

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

(define-condition vao-without-verts (error) ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Must supply vertices in order to draw primitives. Did you forget to pass in :verts?"))))

(define-condition vao-without-vertex-shader (error) ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Must supply a vertex shader (source-file) when creating a VAO instance. Did you forget to pass in :vertex-shader?"))))

(define-condition vao-without-fragment-shader (error) ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Must supply a fragment shader (source-file) when creating a VAO instance. Did you forget to pass in :fragment-shader?"))))

(define-condition context-already-exists (warning) ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Context already exists: ~s" (current-context)))))

(define-condition gpu-struct-not-found (error)
  ((struct-name :initarg :struct-name :initform nil :reader struct-name))
  (:report (lambda (condition stream)
             (format stream "GPU struct could not be found: ~s" (struct-name condition)))))

(define-condition gpu-type-does-not-exist (error)
  ((gpu-type :initarg :gpu-type :initform nil :reader gpu-type))
  (:report (lambda (condition stream)
             (format stream "GPU type does not exist: ~s" (gpu-type condition)))))

(define-condition shaded-object-not-found (error)
  ((object-name :initarg :object-name :initform nil :reader object-name))
  (:report (lambda (condition stream)
             (format stream "Shaded object could not be found: ~s" (object-name condition)))))
