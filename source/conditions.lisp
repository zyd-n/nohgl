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
