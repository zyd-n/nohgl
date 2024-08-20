(in-package #:nohgl)

(defclass texture ()
  ((data :initarg :data :initform nil :accessor data)
   (width :initarg :width :initform nil :accessor width)
   (height :initarg :height :initform nil :accessor height)))

;; this works:
;; (pngload:with-png-in-static-vector (pn #p"lain.png" :flip-y t)
;;   (pngload:data pn))

(defgeneric make-texture (png))
(defmethod make-texture ((png pathname))
  (pngload:with-png-in-static-vector (png-source png :flip-y t)
    (make-instance 'texture :width (pngload:width png-source)
                            :height (pngload:height png-source)
                            :data (static-vectors:static-vector-pointer (pngload:data png-source)))))

(defgeneric generate-texture (texture))
(defmethod generate-texture ((texture texture))
  (with-slots (width height data) texture
    (let ((texture-id (gl:gen-texture)))
      (gl:bind-texture :texture-2d texture-id)
      (gl:tex-parameter :texture-2d :texture-wrap-s :repeat)
      (gl:tex-parameter :texture-2d :texture-wrap-t :repeat)
      (gl:tex-parameter :texture-2d :texture-min-filter :linear-mipmap-linear)
      (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
      (gl:tex-image-2d :texture-2d 0 :rgb width height 0 :rgba :unsigned-byte data)
      (gl:generate-mipmap :texture-2d)
      texture-id)))
