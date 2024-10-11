(in-package #:nohgl)

;; TODO: Move all uniform related code here.
;; TODO: Rethink/Rewrite uniform initialization and reinitialization.

(defun glsl-name (uniform-symbol)
  (substitute #\_ #\- (string-downcase (format nil "~a" uniform-symbol))))

(defun uniform-name-value-pairs (instance)
  (let ((slots (remove 'vao (slots-of instance :all))))
    (mapcar (lambda (slot)
              (list (glsl-name slot)
                    (slot-value instance slot)))
            slots)))

(defun view-projection->uniform ()
  (multiple-value-bind (view projection)
      (view-projection)
    `(("view" ,view) ("projection" ,projection))))

(defun upload-uniforms (vao uniforms-with-data)
  (loop for (uniform data) in uniforms-with-data
        do (let ((uniform-location (gl:get-uniform-location (program (get-vao vao)) uniform)))
             (typecase data
               (float (gl:uniformf uniform-location data))
               (vec3 (cffi:with-pointer-to-vector-data (ptr (varr data))
                       (%gl:uniform-3fv uniform-location 1 ptr)))
               (mat4 (cffi:with-pointer-to-vector-data (ptr (marr data))
                       (%gl:uniform-matrix-4fv uniform-location 1 :false ptr)))))))
