(in-package #:nohgl)

(defun hello-shaders ()
  (start 'hello-shaders :title "Hello, Shaders!" :width 900 :height 600))

(defvar [m] (mat4 1.0 0.0 0.0 0.0
                  0.0 1.0 0.0 0.0
                  0.0 0.0 1.0 0.0
                  0.0 0.0 0.0 1.0))

(defun draw-vertex (i)
  (gl:bind-buffer :array-buffer *vbo-handle*)
  (gl:enable-vertex-attrib-array i)
  (gl:vertex-attrib-pointer i 3 :float 0 0 0)
  (gl:draw-arrays :triangles 0 3)
  (gl:disable-vertex-attrib-array i))

(define-render hello-shaders
    ((scale 0.0)
     (delta 0.005))
  (let ((m (marr (tmat (tscale-by (tfrom-mat [m]) 1.0 (* scale 0.9) 1.0)))))
    (labels ((update-vertex-position (location mtransform)
               (gl:uniform-matrix-4fv location mtransform)
               (setf scale (+ scale delta)
                     delta (if (or (>= scale 1.0) (<= scale -1.0))
                               (* delta -1.0)
                               delta))))
      (update-vertex-position gscale-location m)
      (draw-vertex 0)
      (glfw:swap-buffers *g*)
      (gl:clear :color-buffer))))

