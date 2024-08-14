(in-package #:nohgl)

(defun hello-shaders ()
  (start 'hello-shaders :title "Hello, Shaders!" :width 900 :height 600))

(defun draw-vertex (i)
  (gl:bind-buffer :array-buffer *vbo-handle*)
  (gl:enable-vertex-attrib-array i)
  (gl:vertex-attrib-pointer i 3 :float 0 0 0)
  (gl:draw-arrays :triangles 0 3)
  (gl:disable-vertex-attrib-array i))

(define-render hello-shaders
    ((color 0.0)
     (scale 0.0)
     (delta 0.005))
  (gl:clear-color .09 .09 .09 0)
  (labels ((update-vertex-position (location mtransform)
             (gl:uniform-matrix-4fv location mtransform)
             (setf scale (+ scale delta)
                   delta (if (or (>= scale 1.0) (<= scale -1.0))
                             (* delta -1.0)
                             delta)))
           (cycle-color (r g b &optional (a 1.0))
             (gl:clear-color r g b a)
             (if (>= color 1.0)
                 (setf color 0.0)
                 (incf color (/ 1.0 255.0))))
           (clear-buffer ()
             (gl:clear :color-buffer)
             (glfw:swap-buffers *g*)))
    (let* ((m (mat4 1.0 0.0 0.0 0.0
                    0.0 1.0 0.0 0.0
                    0.0 0.0 1.0 0.0
                    0.0 0.0 0.0 1.0))
           (a (tfrom-mat m)))
      (update-vertex-position gscale-location (marr (tmat (tscale-by a 1.0 (* scale 0.9) 1.0))))
      (draw-vertex 0)
      (glfw:swap-buffers *g*)
      (gl:clear :color-buffer))))

