(in-package #:nohgl)

(define-render hello-shaders
    (:options (title "hello shaders, once again <3"))
    (:locals (c 0.0)
             (scale 0.0)
             (delta 0.005))
  (flet ((keep-house ()
           (gl:clear :color-buffer)
           (gl:clear-color 0 0 0 0)
           (gl:bind-buffer :array-buffer *vbo-handle*)
           (gl:enable-vertex-attrib-array 0)
           (gl:vertex-attrib-pointer 0 3 :float 0 0 0)
           (gl:draw-arrays :triangles 0 3)
           (gl:disable-vertex-attrib-array 0)
           (glfw:swap-buffers *g*)))
    (incf scale delta)
    (when (or (>= scale 1.0) (<= scale -1.0))
      (setf delta (* delta -1.0)))
    (let ((translation (fill-array 1.0 0.0 0.0 (* scale 2)
                                   0.0 1.0 0.0 scale
                                   0.0 0.0 1.0 0.0
                                   0.0 0.0 0.0 1.0)))
      (gl:uniform-matrix-4fv gscale-location translation))
    ;; (gl:clear-color c .1 .2 .6)
    ;; (setf c (if (>= c 1.0) 0.0
    ;;             (+ c (/ 1.0 200.0))))
    (keep-house)))
