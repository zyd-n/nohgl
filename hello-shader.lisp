(in-package #:learngl)

(define-render hello-shaders
    ((c 0.0))
  (flet ((keep-house ()
           (gl:clear :color-buffer)
           (gl:bind-buffer :array-buffer *vbo-handle*)
           (gl:enable-vertex-attrib-array 0)
           (gl:vertex-attrib-pointer 0 3 :float 0 0 0)
           (gl:draw-arrays :triangles 0 3)
           (gl:disable-vertex-attrib-array 0)
           (glfw:swap-buffers *g*)))
    (gl:clear-color c .1 .2 .6)
    (setf c (if (>= c 1.0) 0.0
                (+ c (/ 1.0 256.0))))
    (keep-house)))
