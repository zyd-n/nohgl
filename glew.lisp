(in-package #:glew)

(define-foreign-library glew
  (t (:default "libGLEW")))

(use-foreign-library glew)

(autowrap:c-include "glew.h")
