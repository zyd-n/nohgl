(in-package #:nohgl)

(defclass camera-modifiers ()
  ((yaw   :initarg :yaw   :accessor yaw)
   (pitch :initarg :pitch :accessor pitch)
   (rate  :initarg :rate  :accessor rate)
   (sens  :initarg :sens  :accessor sens)))

(defclass camera-space ()
  ((location  :accessor location)
   (target    :accessor target)
   (up        :accessor up)
   (right     :accessor right)
   (view      :accessor view)
   (fov       :initarg :fov :accessor fov)))

(defclass camera (camera-modifiers camera-space)
  ((%reuse :initform NIL :accessor %reuse))
  (:default-initargs
   :yaw -90
   :pitch 0
   :rate 10
   :sens .09
   :fov 90))

(defmethod initialize-instance :after ((camera camera) &key)
  (setf (location camera) (vec 0 0 3))
  (setf (target camera)   (vec 0 0 3))
  (setf (right camera)    (vec 3 0 0))
  (setf (up camera)       (vec 0 9 0)))

(defun print-camera ()
  (let ((c (camera (current-context))))
    (format t "~%~A~%Position: ~S~%Target:   ~S~%~A~%~%~%~%~%~%~%~%~%~%~%~%~%"
            (printn 57 #\-)
            (location c)
            (target c)
            (printn 57 #\-))))

(defun move-up ()
  (nv+ (location (camera (current-context)))
       (vec 0 .04 0)))

(defun move-down ()
  (nv- (location (camera (current-context)))
       (vec 0 .04 0)))

(defun yaw+ (camera x)
  (+ (yaw camera)
     (* (sens camera)
        (- x (if (%reuse camera) x
                 (x (mouse-location (current-context))))))))

(defun pitch+ (camera y)
  (let ((base-y (if (%reuse camera) y
                    (y (mouse-location (current-context))))))
    (limit -89 89
      (+ (pitch camera)
         (* (sens camera)
            (- base-y y))))))

(defun direction (yaw pitch)
  (vec (* (cos (radian yaw)) (cos (radian pitch)))
       (sin (radian pitch))
       (* (sin (radian yaw)) (cos (radian pitch)))))

(defmethod glfw:mouse-moved ((window g) xpos ypos)
  (unless (out-of-focus window)
    (let* ((camera (camera (current-context)))
           (yaw (yaw+ camera xpos))
           (pitch (pitch+ camera ypos))
           (target (direction yaw pitch)))
      (setf (target camera) target)
      (setf (yaw camera) yaw)
      (setf (pitch camera) pitch)
      (setf (x (mouse-location window)) xpos)
      (setf (y (mouse-location window)) ypos)
      (when (%reuse camera)
        (setf (%reuse camera) NIL)))))

(defun aspect-ratio ()
  (/ (glfw:width (current-context)) (glfw:height (current-context))))

(defun view-projection ()
  (with-slots (location target up fov) (camera (current-context))
    (let ((view (mlookat location (v+ location target) up))
          (projection (mperspective fov (aspect-ratio) 0.1 100.0)))
      (values view projection))))
