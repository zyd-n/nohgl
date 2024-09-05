(in-package #:nohgl)

;; aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
;; aaaaaaaaaaaaaa everything is awful in this file aaaaaaaaaaaaaaaaaaaa
;; aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa

(defclass camera ()
  ((camera-position :accessor camera-position)
   (camera-target :accessor camera-target)
   (camera-up :accessor camera-up)
   (camera-right :accessor camera-right)
   (camera-view :accessor camera-view)
   (camera-yaw :initform -90.0 :accessor camera-yaw)
   (camera-pitch :initform 0.0 :accessor camera-pitch)
   (camera-speed :initform 10.0 :accessor camera-speed)
   (camera-sens :initform 0.09 :accessor camera-sens)
   (mouse-x :accessor mouse-x)
   (mouse-y :accessor mouse-y)))

(defmethod initialize-instance :after ((camera camera) &key)
  (with-accessors ((position camera-position) (target camera-target) (up camera-up) (right camera-right) (view camera-view) (speed camera-speed) (x mouse-x) (y mouse-y)) camera
    (labels ((v-position () (vec3 0.0 0.0 3.0))
             (v-target () (v- (v-position) (vec3 0.0 0.0 0.0)))
             (v-right () (vc (vec3 0.0 1.0 0.0) (v-target)))
             (v-up () (vc (v-target) (v-right))))
      (setf position (v-position))
      (setf target (v-target))
      (setf right (v-right))
      (setf up (v-up)))))

(defvar *key-stack* '())

(defmethod glfw:key-changed ((window g) key scan-code action modifiers)
  (labels ((key-active? (k)
             (or (eq (glfw:key-state k window) :press)
                 (eq (glfw:key-state k window) :repeat)))
           (update-keys ()
             (loop for k in '(:w :a :s :d)
                   do (when (eq key k)
                        (if (key-active? k)
                            (unless (member k *key-stack*)
                              (push key *key-stack*))
                            (setf *key-stack* (remove k *key-stack*)))))))
    (update-keys)
    (when (eq key :escape) (quit))))

;; TODO: Fix hardcoded values
;; TODO: Account for when our mouse moves outside of the window and then comes back.
(defmethod glfw:mouse-moved ((window g) xpos ypos)
  (labels ((pitch-limits ()
             (values (lambda () -89.0) (lambda () 89.0))))
    (with-accessors ((yaw camera-yaw) (pitch camera-pitch) (sens camera-sens) (target camera-target) (x mouse-x) (y mouse-y))
        (camera (current-context))
      (let ((yaw+ (+ yaw (* sens (- xpos x))))
            (pitch+ (limit (+ pitch (* sens (- y ypos)))
                           :limiters (pitch-limits)
                           :min -89.0
                           :max 89.0)))
        (setf target (vec3 (* (cos (radian yaw+)) (cos (radian pitch+)))
                           (sin (radian pitch+))
                           (* (sin (radian yaw+)) (cos (radian pitch+)))))
        (setf x xpos)
        (setf y ypos)
        (setf yaw yaw+)
        (setf pitch pitch+)))))
