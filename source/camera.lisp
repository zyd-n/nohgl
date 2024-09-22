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
   (mouse-y :accessor mouse-y)
   (reuse-last-camera :initform nil :accessor reuse-last-camera)
   (fov :initform 45.0 :accessor fov)))

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

(defvar *input-stack* '())

(defun input-stack ()
  *input-stack*)

(defun (setf input-stack) (value)
  (setf *input-stack* value))

(defvar *keybinds* '(:w :a :s :d :p :left-control :space :button-5 :left-shift :right))

(defun current-keybinds ()
  *keybinds*)

(defun (setf current-keybinds) (value)
  (setf *keybinds* value))

(defun update-input-stack (key)
  (labels ((key-active? (k)
             (or (eq (ignore-errors (glfw:key-state k (current-context))) :press)
                 (eq (ignore-errors (glfw:key-state k (current-context))) :repeat)
                 (eq (ignore-errors (glfw:mouse-button-state k (current-context))) :press)))
           (keybinding? (key keybinding)
             (eq key keybinding)))
    (loop for keybinding in (current-keybinds)
          do (when (keybinding? key keybinding)
               (if (key-active? keybinding)
                   (unless (member keybinding (input-stack))
                     (push key (input-stack)))
                   (setf (input-stack) (remove keybinding (input-stack))))))))

(defmethod glfw:key-changed ((window g) key scan-code action modifiers)
  (update-input-stack key)
  (when (eq key :escape) (quit)))

(defun print-camera ()
  (let ((c (camera (current-context))))
    (format t "~%---------------------------------------------------------~%Camera Position: ~s~%Camera Target:   ~s~%---------------------------------------------------------~%~%~%~%~%~%~%~%~%~%~%~%~%"
            (camera-position c)
            (camera-target c))))

(defun move-up ()
  (let ((c (camera (current-context))))
    (setf (camera-position c)
          (v+ (camera-position c)
              (vec3 0.0 0.04 0.0)))))

(defun move-down ()
  (let ((c (camera (current-context))))
    (setf (camera-position c)
          (v- (camera-position c)
              (vec3 0.0 0.04 0.0)))))

;; TODO: Fix hardcoded values
;; TODO: Account for when our mouse moves outside of the window and then comes back.
(defmethod glfw:mouse-moved ((window g) xpos ypos)
  (unless (user-left-window window)
    (with-accessors ((yaw camera-yaw) (pitch camera-pitch) (sens camera-sens) (target camera-target) (x mouse-x) (y mouse-y) (reuse-last-camera reuse-last-camera))
        (camera (current-context))
      (let ((yaw+ (+ yaw (* sens (- xpos (if reuse-last-camera xpos x)))))
            (pitch+ (limit (+ pitch (* sens (- (if reuse-last-camera ypos y) ypos)))
                           '(-89.0 -89.0)
                           '(89.0 89.0))))
        (setf target (vec3 (* (cos (radian yaw+)) (cos (radian pitch+)))
                           (sin (radian pitch+))
                           (* (sin (radian yaw+)) (cos (radian pitch+)))))
        (setf x xpos)
        (setf y ypos)
        (setf yaw yaw+)
        (setf pitch pitch+)
        (when reuse-last-camera (setf (reuse-last-camera (camera (current-context))) nil))))))
