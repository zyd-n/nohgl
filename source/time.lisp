(in-package #:nohgl)

(defvar *world-stats* nil)

(defclass world-clock ()
  ((duration :initform (error "Must supply a duration of some kind.") :initarg :duration :accessor duration)
   (ticks :reader ticks :initform 0)
   (frames :reader frames :initform 0.0)))

(defgeneric forward-time (clock))
(defgeneric log-time (clock))

(defun nsec (n)
  (local-time:timestamp+ (local-time:now) n :sec))

(defun time-by (duration)
  (make-instance 'world-clock :duration duration))

(defmethod forward-time ((clock world-clock))
  (with-slots (duration ticks frames) clock
    (when (local-time:timestamp>= (local-time:now) duration)
      (setf (duration clock) (nsec 1))
      (incf ticks)
      (log-time clock)
      (decf frames frames))
    (incf frames))
  clock)

(defmethod log-time ((clock world-clock))
  (when *world-stats*
    (with-slots (ticks frames) clock
      (format t "Tick: ~s~%FPS: ~s~%Miliseconds: ~s~%--------------------~%~%" ticks frames (/ 1000.0 frames)))))

(defun debug-with-time (&optional (enable t))
  (setf *world-stats* enable))
