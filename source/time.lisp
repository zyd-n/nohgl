(in-package #:nohgl)

(defvar *world-stats* nil)

(defclass world-clock ()
  ((duration :initform (error "Must supply a duration of some kind.") :initarg :duration :accessor duration)
   (ticks :accessor ticks :initform 0)
   (frames :accessor frames :initform 0.0)
   (last-frame :accessor last-frame :initform 0.0)
   (delta-time :accessor delta-time :initform 0.0)))

(defgeneric forward-time (clock))
(defgeneric log-time (clock))

(defun nsec (n)
  (local-time:timestamp+ (local-time:now) n :sec))

;; We currently confuse two things here: the creation of our world clock and
;; setting how often to log it.
(defun time-by (duration)
  (make-instance 'world-clock :duration duration))

(defmethod forward-time ((clock world-clock))
  (let ((now (glfw:time)))
    (with-accessors ((duration duration)
                     (ticks ticks)
                     (frames frames)
                     (last-frame last-frame)
                     (delta-time delta-time))
        clock
      (setf delta-time (- now last-frame))
      (setf last-frame now)
      (incf ticks)
      (incf frames)
      (when (local-time:timestamp>= (local-time:now) duration)
        (setf duration (nsec 1))
        (log-time clock)
        (decf frames frames))))
  clock)

(defun world-clock ()
  (clock (current-context)))

(defun dt ()
  (delta-time (world-clock)))

(defmethod log-time ((clock world-clock))
  (when *world-stats*
    (with-slots (ticks frames delta-time) clock
      (format t "Tick: ~s~%FPS: ~s~%Miliseconds: ~s~%Delta: ~s~%--------------------~%~%"
              ticks frames (/ 1000.0 frames) delta-time))))

(defun debug-with-time (&optional (enable t))
  (setf *world-stats* enable))
