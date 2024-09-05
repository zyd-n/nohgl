(in-package #:nohgl)

(defclass binding ()
  ((name :initarg :name :accessor name)
   (initform :initarg :initform :accessor initform)
   (initarg :initarg :initarg :accessor initarg)
   (accessor :initarg :accessor :accessor accessor)))

(defun make-binding (name &key (initform nil)
                               (initarg (alexandria:make-keyword name))
                               (accessor name))
  (make-instance 'binding :name name
                          :initform initform
                          :initarg initarg
                          :accessor accessor))

(defun parse-bindings (binding-forms)
  (loop :for (name value) in binding-forms
        :collect (make-binding name :initform value)))

(defun +defclass (name bindings)
  `(defclass ,name (g)
     (,@(loop :for binding in bindings
              :collect `(,(name binding)
                         :initarg ,(initarg binding)
                         :accessor ,(accessor binding)))
      (camera :initform (make-instance 'camera) :accessor camera))))

(defun +prepare (name bindings)
  `(defmethod prepare ((render ,name)
                       &key ,@(loop for b in bindings
                                    collect `((,(initarg b) ,(name b)) ,(initform b)))
                       &allow-other-keys)
     (setf ,@(loop for b in bindings
                   collect `(,(accessor b) render)
                   collect (name b)))))

(defun +draw (name bindings body)
  `(defmethod draw ((render ,name))
     (with-accessors (,@(loop for b in bindings
                              collect `(,(name b) ,(accessor b))))
         render
       ,@body)))

(defmacro define-render (name locals &body body)
  (let ((bindings (parse-bindings locals)))
    `(progn ,(+defclass name bindings)
            ,(+prepare name bindings)
            ,(+draw name bindings body)
            (make-instances-obsolete ',name)
            (find-class ',name))))
