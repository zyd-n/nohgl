(in-package #:nohgl)

(defun slots-with-initforms (slots)
  "Return a list of (initform value) for every slot that has an initform."
  (loop with bindings = ()
        for binding in slots
        do (alexandria:when-let ((binding* (second (member :initform binding))))
             (push (list (first binding) binding*) bindings))
        finally (return bindings)))

(defun initform-values (slots)
  "Return a list of the initform values of the given slots."
  (mapcar (lambda (slot-form)
            (second (member :initform slot-form)))
          slots))

(defun find-binding-reference (binding initform)
  "Return t if we can find a reference to the symbol BINDING in the INITFORM
expression."
  (block nil
    (labels ((recur (expr)
               (cond ((atom expr)
                      (when (eq binding expr)
                        (return-from nil t)))
                     (t (recur (car expr))
                        (recur (cdr expr))))))
      (recur initform))))

(defun binding-already-referenced (binding binding-set)
  (find binding binding-set :test 'eql))

(defun discard-unreferenced-bindings (bindings slots)
  "Remove any bindings that aren't actually referenced by a slot."
  (let ((final-bindings ())
        (initforms (initform-values slots)))
    (loop for binding in bindings
          for binding-name = (first binding)
          do (loop for initform in initforms
                   do (when (find-binding-reference binding-name initform)
                        (unless (binding-already-referenced binding final-bindings)
                          (push binding final-bindings))))
          finally (return final-bindings))))

(defun slots-for-let* (slots)
  "Return an expression suitable for LET*."
  (discard-unreferenced-bindings (slots-with-initforms slots) slots))

(defmacro defclass* (name direct-superclasses direct-slots &rest options)
  "Define a class the same as DEFCLASS but allows you to reference other slots as values in a given :initform, similar to LET*. Depends on the referenced slot also having an initform."
  `(let* ,(slots-for-let* direct-slots)
     (defclass ,name ,direct-superclasses
       ,direct-slots
       ,@options)))
