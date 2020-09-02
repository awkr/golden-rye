;;; gr-source.el

;;; Code:

(defclass gr-source ()
  ((name
	:initarg :name
	:initform nil
	:custom string)
   (candidates
	:initarg :candidates
	:initform nil
	:custom list))
  :abstract t)

(defclass gr-source-sync (gr-source)
  ((candidates
	:initform '("error: you must specify the `candidates' slot, either with a list or a function"))))

(defclass gr-source-async (gr-source)
  ((candidates-process
	:initarg :candidates-process
	:initform nil
	:custom function)))

(defun gr-make-source (name class &rest args)
  (declare (indent 2))
  (let ((source (apply #'make-instance class name args)))
	(setf (slot-value source 'name) name)
	(gr--create-source source)))

;;; internal functions

(defun gr--create-source (object)
  (cl-loop for slot in (object-slots object)
		   for val = (slot-value object slot)
		   when val
		   collect (cons slot val)))

(provide 'gr-source)

;;; gr-source.el ends here
