(defclass gr-source ()
  ((check-before-compute
	:initarg :check-before-compute
	:initform nil
	:custom function
	:documentation
	"计算candidates之前的校验，如：只有输入达到一定长度时，`gr-rg'才会执行")
   (render-line
	:initarg :render-line
	:initform nil
	:custom function)
   (cleanup
	:initarg :cleanup
	:initform nil
	:custom function))
  :abstract t)

(defclass gr-source-sync (gr-source)
  ((candidates
	:initarg :candidates
	:initform nil
	:custom list)))

(defclass gr-source-async (gr-source)
  ((candidates-process
	:initarg :candidates-process
	:initform nil
	:custom function)))

(defun gr-make-source (class &rest args)
  (apply #'make-instance class args))

(provide 'gr-source)
