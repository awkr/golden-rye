(defun gr-proc-output (exe &rest args)
  (with-temp-buffer
	(let ((proc (make-process :name "*temp-gr-proc-name*"
							  :buffer (current-buffer)
							  :command `(,exe ,@args)
							  :noquery t
							  :sentinel #'ignore)))
	  (set-process-query-on-exit-flag proc nil)
	  (while (accept-process-output proc nil nil t)))
	(buffer-string)))

(defun gr-output-filter ())

(provide 'gr-proc)
