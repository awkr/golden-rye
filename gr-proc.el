(defun gr-proc-output (exe &rest args)
  (with-temp-buffer
	(let ((proc (make-process :name "*gr-temp-proc*"
							  :buffer (current-buffer)
							  :command `(,exe ,@args)
							  :noquery t
							  :sentinel #'ignore)))
	  (set-process-query-on-exit-flag proc nil)
	  (while (accept-process-output proc nil nil t)))
	(buffer-string)))

(defun gr-git-root ()
  (gr-proc-output "/usr/local/bin/git" "rev-parse" "--show-toplevel"))

(provide 'gr-proc)
