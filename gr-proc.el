;;; gr-proc.el --- -*- lexical-binding: t -*-

;; Copyright (C) 2020 Hongjian Zhu <zhu.life@gmail.com>

;;; Code:

(defun gr-proc-output (exe &rest args)
  (with-temp-buffer
	(let ((proc (make-process :name gr-rg-proc-name
							  :buffer (current-buffer)
							  :command `(,exe ,@args)
							  :noquery t
							  :sentinel #'ignore)))
	  (set-process-query-on-exit-flag proc nil)
	  (while (accept-process-output proc nil nil t)))
	(buffer-string)))

(provide 'gr-proc)
;;; gr-proc.el ends here
