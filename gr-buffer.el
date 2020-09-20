(require 'gr-source)
(require 'gr-core)

(defun gr-buffer-get-line ()
  (let* ((beg (point))
		 (end (line-end-position)))
	(buffer-substring-no-properties beg end)))

(defun gr-buffer-content ()
  (let* ((inhibit-field-text-motion t)
		 candidates)
	(save-excursion
	  (goto-char (point-min))
	  (while (< (point) (point-max))
		(let ((line (gr-buffer-get-line)))
		  (push line candidates))
		(forward-line)))
	(nreverse candidates)))

(defun gr-buffer-source ()
  (gr-make-source "gr-search-buffer" 'gr-source-sync :candidates (gr-buffer-content)))

;;;###autoload
(defun gr-buffer-search ()
  (interactive)
  (gr-core nil nil (gr-buffer-source) "*gr-buffer-search*"))

(provide 'gr-buffer)
