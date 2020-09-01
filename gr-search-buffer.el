;; search in current buffer

(require 'gr-core)

;;;###autoload
(defun gr-search-buffer ()
  (interactive)
  (gr-core "Search: " nil (gr-buffer-content)))

;; test search buffer
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

;; test
(gr-search-buffer)
