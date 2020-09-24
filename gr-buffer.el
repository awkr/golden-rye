(require 'gr-source)
(require 'gr-core)

(defun gr-buffer-content ()
  (let* ((content (buffer-substring-no-properties (point-min) (point-max)))
	   (lines (split-string (string-trim content) "\n")))
  lines))

(defun gr-buffer-make-source ()
  (gr-make-source 'gr-source-sync :candidates (gr-buffer-content)))

;;;###autoload
(defun gr-buffer-search ()
  (interactive)
  (gr-core nil nil (gr-buffer-make-source) "*gr-b-search*"))

(provide 'gr-buffer)
