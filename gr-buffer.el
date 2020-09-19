;;; gr-buffer.el --- search in current buffer -*-lexical-binding: t-*-

;; Copyright (C) 2020 Hongjian Zhu <zhu.life@gmail.com>

;; Version: 20200919

(require 'gr-source)
(require 'gr-core)

(defconst gr-buffer--gr-buffer-name "*gr-search-buffer*")

;;;###autoload
(defun gr-buffer-search ()
  (interactive)
  (gr-core nil nil (gr-buffer-source) gr-buffer--gr-buffer-name))

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

;; ;; test
;; (gr-buffer-search)

(provide 'gr-buffer)
;;; gr-buffer.el ends here
