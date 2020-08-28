;; gr means Golden Rye, in Chinese: 金色麦田
;; grf means Golden Rye File

(defvar grf-last-window-config nil)
(defvar grf-display-buffer-height 15)

(defcustom grf-save-window-config-functions '(set-window-configuration . current-window-configuration)
  ""
  :group 'grf
  :type 'sexp)

(defun grf-window-config (save-or-restore)
  (cl-case save-or-restore
	(save (setq grf-last-window-config
				(funcall (cdr grf-save-window-config-functions))))
	(restore (funcall (car grf-save-window-config-functions) grf-last-window-config))))

(defvar grf-buffer "grf-buffer")

(defun grf-keyboard-quit ()
  (interactive)
  (abort-recursive-edit))

(defun grf-keyboard-enter ()
  (interactive)
  (exit-minibuffer))

(defgroup grf nil
  ""
  :prefix "grf-" :group 'convenience)

(defvar grf-map
  (let ((map (make-sparse-keymap)))
	(set-keymap-parent map minibuffer-local-map)
	(define-key map (kbd "C-g") 'grf-keyboard-quit)
	(define-key map (kbd "<RET>") 'grf-keyboard-enter)
	map)
  "keymap for grf")

(define-minor-mode grf-minor-mode
  ""
  :group 'grf :keymap grf-map)

(cl-defun grf-prevent-switching-other-window (&key (enabled t))
  (walk-windows
   (lambda (w)
	 (unless (window-dedicated-p w) (set-window-parameter w 'no-other-window enabled)))
   0))

(defun grf-cleanup ()
  (grf-prevent-switching-other-window :enabled nil)
  (grf-window-config 'restore))

(defun grf-window ()
  (get-buffer-window grf-buffer 0))

;;;###autoload
(defun grf ()
  (get-buffer-create grf-buffer)
  (grf-window-config 'save)
  (let* ((split-window-preferred-function #'(lambda (window)
											  (split-window window nil 'below))))
	(display-buffer grf-buffer `(nil (window-height . ,grf-display-buffer-height) (window-width . 0)))
	(select-window (grf-window))
	(grf-prevent-switching-other-window))
  (unwind-protect
	  (minibuffer-with-setup-hook
		  (lambda ()
			(grf-minor-mode 1))
		(read-from-minibuffer (propertize "pattern: ")))
	(progn
	  (grf-cleanup)
	  ))
  )

(grf)
