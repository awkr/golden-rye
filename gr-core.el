;;; gr-core.el --- kernal of gr. -*- lexical-binding: t -*-

;; Copyright (C) 2020 Hongjian Zhu <zhu.life@gmail.com>

;; gr means Golden Rye, in Chinese: 金色麦田

;;; Code:

(defvar gr-buffer "*Golden Rye*")
(defvar gr-last-window-config nil)
(defvar gr-display-buffer-height 13)
(defvar gr-pattern "")
(defvar gr-source nil
  "a list of candidates")
(defvar gr-in-update nil)

(defgroup gr nil
  ""
  :prefix "gr-" :group 'convenience)

(defcustom gr-save-window-config-functions '(set-window-configuration . current-window-configuration)
  ""
  :group 'gr
  :type 'sexp)

(defun gr-window-config (save-or-restore)
  (cl-case save-or-restore
	(save (setq gr-last-window-config
				(funcall (cdr gr-save-window-config-functions))))
	(restore (funcall (car gr-save-window-config-functions) gr-last-window-config))))

(defun gr-keyboard-quit ()
  (interactive)
  (abort-recursive-edit))

(defun gr-keyboard-enter ()
  (interactive)
  (exit-minibuffer))

(defun gr-previous-line ()
  (interactive)
  (with-gr-window
   (forward-line -1)
   (gr-mark-current-line)))

(defun gr-next-line ()
  (interactive)
  (with-gr-window
   (forward-line 1)
   (gr-mark-current-line)))

(defun gr-previous-page ()
  (interactive))

(defun gr-next-page ()
  (interactive))

(defvar gr-map
  (let ((map (make-sparse-keymap)))
	(set-keymap-parent map minibuffer-local-map)
	(define-key map (kbd "C-g") 'gr-keyboard-quit)
	(define-key map (kbd "<RET>") 'gr-keyboard-enter)
	(define-key map (kbd "C-p") 'gr-previous-line)
	(define-key map (kbd "C-n") 'gr-next-line)
	(define-key map (kbd "M-v") 'gr-previous-page)
	(define-key map (kbd "C-v") 'gr-next-page)
	map)
  "keymap for gr")

(define-minor-mode gr-minor-mode
  ""
  :group 'gr :keymap gr-map)

(cl-defun gr-prevent-switching-other-window (&key (enabled t))
  (walk-windows
   (lambda (w)
	 (unless (window-dedicated-p w) (set-window-parameter w 'no-other-window enabled)))
   0))

(defun gr-cleanup ()
  (gr-prevent-switching-other-window :enabled nil)
  (gr-window-config 'restore))

(defun gr-window ()
  (get-buffer-window gr-buffer 0))

(defmacro with-gr-buffer (&rest body)
  "eval BODY inside gr-buffer"
  `(with-current-buffer gr-buffer
	 ,@body))

(defmacro with-gr-window (&rest body)
  `(with-selected-window (gr-window)
	 ,@body))

(defun gr-create-gr-buffer ()
  (let ((inhibit-read-only t))
	(with-current-buffer (get-buffer-create gr-buffer)
	  (kill-all-local-variables)
	  (set (make-local-variable 'buffer-read-only) nil)
	  (buffer-disable-undo)
	  (erase-buffer)
	  (make-local-variable 'gr-source)
	  (setq require-final-newline nil)
	  (setq cursor-type nil))
	(gr-init-overlays gr-buffer)
	(get-buffer gr-buffer)))

(defvar gr-selection-overlay nil
  "overlay used to highlight the currently selected item")

(defgroup gr-faces nil
  ""
  :prefix "gr-"
  :group 'faces
  :group 'gr)
;; (defface gr-selection
;;   `((t ,@(and (>= emacs-major-version 27) '(:extend t))
;; 	   :inherit highlight :distant-foreground "black"))
;;   ""
;;   :group 'gr-faces)

(defface gr-selection
  `((((background dark))
	 :extend t
	 :background "ForestGreen"
	 :distant-foreground "black")
	(((background light))
	 :extend t
	 :background "#b5ffd1"
	 :distant-foreground "black"))
  "face for currently selected item in the gr buffer"
  :group 'gr-faces)

(defun gr-init-overlays (buffer)
  (setq gr-selection-overlay (make-overlay (point-min) (point-min) (get-buffer buffer)))
  (overlay-put gr-selection-overlay 'face 'gr-selection)
  (overlay-put gr-selection-overlay 'priority 1))

(defun gr-core (&optional prompt input source filter)
  (gr-create-gr-buffer)
  (gr-window-config 'save)
  (let* ((split-window-preferred-function #'(lambda (window)
											  (split-window window nil 'below))))
	(display-buffer gr-buffer `(nil (window-height . ,gr-display-buffer-height) (window-width . 0)))
	(select-window (gr-window))
	(gr-prevent-switching-other-window))

  (setq gr-source source)

  ;; display source at the very beginning
  (gr-render)
  (gr-move-to-first-line)

  (unwind-protect
	  (gr-read-pattern prompt input)
	(gr-cleanup)))

(defun gr-read-pattern (&optional prompt input)
  (with-gr-buffer
   (let* (timer)
	 (unwind-protect
		 (minibuffer-with-setup-hook
			 (lambda ()
			   (gr-minor-mode 1)
			   (setq timer
					 (run-with-idle-timer
					  0.01
					  'repeat
					  (lambda ()
						(save-selected-window (gr-check-minibuffer-input))))))
		   (read-from-minibuffer (propertize (or prompt "pattern: ")) input nil nil nil nil t))
	   (when timer
		 (cancel-timer timer)
		 (setq timer nil))))))

(defun gr-check-minibuffer-input ()
  (with-selected-window (or (active-minibuffer-window)
							(minibuffer-window))
	(let* ((input (minibuffer-contents)))
	  (unless (equal input gr-pattern)
		(setq gr-pattern input)
		(setq gr-in-update t)
		(gr-update)))))

(defun gr-update ()
  (with-gr-buffer
   (unwind-protect
	   (let* ((matches (gr-core-search-in-list gr-source gr-pattern)))
		 (gr-render matches)
		 (gr-move-to-first-line))
	 (setq gr-in-update nil))))

(defun gr-render (&optional matches)
  (erase-buffer)
  (cond (matches
		 (gr-render-matches matches))
		((or (null gr-pattern)
			 (zerop (length gr-pattern)))
		 (gr-render-matches gr-source))))

(defun gr-move-to-first-line ()
  "goto first line of `gr-buffer'"
  (goto-char (point-min))
  (gr-mark-current-line))

(defun gr-mark-current-line ()
  (with-gr-buffer
   (move-overlay gr-selection-overlay (point-at-bol) (1+ (point-at-eol)))))

(defun gr-render-matches (matches)
  (cl-loop for m in matches
  		   do (gr-insert-match m)))

(defun gr-insert-match (match)
  (insert match "\n"))

(defun gr-core-search-in-list (source pattern)
  (let ((matched (cl-loop for s in source
					  when (gr-core-is-str-match-pattern s pattern)
					  collect s)))
	matched))

(defun gr-core-is-str-match-pattern (str pattern)
  ;; if matches return t, otherwise return nil
  ;; 按照空格拆分出各匹配项，再逐个比较
  (let* ((idx 0)
  		 (patterns (split-string pattern " ")))
  	(cl-loop for p in patterns
  			 when (not (string= p ""))
  			 do
  			 (when (not (null idx))
  			   (setq idx (string-match p str idx))))
  	(if (not idx) nil
	  t)))

(provide 'gr-core)
;;; gr-core.el ends here

;; test
(gr-core "" "a" '("hey" "hello" "jack" "apple" "peter"))
(gr-core "Search: " "" '("hey" "hello" "jack" "apple" "peter"))
(gr-core nil nil '("hey" "hello" "jack" "apple" "peter"))
