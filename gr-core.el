;; gr means Golden Rye, in Chinese: 金色麦田

(require 'cl-lib)
(require 'subr-x)
(require 'gr-source)

(defconst gr-buffer "*gr*")

(defvar gr-current-window nil
  "current window where `gr' is invoked")
(defvar gr-last-window-config nil)
(defvar gr-buffer-height 15)
(defvar gr-pattern "")
(defvar gr-source nil)
(defvar gr-in-update nil)

(defvar gr-candidates-index 0)
(defvar gr-candidates-len 0)

(defvar gr--proc nil
  "cons. car is proc, cdr is candidates")

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
  (when gr--proc
	(let* ((proc (car gr--proc)))
	  (gr-log "clean proc before quit")
	  (delete-process (car gr--proc)))
	(setq gr--proc nil))
  (exit-minibuffer))

(defun gr-keyboard-enter ()
  (interactive)
  (exit-minibuffer))

(defun gr-previous-line ()
  (interactive)
  (gr-forward-and-mark-line -1))

(defun gr-next-line ()
  (interactive)
  (gr-forward-and-mark-line 1))

(cl-defun gr-forward-and-mark-line (linum)
  (with-gr-window
   ;; (gr-log "movement index %d total %d" gr-candidates-index gr-candidates-len)
   (cond ((> linum 0) ;; move down
		  ;; 下越界
		  (let* ((ln-idx (what-line)))
			(save-excursion
			  (let ((inhibit-field-text-motion t))
				(end-of-buffer)
				(when (equal (what-line) ln-idx)
				  (cl-return-from gr-forward-and-mark-line)))))
		  (gr--forward-and-mark-line linum))
		 ((< linum 0) ;; move up
		  ;; 上越界
		  (when (or (equal (what-line) "Line 1")
					(and (gr-source-async-p gr-source)
						 (equal (what-line) "Line 2")))
			(cl-return-from gr-forward-and-mark-line))
		  (gr--forward-and-mark-line linum)))))

(defun gr--forward-and-mark-line (linum)
  (cond ((gr-source-sync-p gr-source)
		 (forward-line linum)
		 (setq gr-candidates-index (+ gr-candidates-index linum))
		 (gr-update-modeline (gr-modeline-index gr-candidates-index gr-candidates-len))
		 (gr-mark-current-line))
		((gr-source-async-p gr-source)
		 ;; todo 文件行不更新modeline的index [done]
		 ;; todo 打开文件：处于当前文件内容行时，按住组合键C-x f

		 (forward-line linum)
		 ;; 移动时自动略过文件
		 (when (gr-rg-line-file-p (gr-line-at-point))
		   (forward-line (if (> linum 0) 1 -1)))
		 (setq gr-candidates-index (+ gr-candidates-index linum))
		 (gr-update-modeline (gr-modeline-index gr-candidates-index gr-candidates-len))
		 (gr-mark-current-line))))

(defun gr-line-at-point ()
  (buffer-substring-no-properties (point-at-bol) (point-at-eol)))

(defun gr-edge-of-buffer-p (n)
  "return non-nil if we are at EOB or BOB"
  (save-excursion
	(forward-line n)
	(or (eq (point-at-bol) (point-at-eol))
		(equal (what-line) "Line 1"))))

(defun gr-previous-page ()
  (interactive)
  ;; (with-gr-window
  ;;  (scroll-down)
  ;;  (gr-mark-current-line))
  (gr-forward-and-mark-line (* (1- gr-buffer-height) -1))
  )

(defun gr-next-page ()
  (interactive)
  ;; (with-gr-window
  ;;  (scroll-up)
  ;;  (gr-mark-current-line))
  (gr-forward-and-mark-line (1- gr-buffer-height))
  )

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
  ;; (walk-windows
  ;;  (lambda (w)
  ;; 	 (unless (window-dedicated-p w) (set-window-parameter w 'no-other-window enabled)))
  ;;  0)
  )

(defun gr-cleanup ()
  (let* ((cleanup (oref gr-source cleanup)))
	(when cleanup
	  (funcall cleanup)))

  (gr-prevent-switching-other-window :enabled nil)
  (gr-window-config 'restore)

  (setq gr-source nil))

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
  (with-current-buffer (get-buffer-create gr-buffer)
	(kill-all-local-variables)
	(set (make-local-variable 'buffer-read-only) nil)
	(buffer-disable-undo)
	(erase-buffer)
	(make-local-variable 'gr-source)
	(setq require-final-newline nil)
	(setq buffer-read-only t)
	(setq cursor-type nil))
  (gr-init-overlays gr-buffer)
  (get-buffer gr-buffer))

(defvar gr-selection-overlay nil
  "overlay used to highlight the currently selected item")

(defgroup gr-faces nil
  ""
  :prefix "gr-"
  :group 'faces
  :group 'gr)

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

(defun gr-get-window-to-split (_w)
  "for making eyes comfortable, `gr' prefer vertically align for making eyes moving
the shorest distance and confident that `gr' will always appear in the same place"
  (let* (split-width-threshold)
	(cond ((one-window-p t)
		   (split-window (selected-window) nil 'below))
		  (t ;; more than one window
		   (let* ((w (gr-get-edge-window-in-direction (selected-window) 'below)))
			 (if (> (window-body-height w) (1- gr-buffer-height))
				 (split-window w nil 'below)
			   w))))))

(defun gr-get-edge-window-in-direction (w direction)
  (let* ((win (window-in-direction direction w)))
	(if (null win)
		w
	  (gr-get-edge-window-in-direction win direction))))

(defun gr-display-buffer (buffer)
  (let* ((split-window-preferred-function 'gr-get-window-to-split))
	(display-buffer buffer `(nil (window-height . ,gr-buffer-height) (window-width . 0)))
	(select-window (gr-window))
	(gr-prevent-switching-other-window)))

(defun gr-core (&optional prompt input source buffer)
  (setq gr-current-window (selected-window))
  (setq gr-buffer (or buffer gr-buffer))
  (gr-create-gr-buffer)
  (gr-window-config 'save)
  (gr-display-buffer gr-buffer)

  (setq gr-source source)

  (unwind-protect
	  (gr-read-pattern prompt input)
	(gr-cleanup))
  (setq gr-pattern "")
  (setq gr-current-window nil))

(defun gr-read-pattern (&optional prompt input)
  (with-gr-buffer
   (if input
	   (setq gr-pattern input)
	 (setq gr-pattern ""))
   (gr-update)
   (let* (timer)
	 (unwind-protect
		 (minibuffer-with-setup-hook
			 (lambda ()
			   (gr-minor-mode 1)
			   (setq timer
					 (run-with-idle-timer
					  0.01 'repeat
					  (lambda ()
						(save-selected-window (gr-check-minibuffer-input)))))) ;; end lambda
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

(cl-defun gr-update ()
  (with-gr-buffer
   (unwind-protect
	   (cond ((gr-source-sync-p gr-source)
			  (let* ((c (oref gr-source candidates)))
				(if (gr-string-empty-p gr-pattern) ;; show all candidates
					(progn
					  (setq gr-candidates-len (length c)
							gr-candidates-index 1)
					  (gr-render c))
				  (let* ((matched (gr-list-match-pattern c gr-pattern)))
					(setq gr-candidates-len (length matched)
						  gr-candidates-index 1)
					(gr-render matched))))
			  )
			 ((gr-source-async-p gr-source)
			  (let* ((proc-fn (oref gr-source candidates-process))
					 (check-fn (oref gr-source check-before-compute)))
				(when check-fn
				  (condition-case err
					  (funcall check-fn gr-pattern)
					(error
					 ;; (gr-log "check failed: %s" (error-message-string err))
					 ;; todo update mode line to show tips
					 (cl-return-from gr-update))))
				(let* ((proc (funcall proc-fn)))
				  (setq gr--proc (cons proc nil))))))
	 (setq gr-in-update nil))))

(defun gr-modeline-index (index total)
  ;; 约定格式以尽量防止modeline组件左右移动
  (cond ((gr-string-empty-p gr-pattern) "ALL  ")
		((eq total 0) "-/-  ")
		((> total 99) (format "%d/99+" index))
		(t (format "%d/%-2d " index total))))

(defun gr-render (candidates)
  (let ((inhibit-read-only t))
	(erase-buffer)
	(gr-render-matches candidates)
	(gr-update-modeline (gr-modeline-index gr-candidates-index gr-candidates-len))
	(gr-move-to-first-line)))

(defun gr-update-modeline (index)
  "INDEX: current/total"
  (setq mode-line-format
		(list index
			  " " mode-line-buffer-identification)))

(defun gr-rg-show-output (output)
  "为减少一次for循环，在遍历处理输出的同时刷新buffer"
  (with-gr-buffer
   (let* ((inhibit-read-only t)
		  (render-fn (oref gr-source render-line))
		  (max-index (1- (length output)))
		  (n 0))
	 (erase-buffer)
	 (cl-loop for i from 0
			  for batch in output
			  do (progn
				   (setq n (+ (gr-rg-insert-batch render-fn batch) n))
				   (when (< i max-index)
					 (insert "\n"))))
	 (setq gr-candidates-len n
		   gr-candidates-index 0)
	 (gr-update-modeline (gr-modeline-index gr-candidates-index gr-candidates-len))
	 (gr-move-to-first-line)
	 (gr-forward-and-mark-line 1))))

(defun gr-rg-insert-batch (render batch)
  "return number of candidates in BATCH"
  (let* ((lines (split-string batch "\n"))
		 ;; ignore file line
		 (n (1- (length lines)))
		 (valid-linum n))
	(cl-loop for i from 0
			 for line in lines
			 do (let* ((ln (funcall render line)))
				  (if ln
					  (progn
						(insert ln)
						(when (< i n)
						  (insert "\n")))
					(setq valid-linum (1- valid-linum)))))
	valid-linum))

(defun gr-rg-line-file-p (ln)
  (string-prefix-p "/" ln))

(defun gr-render-matches (matches)
  (let* ((last-index (1- (length matches))))
	(cl-loop for i from 0
			 for m in matches
			 do (progn
				  (insert m)
				  (when (< i last-index)
					(insert "\n"))))))

(defun gr-move-to-first-line ()
  "goto first line of `gr-buffer'"
  (goto-char (point-min))
  (unless (equal (point-min) (point-max))
	(gr-mark-current-line)))

(defun gr-mark-current-line ()
  (with-gr-buffer
   (move-overlay gr-selection-overlay (point-at-bol) (1+ (point-at-eol)))))

;; utils

(defun gr-workspace ()
  "if current directory is a git project, return project path,
otherwise `default-directory'"
  (let* ((path (gr-proc-output "/usr/local/bin/git" "rev-parse" "--show-toplevel")))
	(if (eq (car path) 0)
		(string-trim (cdr path))
	  (expand-file-name default-directory))))

(defun gr-proc-output (exe &rest args)
  "(exit-code . output)"
  (let (exit-code)
	(with-temp-buffer
	  (let ((proc (make-process :name "*gr-temp-proc*"
								:buffer (current-buffer)
								:command `(,exe ,@args)
								:noquery t
								:sentinel #'ignore)))
		(set-process-query-on-exit-flag proc nil)
		(while (accept-process-output proc nil nil t))
		(setq exit-code (process-exit-status proc)))
	  (cons exit-code (buffer-string)))))

(defun gr-log (fmtstr &rest args)
  (with-current-buffer (get-buffer-create "*gr-debug-log*")
	(outline-mode)
	(buffer-disable-undo)
	(let ((inhibit-read-only t))
	  (goto-char (point-max))
	  (insert (apply #'format (cons fmtstr args)) "\n")
	  (goto-char (point-max)))))

(defun gr-string-empty-p (s)
  (if (or (null gr-pattern)
		  (zerop (length gr-pattern)))
	  t
	nil))

(defun gr-string-match-p (str pattern)
  "return t if STR matches PATTERN, otherwise return nil"
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

(defun gr-list-match-pattern (strlist pattern)
  "return a list in which each item is a string matches PATTERN"
  (let ((matched (cl-loop for s in strlist
						  when (gr-string-match-p s pattern)
						  collect s)))
	matched))

(defun gr-regexp-valid-p (expr)
  "return nil if expr is valid, otherwise return the error"
  (condition-case err
      (progn
        (string-match-p expr "")
        nil)
	(invalid-regexp
	 err)))

(provide 'gr-core)
