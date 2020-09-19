;; gr means Golden Rye, in Chinese: 金色麦田

(require 'cl-lib)
(require 'subr-x)

(defconst gr-buffer "*gr*")

(defvar gr-current-window nil
  "current window where `gr' is invoked")
(defvar gr-last-window-config nil)
(defvar gr-buffer-height 15)
(defvar gr-pattern "")
(defvar gr-source nil
  "a list of candidates")
(defvar gr-in-update nil)

(defvar gr--proc nil
  "cons. car is proc, cdr is candidates")
(defvar gr--proc-timeout-thread nil)

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
	  (set-process-filter proc nil)
	  (delete-process proc)))
  (abort-recursive-edit))

(defun gr-keyboard-enter ()
  (interactive)
  (exit-minibuffer))

(defun gr-previous-line ()
  (interactive)
  (gr-forward-and-mark-line -1))

(defun gr-next-line ()
  (interactive)
  (gr-forward-and-mark-line 1))

(defun gr-forward-and-mark-line (linum)
  (with-gr-window
   (forward-line linum)
   ;; (gr-mark-current-line)
   ;; (unless (gr-edge-of-buffer-p linum)
   ;; 	 (forward-line linum)
   ;; 	 (gr-mark-current-line))
   ))

(defun gr-edge-of-buffer-p (n)
  "return non-nil if we are at EOB or BOB"
  (save-excursion
	(forward-line n)
	(eq (point-at-bol) (point-at-eol))))

(defun gr-previous-page ()
  (interactive)
  (with-gr-window
   (scroll-down)
   (gr-mark-current-line)))

(defun gr-next-page ()
  (interactive)
  (with-gr-window
   (scroll-up)
   (gr-mark-current-line)))

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
  (let* ((cleanup (assoc-default 'cleanup gr-source)))
	(when cleanup
	  (funcall cleanup)))
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
			 (if (> (window-body-height w) (- gr-buffer-height 1))
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
	   (let* ((fn (assoc-default 'candidates gr-source))
			  (procfn (assoc-default 'candidates-process gr-source))
			  (checker-fn (assoc-default 'check-before-compute gr-source)))
		 (when checker-fn
		   (condition-case err
			   (funcall checker-fn gr-pattern)
			 (error
			  (gr-log "check failed: %s" (error-message-string err))
			  ;; todo update mode line to show tips
			  (cl-return-from gr-update)
			  )))
		 (let* ((candidates (if procfn
								(funcall procfn)
							  fn)))
		   (cond ((processp candidates)
				  (setq gr--proc (cons candidates nil))
				  ;; candidates will be filtered in process filter
				  (set-process-filter candidates 'gr-output-filter)
				  (setq gr--proc-timeout-thread (make-thread (lambda ()
															   (sleep-for 5)
															   (when (and gr--proc
																		  (process-live-p (car gr--proc)))
																 (gr-log "process timeout, kill it")
																 (kill-process (car gr--proc)))))))
				 ((or (null gr-pattern) ;; show all candidates
					  (zerop (length gr-pattern)))
				  (gr-render candidates))
				 (t
				  (let* ((matches (gr-list-match-pattern candidates gr-pattern)))
					(gr-render matches))))))
	 (setq gr-in-update nil))))

(defun gr-render (candidates)
  (erase-buffer)
  (gr-render-matches candidates)
  (goto-char (point-min)))

(defun gr-output-filter (proc output)
  "the `process-filter' function for gr async source.
note: this may be called multi times when process returns serval times"
  (when (eq proc (car gr--proc))
	(gr-log "output filter")
	(setcdr gr--proc (append (cdr gr--proc) `(,(string-trim output))))))

(cl-defun gr-process-sentinel (proc status)
  "为防止process多次返回造成Emacs闪烁，以及为了提供稳定的用户体验，`gr'会一次性返回process的执行结果。process的性能由process负责，实际上，对于如rg之类的程序，在大多数项目中性能（用户等待搜索返回的时间）几乎没有影响"
  (unless (eq proc (car gr--proc)) (cl-return-form gr-process-sentinel))
  (gr-log (format "proc status: %s" status))
  (cond ((equal status "finished\n")
		 (gr-cancel-timeout)
		 (let* ((message (cdr gr--proc)))
		   (setq gr--proc nil)
		   (gr-rg-process-output message)))
		((string-prefix-p "exited abnormally" status)
		 (gr-cancel-timeout)
		 ;; extract error message, totally hard code
		 ;; todo update mode line
		 (let* ((last-message (car (last (cdr gr--proc)))))
		   (if last-message
			   (progn
				 (gr-log "process error: %s" last-message)
				 (let* ((lines (split-string last-message "\n")))
				   (when (eq (length lines) 4)
					 (gr-log (concat (car lines)
									 (string-trim-left (car (last lines)) "error:"))))))
			 (progn
			   (gr-log "not found")
			   (with-gr-buffer (erase-buffer))))))))

(defun gr-cancel-timeout ()
  (when (and gr--proc-timeout-thread
			 (thread-alive-p gr--proc-timeout-thread))
	(thread-signal gr--proc-timeout-thread 'quit nil)
	(setq gr--proc-timeout-thread nil)))

(defun gr-rg-process-output (output)
  "为减少一次for循环，在遍历处理输出的同时刷新buffer"
  (with-gr-buffer
   (let* ((render-fn (assoc-default 'render-line gr-source)))
	 (erase-buffer)
	 (cl-loop for batch in output
			  for lines = (split-string batch "\n")
			  do (cl-loop for line in lines
						  do (let* ((ln (funcall render-fn line)))
							   (when ln (insert ln "\n"))))))))

(defun gr-render-matches (matches)
  (cl-loop for m in matches
		   do (insert m "\n")))

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
