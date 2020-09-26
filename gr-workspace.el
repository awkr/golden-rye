(require 'gr-source)
(require 'gr-core)

(defconst gr-rg--proc-name "*gr-rg--rg-proc*")
(defconst gr-rg--proc-buffer-name "*gr-rg--rg-output*")
(defconst gr-rg--binary "/usr/local/bin/rg")

(defvar gr-rg--min-char-num 3
  "rg will not be invoked unless the input is at least this many chars")
(defvar gr-rg--dir "")
(defvar gr-rg--proc nil
  "current rg process, uesd to kill before rising a new one")
(defvar gr-rg-timeout-thread nil)
(defvar gr-rg-output ""
  "rg的输出")

;; customizations

(defgroup gr-rg nil
  "group for `gr-rg' customizations")

(defface gr-rg-file-face
  '((t (:underline t)))
  "face for the file when displaying matches in the `gr-buffer'"
  :group 'gr-rg)

(defun gr-rg--make-face (face str)
  (apply #'propertize (append (list str) `(face ,face))))

(defun gr-rg-make-proc ()
  ;; kill old proc to get the newest result as soon as possible
  (gr-rg--cleanup)

  (gr-log "creating process")
  (gr-log "pattern: %s" gr-pattern)
  (let* ((input gr-pattern)
		 (proc (make-process :name gr-rg--proc-name
							 :buffer gr-rg--proc-buffer-name
							 :command `(,gr-rg--binary "-S" "-i" "--color" "never" ,input ,gr-rg--dir)
							 :sentinel #'gr-rg-sentinel
							 :noquery t)))
	(gr-log "creating process ... done")
	(gr-log "cmd: %s" (mapconcat 'identity (process-command proc) " "))

	;; candidates will be filtered in process filter
	(set-process-filter proc 'gr-rg-output-filter)
	(set-process-query-on-exit-flag proc nil)
	(setq gr-rg--proc proc)
	(setq gr-rg-timeout-thread
		  (make-thread (lambda ()
						 ;; gr把响应时间放在第一位，通过压缩超时时间，倒逼gr优化自己
						 (sleep-for 5)
						 (when (and gr-rg--proc
									(process-live-p gr-rg--proc))
						   (gr-log "rg process timeout, kill it")
						   (kill-process gr-rg--proc)))))
	proc))

(defun gr-rg--check-before-compute (pattern)
  (cond ((< (length pattern) gr-rg--min-char-num)
		 (user-error "not enough chars: min %d" gr-rg--min-char-num))
	    (t
		 ;; because Emacs regexp is not compatiable with PRCE regexp which maybe
		 ;; the most widely known engine, familiar to most developers
		 ;; so `gr' gives up validating input, leave this to `rg' :)

		 ;; (let* ((err (gr-core-valid-regexp-p pattern)))
		 ;;   (when err (user-error (error-message-string err))))
		 )))

(defun gr-rg-output-filter (proc output)
  "`process-filter' for async source, may be called multi times if process returns serval times"
  (gr-log "output filter: %s" (process-name proc))
  ;; (gr-log "==> %s" output)
  (setq gr-rg-output (concat gr-rg-output output)))

(defun gr-rg-sentinel (proc status)
  "为防止process多次返回造成Emacs闪烁，以及为了提供稳定的用户体验，`gr'会一次性返回process的执行结果。
process的性能由process负责，实际上，对于如rg之类的程序，在大多数项目中性能（用户等待搜索返回的时间）几乎没有影响"
  (gr-log (format "<rg sentinel> STATUS: %s" status))
  (cond ((equal status "finished\n")
		 (gr-rg-cancel-timeout)
		 (gr-rg-on-finished gr-rg-output))
		((string-prefix-p "exited abnormally" status)
		 (gr-rg-cancel-timeout)
		 ;; extract error message, totally hard code
		 (if (> (length gr-rg-output) 0)
			 (progn
			   (gr-log "process error: %s" gr-rg-output)
			   (let* ((lines (split-string (string-trim gr-rg-output) "\n")))
				 (when (eq (length lines) 4)
				   (gr-log (concat (car lines)
								   (string-trim-left (car (last lines)) "error:"))))))
		   (gr-rg-on-not-found)))))

(defun gr-rg-cancel-timeout ()
  (when (and gr-rg-timeout-thread
			 (thread-alive-p gr-rg-timeout-thread))
	(thread-signal gr-rg-timeout-thread 'quit nil)
	(setq gr-rg-timeout-thread nil)))

(defun gr-rg-kill-rg ()
  (when (and gr-rg--proc
  			 (process-live-p gr-rg--proc))
  	(kill-process gr-rg--proc)
  	(setq gr-rg--proc nil)))

(defun gr-rg--render-line (line)
  (cond ((gr-rg-line-file-p line) ;; for simplicity and speed, use this expr to detech file
		 (gr-rg--make-face 'gr-rg-file-face line))
		((> (length line) 0)
		 line)
		(t ;; rg的返回结果中，两个文件之间存在空行
		 nil)))

(defun gr-rg--cleanup ()
  (gr-rg-kill-rg)
  (gr-rg-cancel-timeout)
  (setq gr-rg-output ""))

(defun gr-rg-make-source ()
  (gr-make-source 'gr-source-async :candidates-process #'gr-rg-make-proc
				  :check-before-compute #'gr-rg--check-before-compute
				  :render-line #'gr-rg--render-line
				  :cleanup #'gr-rg--cleanup))

;;;###autoload
(defun gr-workspace-search ()
  (interactive)
  (setq gr-rg--dir (gr-workspace))
  (gr-log "about to search in workspace: %s" gr-rg--dir)
  (gr-core nil nil (gr-rg-make-source) "*gr-w-search*"))

(provide 'gr-workspace)
