;;; gr-search-buffer.el --- search in current workspace

;;; Code:

(require 'gr-source)
(require 'gr-core)

(defconst gr-rg--gr-buffer-name "*gr-rg*")
(defconst gr-rg--proc-name "*gr-rg--rg-proc*")
(defconst gr-rg--proc-buffer-name "*gr-rg--rg-output*")
(defconst gr-rg--source-name "gr-rg")
(defconst gr-rg--exe-file "/usr/local/bin/rg")

(defvar gr-rg--min-char-num 3
  "rg will not be invoked unless the input is at least this many chars")
(defvar gr-rg--proc nil
  "current rg process, uesd to kill before rising a new one")

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

  (let* ((dir "/Users/blue/envzo/zefram")
		 (input gr-pattern)
		 (proc (make-process :name gr-rg--proc-name
							 :buffer gr-rg--proc-buffer-name
							 :command `(,gr-rg--exe-file "-S" "-i" "--color" "never" ,input ,dir)
							 :sentinel #'gr-core-process-sentinel
							 :noquery t)))
	(set-process-query-on-exit-flag proc nil)
	(setq gr-rg--proc proc)
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

(defun gr-rg--render-line (line)
  (cond ((string-prefix-p "/" line) ;; for simplicity and speed, use this expr to detech file
		 (gr-rg--make-face 'gr-rg-file-face line))
		((> (length line) 0)
		 line)
		(t
		 nil)))

(defun gr-rg--cleanup ()
  (when (and gr-rg--proc
  			 (process-live-p gr-rg--proc))
  	(kill-process gr-rg--proc)
  	(setq gr-rg--proc nil)))

(defconst gr-rg-proc-source
  (gr-make-source gr-rg--source-name 'gr-source-async
	:candidates-process #'gr-rg-make-proc
	:check-before-compute #'gr-rg--check-before-compute
	:render-line #'gr-rg--render-line
	:cleanup #'gr-rg--cleanup))

;;; gr-search-workspace.el ends here

;; test

(gr-core nil "doesstrmatches\\(" gr-rg-proc-source gr-rg--gr-buffer-name)
