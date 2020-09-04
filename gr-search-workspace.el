;; search in current workspace

(require 'gr-source)
(require 'gr-core)

(defconst gr-rg--gr-buffer-name "*gr-rg*")
(defconst gr-rg--proc-name "*gr-rg--rg-proc*")
(defconst gr-rg--proc-buffer-name "*gr-rg--rg-output*")
(defconst gr-rg--source-name "gr-rg")

(defun gr-rg-make-proc ()
  (let* ((proc (make-process :name gr-rg--proc-name
							 :buffer gr-rg--proc-buffer-name
							 :command '("echo" "\"hello\nworld\"")
							 :noquery t)))
	(set-process-query-on-exit-flag proc nil)
	proc))

(defconst gr-rg-proc-source
  (gr-make-source gr-rg--source-name 'gr-source-async
	:candidates-process #'gr-rg-make-proc))

;; test
(gr-core nil nil gr-rg-proc-source gr-rg--gr-buffer-name)
