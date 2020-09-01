;; search in current workspace
;; a gr interface to ripgrep

(require 'gr-proc)

(defconst gr-rg-mi-search-chars 2)
(defconst gr-rg-proc-name "*gr-rg-proc*")
(defconst gr-rg-proc-buffer-name "*gr-rg-output*")

(defun gr-rg-cleanup ()
  ;; todo delete overlays
  (gr-rg-kill-rg-proc))

(defun gr-rg-kill-rg-proc ()
  (let ((proc (get-process gr-rg-proc-name)))
	(when (process-live-p proc)
	  (delete-process proc))))

;; test rg process
(unwind-protect
	(gr-proc-output "/usr/local/bin/rg" "ignore" "/Users/blue/proj/golden-rye")
  (gr-rg-cleanup))
