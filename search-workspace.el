;; search in current workspace
;; a gr interface to ripgrep

;; (defun gr-rg-pattern-to-regexp ()
;;   )

;; (defun gr-rg-construct-argv ()
;;   )

;; (defun gr-rg-make-process-from-argv ()
;;   )

;; (defun gr-rg-make-process ()
;;   (let* ((rg-regexp (gr-rg-pattern-to-regexp "buffer"))
;; 		 (argv (gr-rg-construct-argv rg-regexp))
;; 		 (proc (gr-rg-make-process-from-argv argv)))
;; 	proc))

;; test rg process
(let* ((rg-proc (gr-rg-make-process)))
  ;; do with rg-proc
  )

(defun gr-rg-process-output (exe &rest args)
  (with-temp-buffer
	(let ((proc (make-process :name "gr-rg-temp-proc"
							  :buffer (current-buffer)
							  :command `(,exe ,@args)
							  :sentinel #'ignore)))
	  (while (accept-process-output proc nil nil t)))
	(buffer-string)))

(gr-rg-process-output "rg" "Rye" "/Users/blue/proj/golden-rye")
