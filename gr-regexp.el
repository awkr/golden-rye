(defun gr-regexp-valid-p (expr)
  "return nil if expr is valid, otherwise return the error"
  (condition-case err
      (progn
        (string-match-p expr "")
        nil)
	(invalid-regexp
	 err)))

(provide 'gr-regexp)
