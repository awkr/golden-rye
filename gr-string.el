(defun gr-string-match-p (str pattern)
  ;; if it matches return t, otherwise return nil
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

(defun gr-search-list (strlist pattern)
  (let ((matched (cl-loop for s in strlist
						  when (gr-string-match-p s pattern)
						  collect s)))
	matched))

(provide 'gr-string)
