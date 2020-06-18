;;; xhy-notetaking.el: settings for taking notes with Org-mode

;; Helper functions
(defun xhy/org-get-ids-with-tags (match)
  "Given MATCH, search for headings with those tags, return a list of their ids.

MATCH uses the same syntax as `org-agenda' TODO/TAGS matching,
but only tags search returns meaningful results."
  ()
  (let ((results ()))
    (mapcar (lambda (f)
	      (with-temp-buffer
		(insert-file-contents f)
		(org-mode)
		(setq results (append
			       (org-scan-tags 'org-id-get
					      (cdr
					       (org-make-tags-matcher
						match)) t 3)
			       results))))
	    (directory-files (car org-agenda-files)
			     t
			     "^[0-9]\\{4\\}-[0-9]\\{2\\}\\.org"))
    results))

;; test
;; (assert (eql 102 (length (xhy/org-get-ids-with-tags "emacs|ruby"))))

(provide 'xhy-notetaking)
