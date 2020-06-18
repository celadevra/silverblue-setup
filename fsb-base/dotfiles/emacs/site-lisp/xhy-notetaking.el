;;; xhy-notetaking.el: settings for taking notes with Org-mode

;; org-mode customization
(setq org-agenda-files '("~/Documents/org/"))
(setq org-agenda-start-on-weekday nil)
(setq org-agenda-custom-commands
      '(("n" "Agenda and all TODOs"
	 ((alltodo "" nil))
	 nil)
	("b" "Backlog TODOs" todo "BACKLOG" nil)))
(setq org-refile-targets '((org-agenda-files . (:maxlevel . 3))))
(add-to-list 'org-modules 'org-id)
(setq org-capture-templates
      '(("h" "Zettel" entry (function xhy/find-org-capture-target)
	 "* %^{Todo|NOTE|TODAY|BACKLOG|MARK} %? %^G\nSCHEDULED: %t\n\n" :time-prompt t)
	("t" "Derived Zettel" entry (function xhy/find-org-capture-target)
	 "* %^{Todo|NOTE|TODAY|BACKLOG|MARK} %? %^G\nSCHEDULED: %t\n\n  Related: %a" :time-prompt t)
	("n" "Duplicate to" entry (function xhy/find-org-capture-target)
	 "%(xhy/get-current-heading-content)" :time-prompt t)))

(add-hook 'org-capture-before-finalize-hook 'org-id-get-create)
;;(add-hook 'org-capture-after-finalize-hook 'xhy/checkin-zettel)
(setq org-todo-keywords '((sequence "|" "NOTE")
			  (sequence "TODAY(t!)" "|" "BACKLOG(b!)" "DONE(d!)" "CANCELLED(c!)")
			  (sequence "|" "MARK(m!)" "ONGOING(o!)" "FINISHED(f)")))
;; keybindings
(my-leader-def
  "oa"  'org-agenda
  "oc"  'org-capture
  "ol"  'org-store-link
  "oL"  'org-insert-link
  "ot"  'org-insert-structure-template)

;; Helper functions
(defun xhy/find-org-capture-target ()
  "Find org capture target by user's input.  The user gives a
date, or use the default date -- today.  The target is a file
named YYYYMM.org, and captured entry go to a week-based date
tree."
  ()
  (let* ((prompt-time (plist-get org-capture-plist :time-prompt))
	 (default-time (if prompt-time (org-capture-put :default-time (org-read-date nil t))
			 (org-capture-put :default-time (current-time))))
	 (month (format-time-string "%Y-%m"
				    (plist-get org-capture-plist :default-time)))
	 (week (concat "Week " (format-time-string "%U"
						  (plist-get org-capture-plist :default-time))))
	 (date (concat "[" (format-time-string "%Y-%m-%d %a"
					       (plist-get org-capture-plist :default-time)) "]"))
	 (target-file (concat "~/Documents/org/" month ".org"))
	 (file-exists (file-exists-p target-file)))
    (find-file target-file)
    (unless (ignore-errors
	      (org-find-exact-headline-in-buffer week))
      (goto-char (point-min))
      (org-insert-heading nil nil t)
      (insert week))
    (goto-char (org-find-exact-headline-in-buffer week))
    (unless (ignore-errors (org-find-exact-headline-in-buffer date))
      (goto-char (org-find-exact-headline-in-buffer week))
      (end-of-line)
      (org-insert-subheading t)
      (insert date))
    (goto-char (org-find-exact-headline-in-buffer date))))

(defun xhy/get-current-heading-content ()
  "Copy current heading's content then extract the string from kill ring."
  ()
  (org-back-to-heading)
  (org-copy-subtree)
  (substring-no-properties (car kill-ring)))

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
