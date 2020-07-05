;;; xhy-notetaking.el: settings for taking notes with Org-mode

;; org-mode customization
(setq org-agenda-files '("~/Documents/org/"))
(setq org-agenda-start-on-weekday nil) ; show agenda for next 7 days
(setq org-agenda-custom-commands
      '(("n" "Agenda and all TODOs"
	 ((alltodo "" nil))
	 nil)
	("b" "Backlog TODOs" todo "BACKLOG" nil)))
(setq org-agenda-span 'day) ; show daily agenda by default
(setq org-refile-targets '((org-agenda-files . (:maxlevel . 3))))
(add-to-list 'org-modules 'org-id)
(add-to-list 'org-modules 'org-habit)
(setq org-capture-templates
      '(("h" "Zettel" entry (function xhy/find-org-capture-target)
	 "* %^{Todo|NOTE|TODAY|BACKLOG|MARK} %? %^G\nSCHEDULED: %t\n\n" :time-prompt t)
	("t" "Derived Zettel" entry (function xhy/find-org-capture-target)
	 "* %^{Todo|NOTE|TODAY|BACKLOG|MARK} %? %^G\nSCHEDULED: %t\n\n  Related: %a" :time-prompt t)
	("n" "Duplicate to" entry (function xhy/find-org-capture-target)
	 "%(xhy/get-current-heading-content)" :time-prompt t)
	("m" "Microblogging" plain (function
				    (lambda () (find-file 
						(concat
						 "~/Documents/expoundite.net/toot/"
						 (org-id-uuid)
						 ".mdwn"))))
	 "%k\n\n[[!tag %?]]")))
(setq org-id-link-to-org-use-id t) ; use id as link target by default

(add-hook 'org-mode-hook 'auto-fill-mode)

(add-hook 'org-capture-before-finalize-hook 'org-id-get-create)
;;(add-hook 'org-capture-after-finalize-hook 'xhy/checkin-zettel)
(setq org-todo-keywords '((sequence "|" "NOTE")
			  (sequence "TODAY(t!)" "|" "BACKLOG(b!)" "DONE(d!)" "CANCELLED(c!)")
			  (sequence "|" "MARK(m!)" "ONGOING(o!)" "FINISHED(f)")))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (shell . t)
   (js . t)
   (ruby . t)))

;; keybindings
(my-leader-def
  "oa"  'org-agenda
  "oc"  'org-capture
  "ol"  'org-store-link
  "oL"  'org-insert-link
  "ot"  'org-insert-structure-template)

(my-leader-def
  "or" 'xhy/zettel-search-back-link)

(my-leader-def
  "ok" 'xhy/zettel-create-link-with-search
  "oo" 'counsel-org-goto
  "oO" 'counsel-org-goto-all)

;; remove spaces when exporting chinese texts
(defadvice org-html-paragraph (before org-html-paragraph-advice
				      (paragraph contents info) activate)
  "Join consecutive Chinese lines into a single long line without
unwanted space when exporting org-mode to html."
  (let* ((origin-contents (ad-get-arg 1))
	 (fix-regexp "[[:multibyte:]]")
	 (fixed-contents
	  (replace-regexp-in-string
	   (concat
	    "\\(" fix-regexp "\\) *\n *\\(" fix-regexp "\\)") "\\1\\2" origin-contents)))

    (ad-set-arg 1 fixed-contents)))

;; org-ref
(use-package org-ref :ensure t)

(setq org-ref-default-bibliography '("~/Documents/org/references.bib"))
(setq reftex-default-bibliography '("~/Documents/org/references.bib"))

;; zotxt
(use-package zotxt :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-zotxt-mode 1))))
(eval-after-load "zotxt"
'(setq zotxt-default-bibliography-style "chicago-author-date"))
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
(defun xhy/zettel-search-back-link ()
  "Search for zettels that link to this zettel, and show them in agenda view."
  (interactive)
  (let ((this-id (org-id-get)))
    (if this-id
	(org-search-view nil (concat "[id:" this-id))
      (message "This zettel has no ID yet."))))

(defun xhy/get-headings ()
  "Get headings and ids in each org entry with TODO keywords.
  Store them in an alist like this:
  '((heading1 . id1)
    (heading2 . id2)
    ...)
  and return the alist."
  ()
  ;; make file list based on org-agenda-files
  (let ((f-list ())
	(headings ())
	(ids ())
	(headings-alist))
    (dolist (d-or-f-name org-agenda-files)
      (if (directory-name-p d-or-f-name)
	  (setq f-list (append (directory-files d-or-f-name t "^[0-9]\\{4\\}-[0-9]\\{2\\}\\.org$") f-list))
	(push d-or-f-name f-list)))
    ;; one file at a time, get every heading and their ID
    (org-map-entries
     (lambda () (push (org-get-heading) headings)
       (push (org-id-get-create) ids)
       (while (and headings ids) ; don't have zip, so a hack
	 (push (cons (pop headings) (pop ids)) headings-alist)))
     "TODO={.+}"
     f-list)
    headings-alist))

(defun xhy/insert-link-from-ivy-selection (head-id-cons)
  "Insert link to EID at current region, or if no region, insert
  a link to EID with the description part as HEADING." 
  (interactive)
  (if (use-region-p)
      (org-insert-link nil (concat "id:" (cdr head-id-cons)))
    (org-insert-link nil (concat "id:" (cdr head-id-cons)) (car head-id-cons))))

(defun xhy/zettel-create-link-with-search (&optional beg end)
  "With a selected region, read from user input for the search
  term. Text in the region will be used as default. Use it to
  narrow down the candidate list. Once the user select an
  entry, insert link at the region."
  (interactive "r")
  (let ((initial-input (if (use-region-p)
			   (buffer-substring beg end)
			 (thing-at-point 'symbol t))))
    (ivy-read "Search string: "     ; PROMPT
	      (xhy/get-headings)    ; COLLECTION
	      :predicate nil
	      :require-match t
	      :initial-input initial-input
	      :action 'xhy/insert-link-from-ivy-selection)))


(provide 'xhy-notetaking)
