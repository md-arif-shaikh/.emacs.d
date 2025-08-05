;; -*- lexical-binding: t; -*-

;;; arxiv-search.el --- lisp functions to search arxiv papers
;;; Commentary:
;;; Code:

(require 'org)
(require 'dom)

(defun arif/arxiv ()
  "Search arxiv."
  (interactive)
  (let* ((search-method (completing-read  "How do you want to search: " '("dates" "author")))
	 (url)
	 (author)
	 (date-from)
	 (date-to)
	 (weeks)
	 (search-string)
	 (search-field)
	 (search-category)
	 (last-weeks-dates (cl-loop for n in (number-sequence 0 8)
				 collect (format-time-string "%Y-%m-%d" (time-subtract (current-time) (days-to-time (* n 7)))))))
    (cond ((equal search-method "author")
	   (setq author (read-string "Enter author name: "))
	   (setq url (format "https://arxiv.org/search/?query=%s&searchtype=author&source=header" (replace-regexp-in-string " " "+" author))))
	  ((equal search-method "dates")
	   (setq date-from (completing-read "Enter date from when to search for [YYYY-MM-DD]: " last-weeks-dates))
	   (setq date-to (completing-read "Enter date upto when to search for [YYYY-MM-DD]: " last-weeks-dates))
	   (setq search-string (completing-read "Enter string to search for: " '("gravitational waves" "neutron star")))
	   (setq search-field (completing-read "Enter search filed: " '("abstract" "Title")))
	   (setq search-category (completing-read "Enter search category: " '("astro-ph" "gr-qc")))
	   (setq url (concat "https://arxiv.org/search/advanced?advanced=&terms-0-operator=AND&terms-0-term="
			     (replace-regexp-in-string " " "+" search-string)
			     "&terms-0-field="
			     search-field
			     "&classification-physics=y&classification-physics_archives="
			     search-category
			     "&classification-include_cross_list=include&date-year=&date-filter_by=date_range&date-from_date="
			     date-from
			     "&date-to_date="
			     date-to
			     "&date-date_type=submitted_date_first&abstracts=show&size="
			     "50"
			     "&order=-announced_date_first"))))
    (with-current-buffer (url-retrieve-synchronously url)
      (let* ((dom (libxml-parse-html-region (point-min) (point-max)))
	     (titles (dom-by-class dom "title is-5 mathjax"))
	     (authors (dom-by-class dom "authors"))
	     (abstract (dom-by-class dom "abstract-full has-text-grey-dark mathjax"))
	     (arxiv-ids (dom-by-class dom "list-title is-inline-block"))
	     (number-of-titles (length titles))
	     (org-buffer (generate-new-buffer "arxiv.org"))
	     (title-string (cond ((equal search-method "author") (concat "by " author))
				 ((equal search-method "dates") (concat "from " date-from " " date-to))))
	     (file-name (format "/tmp/papers_%s.org" (replace-regexp-in-string " " "_" title-string))))
	(set-buffer org-buffer)
	(insert (format "#+TITLE: List of papers %s\n" title-string))
	(insert "#+AUTHOR: Md Arif Shaikh\n")
	(insert (format "#+DATE: %s\n" (current-time-string)))
	(insert (format "Found %d papers\n" number-of-titles))
	(dolist (n (number-sequence 0 (1- number-of-titles)))
	  ;; insert title
	  (insert (concat "* " (string-trim (dom-text (nth n titles))) "\n"))
	  ;; insert authors
	  (let* ((name-list (split-string (dom-texts (nth n authors)) ","))
		 (number-of-authors (length name-list))
		 (first-author)
		 (name-string))
	    (setq name-list (mapcar (lambda (name) (string-trim name)) name-list))
	    (setq first-author (string-trim (substring (nth 0 name-list) (length "Authors:") nil)))
	    (setq name-list (push first-author (cdr name-list)))
	    (setq name-string (if (> number-of-authors 1)
				  (concat (string-join (seq-subseq name-list 0 -1) ", ") " and " (seq-elt name-list (1- number-of-authors)))
				first-author))
	    (insert (concat "  - Authors: " name-string "\n")))
	  ;; insert abstract
	  (insert (concat "  - Abstract: " (string-trim (dom-text (nth n abstract))) "\n"))
	  ;; insert arxiv url
	  (let* ((arxiv-id (string-trim (nth 0 (split-string (dom-texts (nth n arxiv-ids)) "\n"))))
		 (arxiv-number (substring arxiv-id (length "arXiv:") nil))
		 (arxiv-url (concat "https://arxiv.org/abs/" arxiv-number)))
	    (insert (concat "  - URL: " arxiv-url "\n")))
	  )
	(write-file file-name)
	(find-file file-name)
	(beginning-of-buffer)
	(org-overview)))))

(global-set-key (kbd "C-c s a") 'arif/arxiv)

(provide 'arxiv-search)
;;; arxiv-search ends here
