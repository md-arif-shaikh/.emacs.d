(require 'url)
(defun arif/download-url (url download-dir download-name)
  "Download URL and save in DOWNLOAD-DIR with DOWNLOAD-NAME."
  (interactive "sEnter url to download: 
sEnter destination directory: 
sEnter destination file name: ")
  (let ((file-name (concat download-dir download-name)))
    (url-copy-file url file-name 1)))

(defun arif/download-paper (url paper-title first-author-surname year)
  "Download paper from given URL and save using DOWNLOAD-NAME."
  (interactive "sEnter url to download: 
sEnter destination file name: 
sEnter first author surname: 
sEnter year of publication: ")
  (let* ((dir-name (completing-read "Choose directory: " (directory-files "~/Dropbox/Literature/")))
	(file-name (concat "~/Dropbox/Literature/" dir-name "/" first-author-surname "_" year "_" (replace-regexp-in-string " " "_" (downcase paper-title)) ".pdf")))
    (url-copy-file url file-name 1)
    (if (equal (read-string "Open file? y/n: ") "y")
	(find-file file-name)
      (message "saved %s" file-name))))

(defun arxiv-get-bibliographic-code (arxiv-number) ;; https://github.com/jkitchin/org-ref/blob/master/org-ref-arxiv.el
  "Get Bibliographic code for ARXIV-NUMBER."
  (with-current-buffer
      (url-retrieve-synchronously
       (concat
        "https://ui.adsabs.harvard.edu/abs/arXiv:"
        arxiv-number))
    (search-forward-regexp "<link rel=\"canonical\" href=\"http://ui.adsabs.harvard.edu/abs/\\(.*\\)/abstract\"/>")
    (match-string 1)))

(defun arxiv-get-bibtex-entry (arxiv-bibliographic-code) ;; https://github.com/jkitchin/org-ref/blob/master/org-ref-arxiv.el
  "Get bibtex entry for ARXIV-BIBLIOGRAPHIC-CODE."
  (with-current-buffer
      (url-retrieve-synchronously (format "https://ui.adsabs.harvard.edu/abs/%s/exportcitation" arxiv-bibliographic-code))
    (when (re-search-forward
	   "<textarea.*>\\(.*\\(?:\n.*\\)*?\\(?:\n\\s-*\n\\|\\'\\)\\)</textarea>"
	   nil t)
      (xml-substitute-special (match-string 1)))))

(defun arif/download-arxiv-no (arxiv-no)
  "Download arxiv paper for given ARXIV-NO.  File name is retrieved from bibtex entry."
  (interactive "sEnter arxiv no to download: ")
  (let* ((url (concat "https://arxiv.org/pdf/" arxiv-no ".pdf"))
	 (dir-name (completing-read "Choose directory: " (directory-files "~/Dropbox/Literature/")))
	 (bibtex-entry (arxiv-get-bibtex-entry (arxiv-get-bibliographic-code arxiv-no)))
	 (first-author-surname (substring (nth 0 (split-string (nth 1 (split-string bibtex-entry "=")) ",")) 3 -1))
	 (title (substring (nth 0 (split-string (nth 2 (split-string bibtex-entry "=")) ",")) 3 -2))
	 (year (substring (nth 0 (split-string (nth 5 (split-string bibtex-entry "=")) ",")) 1))
	 (file-name (concat "~/Dropbox/Literature/" dir-name "/" first-author-surname "_" year "_" arxiv-no "_" (replace-regexp-in-string " " "_" (downcase title)) ".pdf")))
    (url-copy-file url file-name 1)
    (if (equal (read-string "Open file? y/n: ") "y")
	(find-file file-name)
      (message "saved %s" file-name))))

(global-set-key (kbd "C-c C-d a") #'arif/download-arxiv-no)
(global-set-key (kbd "C-c C-d p") #'arif/download-paper)
(global-set-key (kbd "C-c C-d u") #'arif/download-url)
