(require 'url)
(defun arif/download-url (url download-dir download-name)
  "Download URL and save in DOWNLOAD-DIR with DOWNLOAD-NAME."
  (interactive "sEnter url to download: 
sEnter destination directory: 
sEnter destination file name: ")
  (let ((file-name (concat download-dir download-name)))
    (url-copy-file url file-name 1)))

(defun arif/download-paper (url download-name)
  "Download paper from given URL and save using DOWNLOAD-NAME."
  (interactive "sEnter url to download: 
sEnter destination file name: ")
  (let* ((dir-name (completing-read "Choose directory: " (directory-files "~/Dropbox/Literature/")))
	(file-name (concat "~/Dropbox/Literature/" dir-name "/" (replace-regexp-in-string " " "_" download-name))))
    (url-copy-file url file-name 1)))

(defun arif/download-arxiv-no (arxiv-no download-name)
  "Download arxiv paper for given ARXIV-NO and save using DOWNLOAD-NAME."
  (interactive "sEnter arxiv no to download: 
sEnter destination file name: ")
  (let* ((url (concat "https://arxiv.org/pdf/" arxiv-no ".pdf"))
	 (dir-name (completing-read "Choose directory: " (directory-files "~/Dropbox/Literature/")))
	(file-name (concat "~/Dropbox/Literature/" dir-name "/" (replace-regexp-in-string " " "_" download-name ".pdf"))))
    (url-copy-file url file-name 1)))


