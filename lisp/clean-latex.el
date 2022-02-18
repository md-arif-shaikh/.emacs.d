;; clean latex auxiliary files for a given directory
(defun clean-latex (directory)
  "Clean all the auxiliary files generated due to LaTeX compilation
in a given DIRECTORY."
  (interactive (list (read-file-name "Enter directory: ")))
  (let ((files '())
	(num-of-files 0)
	(cleanp))
    (dolist (extension '("out" "aux" "log" "gz" "bbl"))
      (push (file-expand-wildcards (format "%s*.%s" directory extension)) files))
    (setq files (flatten-list files))
    (setq num-of-files (length files))
    (setq cleanp (string-equal (completing-read (format "Clean %s files: " num-of-files) '("yes" "no")) "yes"))
    (when cleanp
      (dolist (file files)
	(delete-file file))
      (message (format "cleaned %s files." num-of-files)))))
