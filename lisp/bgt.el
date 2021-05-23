;;; bgt.el --- record blood glucose level and produce blood glucose table
;;; commentary:
;;; code:

(require 'org)

(defcustom bgt-file-name "/tmp/bgt.org"
  "Org file name to save the blood glucose data."
  :type 'string
  :group 'bgt
  )

(defcustom bgt-csv-file-name "/tmp/bgt.csv"
  "CSV file name for exporting the table data from org file."
  :type 'string
  :group 'bgt)

(defun bgt-add-entry ()
  "Add bg record."
  (interactive)
  (let ((file-name bgt-file-name)
	(date-time (org-read-date 'with-time nil nil "Record time:  "))
	(bg-level (read-string "BG level: "))
	(bg-category (completing-read "Record type: " '("Fasting" "Random" "Post-prandial")))
	(bg-test (completing-read "Test type: " '("Plasma" "Capillary")))
	(bg-lab (completing-read "Lab Name: " '("Glucometer"))))
    (with-temp-buffer
      (when (not (file-exists-p file-name))
	;;(set-buffer (generate-new-buffer file-name))
	(insert "#+TITLE: Blood Glucose Table\n\n")
	(insert "* BGT\n")
	(insert ":PROPERTIES:\n")
	(insert (format ":TABLE_EXPORT_FILE: %s\n" bgt-csv-file-name))
	(insert ":TABLE_EXPORT_FORMAT: orgtbl-to-csv\n")
	(insert ":END:\n\n")
	(insert "|--|--|--|--|--|\n")
	(insert "|Date |BG | Category | Test | Lab|\n")
	(insert "|--|--|--|--|--|\n")
	(append-to-file (point-min) (point-max) file-name)))
    (with-temp-buffer
      (insert (format "|%s |%s |%s |%s |%s |\n" date-time bg-level bg-category bg-test bg-lab))
      (insert "|--|--|--|--|--|\n")
      (append-to-file (point-min) (point-max) file-name))
    (with-temp-buffer
      (switch-to-buffer (find-file-noselect file-name))
      (goto-char (point-max))
      (forward-line -1)
      (org-table-align)
      (write-file file-name))))

(defun bgt-export-to-csv ()
  "Export bgt data to a csv file."
  (interactive)
  (let ((file-name bgt-file-name)
	(export-file-name bgt-csv-file-name))
    (with-temp-buffer
      (switch-to-buffer (find-file-noselect file-name))
      (goto-char (point-max))
      (forward-line -2)
      (org-table-export))))

(setq bgt-file-name "~/Dropbox/org/bgt.org")
(setq bgt-csv-file-name "~/Dropbox/org/bgt.csv")
(global-set-key (kbd "C-c b g") 'bgt-add-entry)

(provide 'bgt)
;;; bgt.el ends here
