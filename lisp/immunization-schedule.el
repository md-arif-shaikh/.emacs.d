;;; immunization-schedule --- schedular for immunization
;;; commentary:
;;; code:

(defcustom i-schedule-date-of-birth nil
  "Date of birth to start the immunization process."
  :type 'string
  :options "2021-05-20"
  :group 'immunization-schedule)

(defcustom i-schedule-baby-name "your baby"
  "Name of your baby."
  :type 'string
  :options "সাম্য আরোহণ"
  :group 'immunization-schedule)

(defvar vaccine-schedules-list)
(setq vaccine-schedules-list '(("BCG" . ("++0d"))
			       ("Hepatitis B" . ("++0d" "++6w" "++10w" "++14w"))
			       ("OPV" . ("++0d" "++6w" "++10w" "++14w"))
			       ("DPT" . ("++6w" "++10w" "++14w"))
			       ("Pentavalent" . ("++6w" "++10w" "++14w"))
			       ("Rotavirus" . ("++6w" "++10w" "++14w"))
			       ("IPV" . ("++6w" "++14w"))
			       ("Measles" . ("++9m" "++16m"))
			       ("JE-1" . ("++9m" "++16m"))
			       ("Vitamin A" . ("++9m"))
			       ("DPT Booster 1" . ("++16m"))
			       ("OPV Booster" . ("++16m"))
			       ("DPT Booster 2" . ("++5y"))
			       ("TT" . ("++10y" "++16y"))))

(defun i-schedule-vaccine (vaccine-name)
  "Schedule vaccination of VACCINE-NAME."
  (when (equal i-schedule-date-of-birth nil)
    (user-error "Date of birth can not be nil.  Set the date of birth using i-schedule-date-of-birth"))
  (let* ((vaccine-schedules (cdr (assoc vaccine-name vaccine-schedules-list)))
	 (vaccine-dates (mapcar (lambda (schedule) (org-read-date nil nil schedule nil (org-time-string-to-time i-schedule-date-of-birth))) vaccine-schedules)))
    (mapconcat (lambda (vaccine-date) (string-join (list "* " (format "করুন %s কে %s ভ্যাকসিন দিন\nSCHEDULED: <%s 11:00>" i-schedule-baby-name vaccine-name vaccine-date)))) vaccine-dates "\n")))

(defun i-schedule-add-agenda (vaccine-list file-name)
  "Add todo item in an org file FILE-NAME for vaccines in VACCINE-LIST."
  (with-temp-buffer
    (let* ((vaccines (mapcar 'car vaccine-list)))
      (set-buffer (generate-new-buffer file-name))
      (insert (format "#+TITLE: Immunization for %s\n" i-schedule-baby-name))
      (insert (string-join (cl-loop for vaccine in vaccines
				    collect (i-schedule-vaccine vaccine)) "\n"))
      (write-file file-name)
      (find-file file-name)
      (beginning-of-buffer)
      (org-overview)
      )))

(setq i-schedule-date-of-birth "2021-05-20")
(setq i-schedule-baby-name "সাম্য আরোহণ")
(i-schedule-add-agenda vaccine-schedules-list "~/Dropbox/org/immunization.org")

(provide 'immunization-schedule)
;;; immunization-schedule ends here
