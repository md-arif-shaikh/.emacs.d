;; Translate digits to Bengali
(defun digit-to-bn (digit)
  (let* ((digits '(("0" . "০")
		   ("1" . "১")
		   ("2" . "২")
		   ("3" . "৩")
		   ("4" . "৪")
		   ("5" . "৫")
		   ("6" . "৬")
		   ("7" . "৭")
		   ("8" . "৮")
		   ("9" . "৯"))))
    (cdr (assoc (number-to-string digit) digits))))

;; Translate dayname to Bengali
(defun day-name-to-bn (day-name)
  (let* ((day-names '(("Mon" . "সোম")
		     ("Tue" . "মঙ্গল")
		     ("Wed" . "বুধ")
		     ("Thu" . "বৃহস্পতি")
		     ("Fri" . "শুক্র")
		     ("Sat" . "শনি")
		     ("Sun" . "রবি"))))
    (cdr (assoc day-name day-names))))

;; translate monthname to Bengali
(defun month-name-to-bn (month-name)
  (let* ((month-names '(("Jan" . "জানুয়ারী")
			("Feb" . "ফেব্রুয়ারী")
			("Mar" . "মার্চ")
			("Apr" . "এপ্রিল")
			("May" . "মে")
			("Jun" . "জুন")
			("Jul" . "জুলাই")
			("Aug" . "আগস্ট")
			("Sep" . "সেপ্টেম্বর")
			("Oct" . "অক্টোবর")
			("Nov" . "নভেম্বর")
			("Dec" . "ডিসেম্বর"))))
    (cdr (assoc month-name month-names))))

;; Get the digits in a number as a string list of corresponding digits
(defun numberstring-to-digits (numberstring)
  (if (= (length numberstring) 0)
      '()
    (cons (substring numberstring 0 1) (numberstring-to-digits (substring numberstring 1 (length numberstring))))))

;; Translate number to Bengali
(defun number-to-bn (number)
  (let* ((numberstring (number-to-string number))
	 (digits (numberstring-to-digits numberstring)))
    (string-join (loop for digit in digits
		       collect (digit-to-bn (string-to-number digit))))))

;; display the scheduled time in bengali
(defun make-org-time-bn (time-string)
  (let* ((hr (string-to-number (substring time-string 0 2)))
	 (mn (string-to-number (substring time-string -3 nil)))
	 (bn-mn "০০")
	 (part-of-day "সকাল"))
    (cond ((and (> hr 12) (< hr 17))
	   (setq part-of-day "বিকেল")
	   (setq hr (- hr 12)))
	  ((and (>= hr 17) (< hr 21))
	   (setq part-of-day "সন্ধ্যে ")
	   (setq hr (- hr 12)))
	  ((>= hr 21)
	   (setq part-of-day "রাত্রি ")
	   (setq hr (- hr 12)))
	  ((= hr 12) (setq part-of-day "দুপুর")))
    (unless (= mn 0)
      (setq bn-mn (number-to-bn mn))
      )
    (format "%s %s%s%s " part-of-day (number-to-bn hr) ":" bn-mn)))

(defun arif/org-agenda-format-date-aligned (date)
  "Format a DATE string for display in the daily/weekly agenda.
   This function makes sure that dates are aligned for easy reading.
   Slightly tweaked version of `org-agenda-format-date-aligned' that
   produces dates with a fixed length."
  (require 'cal-iso)
  (let* ((dayname (calendar-day-name date t))
	 (day (cadr date))
	 (day-of-week (calendar-day-of-week date))
	 (month (car date))
	 (monthname (calendar-month-name month t))
	 (year (nth 2 date))
	 (iso-week (org-days-to-iso-week
		    (calendar-absolute-from-gregorian date)))
	 (weekyear (cond ((and (= month 1) (>= iso-week 52))
			  (1- year))
			 ((and (= month 12) (<= iso-week 1))
			  (1+ year))
			 (t year)))
	 (weekstring (if (= day-of-week 1)
			 (format " (সপ্তাহ %02s)" (day-num-to-bn iso-week))
		       "")))
    (format "%-6s %s %s %s %s"
	    (day-name-to-bn dayname) (day-num-to-bn day) (month-name-to-bn monthname) (year-num-to-bn year) weekstring)))

(setq org-agenda-format-date #'arif/org-agenda-format-date-aligned)

(setq org-agenda-scheduled-leaders
      '("পূর্ব নির্ধারিত:  " "%2dx নির্ধারিত: "))
(setq org-agenda-deadline-leaders
      '("শেষ তারিখ:  " "%2d দিনের মধ্যে: "  "%2d দিন আগে: "))
(setq org-agenda-current-time-string
      " ------------------------------- এখন সময় --------------------------------------")

(defun arif/org-agenda-prefix-format ()
  (let ((scheduled (org-get-scheduled-time (point)))
	(deadline (org-get-deadline-time (point))))
    (if (or scheduled deadline)
	(if scheduled
	    (make-org-time-bn (format-time-string "%H%M%" scheduled))
	  (if deadline
	      (make-org-time-bn (format-time-string "%H%M%" deadline))
	    ""))
      (make-org-time-bn (format-time-string "%H%M"))
      )))

(setq org-agenda-prefix-format   "%(arif/org-agenda-prefix-format)%2s")
(setq org-agenda-overriding-header "আজকের কর্মসূচী:\n-------------")

(setq org-todo-keywords
      '((sequence "করুন(k)" "|" "করা হয়ে গেছে(K)" "বাতিল করা হয়েছে(B)")
	(sequence "যোগ দিন(j)" "|" "যোগ দেওয়া হয়েছে(J)" "যোগ দিতে পারিনি(U)")
	(sequence "মিটিং(m)" "|" "মিটিং হয়ে গেছে(M)" "পিছোন হয়েছে(P)")
	(sequence "পড়ুন(p)" "|" "পড়া হয়ে গেছে(S)")
	(sequence "আলোচনা(a)" "|" "আলোচনা করা হয়েছে(A)")))

(setq org-todo-keyword-faces
      '(
	("করুন" . (:foreground "orange" :underline t :box nil  :weight extrabold))
	("যোগ দিন" . ( :foreground "orange" :underline t :box nil  :weight extrabold))
	("মিটিং" . ( :foreground "orange" :underline t :box nil  :weight extrabold))
	("পড়ুন" . ( :foreground "orange" :underline t :box nil  :weight extrabold))
	("আলোচনা" . ( :foreground "orange" :underline t :box nil  :weight extrabold))
	("বাতিল করা হয়েছে" . ( :foreground "gray50" :underline t :box nil))
	("করা হয়ে গেছে" . ( :foreground "gray50" :underline t :box nil))
	("যোগ দেওয়া হয়েছে" . ( :foreground "gray50" :underline t :box nil))
	("মিটিং হয়ে গেছে" . ( :foreground "gray50" :underline t :box nil))
	("পিছোন হয়েছে" . ( :foreground "gray50" :underline t :box nil))
	("পড়া হয়ে গেছে" . ( :foreground "gray50" :underline t :box nil))
	("আলোচনা করা হয়েছে" . ( :foreground "gray50" :underline t))))
