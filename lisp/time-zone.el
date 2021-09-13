(defcustom arif/time-zones nil
  "Assoc list of month names."
  :type 'alist
  :group 'time-zones)

(defcustom arif/times nil
  "Times to convert."
  :type 'list
  :group 'time-zones)

(setq arif/time-zones '(("EST" . "-0500")
			("CST" . "-0600")
			("CDT" . "-0500")
			("EET" . "+0200")
			("CET" . "+0100")
			("IST" . "+0530")
			("KOLKATA" . "+0530")
			("BENGALURU" . "+0530")
			("DELHI" . "+0530")
			("MUMBAI" . "+0530")
			("CHENNAI" . "+0530")
			("JST" . "+0900")
			("TOKYO" . "+0900")))

(setq arif/times '("9:00 AM"
		   "9:30 AM"
		   "10:00 AM"
		   "10:30 AM"
		   "11:00 AM"
		   "11:30 AM"
		   "12:00 PM"
		   "1:00 PM"
		   "1:30 PM"
		   "2:00 PM"
		   "2:30 PM"
		   "3:00 PM"
		   "3:30 PM"
		   "4:00 PM"
		   "4:30 PM"
		   "5:00 PM"
		   "5:30 PM"
		   "6:00 PM"
		   "6:30 PM"
		   "7:00 PM"
		   "7:30 PM"
		   "8:00 PM"
		   "8:30 PM"
		   "9:00 PM"
		   "9:30 PM"
		   "10:00 PM"
		   "10:30 PM"
		   "11:00 PM"
		   "11:30 PM"))

(defun arif/convert-time ()
  "Convert from one time-zone to another time-zone."
  (interactive)
  (let* ((days '("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"))
	 (time-to-convert (completing-read "Enter time to convert: " arif/times))
	 (time (parse-time-string time-to-convert))
	 (from-zone-u (upcase (completing-read "Enter from zone: " arif/time-zones)))
	 (to-zone-u (upcase (completing-read "Enter to zone: " arif/time-zones)))
	 (from-sec (nth 0 time))
	 (from-min (nth 1 time))
	 (from-hour (nth 2 time))
	 (from-day (nth 6 time)))
    (when (string-match-p (regexp-quote "PM") (upcase time-to-convert))
      (setq  from-hour (+ 12 from-hour)))
    (let* ((time-shift (- (nth 5 (parse-time-string (cdr (assoc to-zone-u arif/time-zones))))
			  (nth 5 (parse-time-string (cdr (assoc from-zone-u arif/time-zones))))))
	   (hour-shift (/ time-shift 100))
	   (min-shift (% time-shift 100))
	   (to-min (+ from-min min-shift))
	   (to-hour (+ from-hour hour-shift))
	   (to-day-name "")
	   A-or-P)
      (when (< to-min 0)
	(setq to-min (+ to-min 60))
	(setq to-hour (1- to-hour)))
      (cond ((>= to-hour 24) (progn
			       (setq to-hour (- to-hour 24))
			       (if (not (equal from-day nil))
				   (setq to-day-name (nth (1+ from-day) days))
				 (setq to-day-name "+ 1 day"))))
	    ((< to-hour 0) (progn
			     (setq to-hour (+ 24 to-hour))
			     (if (not (equal from-day nil))
				 (setq to-day-name (nth (1- from-day) days))
			       (setq to-day-name "- 1 day"))))
	    ((and (> to-hour 0) (< to-hour 24)) (if (not (equal from-day nil))
						    (setq to-day-name (nth from-day days))
						  (setq to-day-name ""))))
      (cond ((= to-hour 0) (setq A-or-P "Midnight"))
	    ((< to-hour 12) (setq A-or-P "AM"))
	    ((= to-hour 12) (setq A-or-P "Noon"))
	    ((> to-hour 12) (progn
			      (setq to-hour (- to-hour 12))
			      (setq A-or-P "PM"))))
      (message (format "%s %s = %.2d:%.2d %s %s %s" (upcase time-to-convert) from-zone-u to-hour to-min (upcase A-or-P) (upcase to-day-name) to-zone-u)))))
