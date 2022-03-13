;;; lunch-paper.el --- Generate lunch paper email    -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Md Arif Shaikh

;; Author: Md Arif Shaikh <arifshaikh.astro@gmail.com>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; generates a letter that I need to send weekly with appropriately changed dates

;;; Code:

(require 'org)
(defun lunch-paper-email-body (day-name)
  "Generate lunch paper email for DAY-NAME."
  (interactive (list (completing-read "Schedule on: " '("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday"))))
  (let* ((date (if (string-equal (format-time-string "%A") day-name) (org-read-date nil nil "+0d") (org-read-date nil nil (concat "++" day-name))))
	 (body (format "We will have our weekly paper discussion at 12:00 noon online. A tentative list of papers from the last week could be found at http://gitlab.icts.res.in/astrorel/group-activities/wikis/this-weeks-papers#%s. Please feel free to add papers you find interesting.

Zoom link for the meeting: https://icts-res-in.zoom.us/j/3720582532?pwd=ZExCQkYvaDc4MXlOWUVtYUZmSnVpQT09

cheers
Arif" date)))
    (with-temp-buffer
      (insert body)
      (copy-region-as-kill (point-min) (point-max)))))

(defun lunch-paper-email-subject (day-name)
  "Generate lunch paper email for DAY-NAME."
  (interactive (list (completing-read "Schedule on: " '("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday"))))
  (let* ((date (if (string-equal (format-time-string "%A") day-name) (org-read-date nil nil "+0d") (org-read-date nil nil (concat "++" day-name))))
	 (month (format-time-string "%B" (org-time-string-to-seconds date)))
	 (day-of-month (format-time-string "%d" (org-time-string-to-seconds date)))
	 (year (format-time-string "%Y" (org-time-string-to-seconds date)))
	 (body (format "%s %s %s%s %s paper discussion at 12:00 noon" day-name month day-of-month (cond ((member (string-to-number day-of-month) '(1 21 31)) "st")
													((member (string-to-number day-of-month) '(2 22)) "nd")
													((member (string-to-number day-of-month) '(3 23)) "rd")
												        (t "th"))
		       year)))
    (with-temp-buffer
      (insert body)
      (copy-region-as-kill (point-min) (point-max)))))
(provide 'lunch-paper)
;;; lunch-paper.el ends here
