;; org-agenda
(use-package org
  :config
  (global-set-key (kbd "C-c a") 'org-agenda)
  (global-set-key (kbd "C-c c") 'org-capture)
  (setq org-agenda-files '("/Users/arif/Dropbox/org/alarms.org"
			   "/Users/arif/Dropbox/org/anniversaries.org"
			   "/Users/arif/Dropbox/org/calls.org"
			   "/Users/arif/Dropbox/org/inbox.org")))
  
;; ===============================
;; BASIC AGENDA SETTINGS
;; ===============================
(setq org-agenda-span 14)
(setq org-agenda-start-day "+0d")
(setq org-agenda-start-on-weekday nil)
(setq org-agenda-include-deadlines t)
(setq org-deadline-warning-days 14)   ; Show warnings 2 weeks ahead
(setq org-agenda-skip-scheduled-if-done t)  ; Hide done scheduled items
(setq org-agenda-skip-deadline-if-done t)   ; Hide done deadline items
(setq org-scheduled-past-days 7)      ; Show 1 week of past scheduled items
(setq org-deadline-past-days 7)       ; Show 1 week of past deadlines

;; Time grid settings
(setq org-agenda-time-leading-zero t)
(setq org-agenda-timegrid-use-ampm nil)  ; Use 24-hour format (change to t for AM/PM)
(setq org-agenda-use-time-grid t)
(setq org-agenda-show-current-time-in-grid t)
(setq org-agenda-time-grid
      '((daily today require-timed)
        (800 1000 1200 1400 1600 1800 2000)
        "......" "----------------"))

;; Visual improvements
(setq org-agenda-block-separator "")
(setq org-agenda-start-with-log-mode nil)  ; Don't show log by default (press 'l' to toggle)
(setq org-agenda-tags-column -100)    ; Right-align tags
(setq org-agenda-dim-blocked-tasks t) ; Dim tasks blocked by dependencies

;; ===============================
;; TODO KEYWORDS & STATES
;; ===============================
(setq org-todo-keywords 
      '((sequence "TODO(t)" "NEXT(n)" "WAITING(w@/!)" "|" "DONE(d!)" "CANCELLED(c@)")
        (sequence "MEET(m)" "|" "MET(M)" "POSTPONED(P)")
        (sequence "ATTEND(a)" "|" "ATTENDED(A)" "UNABLE TO ATTEND(U)")
        (sequence "READ(r)" "READING(R)" "|" "FINISHED(F)")
        (sequence "DISCUSS(s)" "|" "DISCUSSED(D)")
        (sequence "PRESENT(p)" "|" "PRESENTED(P)" "CANCELLED(C)")
        (sequence "WORKSHOP(o)" "|" "DONE(d)")
        (sequence "CONFERENCE(c)" "|" "DONE(d)")
        (sequence "SEMINAR(e)" "|" "DONE(d)")
        (sequence "VISIT(v)" "|" "DONE(d)")
        (sequence "FOOTBALL(f)" "|" "FINISHED(F)")
        (sequence "PROJECT(j)" "|" "COMPLETED(C)" "CANCELLED(k@)")))

(setq org-todo-keyword-faces 
      '(("TODO" . (:background "#61afef" :foreground "#282c34" :weight ultra-bold :box (:line-width 2 :color "#61afef")))
        ("NEXT" . (:background "#98c379" :foreground "#282c34" :weight ultra-bold :box (:line-width 2 :color "#98c379")))
        ("WAITING" . (:background "#e5c07b" :foreground "#282c34" :weight ultra-bold :box (:line-width 2 :color "#e5c07b")))
        ("ATTEND" . (:background "#be5046" :foreground "#282c34" :weight ultra-bold :box (:line-width 2 :color "#be5046")))
        ("MEET" . (:background "#207FA1" :foreground "#282c34" :weight ultra-bold :box (:line-width 2 :color "#207FA1")))
        ("READ" . (:background "#d19a66" :foreground "#282c34" :weight ultra-bold :box (:line-width 2 :color "#d19a66")))
        ("READING" . (:background "#e5c07b" :foreground "#282c34" :weight ultra-bold :box (:line-width 2 :color "#e5c07b")))
        ("DISCUSS" . (:background "#d19a66" :foreground "#282c34" :weight ultra-bold :box (:line-width 2 :color "#d19a66")))
        ("PRESENT" . (:background "#c678dd" :foreground "#282c34" :weight ultra-bold :box (:line-width 2 :color "#c678dd")))
        ("WORKSHOP" . (:background "#56b6c2" :foreground "#282c34" :weight ultra-bold :box (:line-width 2 :color "#56b6c2")))
        ("CONFERENCE" . (:background "#c678dd" :foreground "#282c34" :weight ultra-bold :box (:line-width 2 :color "#c678dd")))
        ("SEMINAR" . (:background "#56b6c2" :foreground "#282c34" :weight ultra-bold :box (:line-width 2 :color "#56b6c2")))
        ("VISIT" . (:background "#e06c75" :foreground "#282c34" :weight ultra-bold :box (:line-width 2 :color "#e06c75")))
        ("FOOTBALL" . (:background "#98c379" :foreground "#282c34" :weight ultra-bold :box (:line-width 2 :color "#98c379")))
        ("PROJECT" . (:background "#c678dd" :foreground "#282c34" :weight ultra-bold :box (:line-width 2 :color "#c678dd")))
        ("CANCELLED" . (:background "#5c6370" :foreground "#abb2bf" :weight bold :box (:line-width 1 :color "#5c6370") :strike-through t))
        ("DONE" . (:background "#98c379" :foreground "#282c34" :weight bold :box (:line-width 1 :color "#98c379") :strike-through t))
        ("ATTENDED" . (:background "#5c6370" :foreground "#abb2bf" :weight bold :box (:line-width 1 :color "#5c6370") :strike-through t))
        ("MET" . (:background "#5c6370" :foreground "#abb2bf" :weight bold :box (:line-width 1 :color "#5c6370") :strike-through t))
        ("POSTPONED" . (:background "#5c6370" :foreground "#abb2bf" :weight bold :box (:line-width 1 :color "#5c6370") :strike-through t))
        ("PRESENTED" . (:background "#5c6370" :foreground "#abb2bf" :weight bold :box (:line-width 1 :color "#5c6370") :strike-through t))
        ("COMPLETED" . (:background "#98c379" :foreground "#282c34" :weight bold :box (:line-width 1 :color "#98c379") :strike-through t))
        ("FINISHED" . (:background "#5c6370" :foreground "#abb2bf" :weight bold :box (:line-width 1 :color "#5c6370") :strike-through t))
        ("UNABLE TO ATTEND" . (:background "#5c6370" :foreground "#abb2bf" :weight bold :box (:line-width 1 :color "#5c6370") :strike-through t))))

;; ===============================
;; TAGS
;; ===============================
(setq org-tag-alist
      '((:startgroup . nil)
        ("@work" . ?w) ("@home" . ?h) ("@computer" . ?c) ("@phone" . ?p) ("@errand" . ?e)
        (:endgroup . nil)
        ("research" . ?r)
        ("teaching" . ?t)
        ("admin" . ?a)
        ("writing" . ?W)
        ("coding" . ?C)
        ("reading" . ?R)
        ("planning" . ?P)))

;; ===============================
;; CAPTURE TEMPLATES
;; ===============================
(setq org-capture-templates
      '(("t" "Quick TODO" entry
         (file+headline "~/Dropbox/org/inbox.org" "Inbox")
         "* TODO %?\n  %i\n  %a"
         :prepend t)
        
        ("n" "Next Action" entry
         (file+headline "~/Dropbox/org/inbox.org" "Inbox")
         "* NEXT %?\n  SCHEDULED: %t\n  %i"
         :prepend t)
        
        ("s" "Scheduled Tasks")
        ("st" "Schedule TODO" entry
         (file+headline "~/Dropbox/org/inbox.org" "Scheduled")
         "* TODO %?\n  SCHEDULED: %^t\n"
         :prepend t)
        ("sm" "Schedule Meeting" entry
         (file+headline "~/Dropbox/org/inbox.org" "Meetings")
         "* MEET %? :meeting:\n  SCHEDULED: %^T\n  :PROPERTIES:\n  :LOCATION: %^{Location}\n  :END:\n"
         :prepend t)
        ("sa" "Schedule Attendance" entry
         (file+headline "~/Dropbox/org/inbox.org" "Events")
         "* ATTEND %?\n  SCHEDULED: %^T\n"
         :prepend t)
        ("sw" "Schedule Workshop" entry
         (file+headline "~/Dropbox/org/inbox.org" "Events")
         "* WORKSHOP %?\n  SCHEDULED: %^t\n"
         :prepend t)
        
        ("d" "Deadlines")
        ("dt" "Set Deadline for TODO" entry
         (file+headline "~/Dropbox/org/inbox.org" "Inbox")
         "* TODO %?\n  DEADLINE: %^t\n"
         :prepend t)))


(setq org-agenda-deadline-leaders
      '("" "" "%2d d. ago: ")
      org-deadline-warning-days 0
      org-agenda-span 7
      org-agenda-start-day "-0d"
      org-agenda-skip-function-global 
      '(org-agenda-skip-entry-if 'todo 'done)
      org-log-done 'time)
