(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	   'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(defun arif/load-file (file-name)
  (if (file-exists-p file-name)
      (load-file file-name)
    (message (format "%s file not found" file-name))))

(arif/load-file "~/.config/emacs/remote-machines.el")
(defun arif/connect-remote-dir (remote-machine-name)
  "Connect to REMOTE-MACHINE-NAME."
  (interactive "sRemote Machine Name: ")
  (let ((remote-user-name (cdr (assoc remote-machine-name remote-user-names))))
    (set-buffer (dired (format "/sshx:%s:/home/%s/" remote-machine-name remote-user-name)))
    (add-to-list 'tramp-remote-path 'tramp-own-remote-path)))

(defun arif/connect-remote-shell (remote-machine-name)
  "Connect to terminal in on REMOTE-MACHINE-NAME."
  (interactive "sRemote Machine Name: ")
  (let* ((remote-shell-types '(("comet" . "/bin/bash")
			       ("dodo" . "/bin/zsh")
			       ("cit" . "/bin/bash")))
	 (default-directory (format "/sshx:%s:" remote-machine-name))
	 (tramp-encoding-shell (cdr (assoc remote-machine-name remote-shell-types))))
    (shell)))

(global-set-key (kbd "C-c r d") #'arif/connect-remote-dir)
(global-set-key (kbd "C-c r s") #'arif/connect-remote-shell)

(use-package doom-themes
  :straight t
  :if (display-graphic-p)
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
	doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (setq doom-themes-treemacs-enable-variable-pitch nil)
  (doom-themes-treemacs-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package gruvbox-theme
    :straight t
    :if (not (display-graphic-p))
    :init
    (load-theme 'gruvbox-dark-soft t))

(use-package all-the-icons
  :straight t
  :ensure t
  :if (and (display-graphic-p) (eq system-type (or 'gnu/linux 'darwin)))
  )
(use-package all-the-icons-dired
  :straight t
  :ensure t
  :if (and (display-graphic-p) (eq system-type (or 'gnu/linux 'darwin)))
  :hook
  (dired-mode . (lambda ()
		  (interactive)
		  (unless (file-remote-p default-directory)
		    (all-the-icons-dired-mode)))))

(if (or (eq system-type 'windows-nt) (eq system-type 'cygwin))
    (progn
      (set-language-environment "UTF-8")
      (set-default-coding-systems 'utf-8)))

(use-package doom-modeline
  :straight t
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  ;; Configure doom-modeline variables
  (setq find-file-visit-truename t)
  (setq doom-modeline-bar-width 3)
  (setq doom-modeline-height 10)
  (setq doom-modeline-buffer-file-name-style 'file-name)
  (setq doom-modeline-lsp t)
  (setq doom-modeline-minor-modes nil)
  (setq doom-modeline-github t)
  (setq doom-modeline-github-interval (* 30 60))
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-icon (and (display-graphic-p) (eq system-type (or 'gnu/linux 'darwin))))
  (setq doom-modeline-env-version t)
  (if (and (display-graphic-p) (eq system-type (or 'gnu/linux 'darwin)))
      (setq doom-modeline-minor-modes nil)
    (setq doom-modeline-minor-modes t))
  ;;(setq doom-modeline-mu4e t)
  (setq doom-modeline-buffer-encoding nil)
  )

(add-to-list 'default-frame-alist '(fullscreen . fullscreen))

(setq inhibit-startup-message t)

(if (display-graphic-p)
    (progn
      (tool-bar-mode -1)
      (scroll-bar-mode -1)
      (menu-bar-mode -1)
      (set-fringe-mode 0)))
(if (daemonp)
    (setq default-frame-alist (menu-bar-mode -1)))

(use-package rainbow-delimiters
  :straight t
  :after (prog-mode)
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(show-paren-mode 1)
(setq show-paren-style 'parenthesis)

(global-hl-line-mode 1)

(setq linum-format "%4d \u2502 ")
;;(setq display-line-numbers 'relative)

(use-package autopair
  :straight t)
(autopair-global-mode)

(set-face-attribute 'default nil :family "Fira Code" :height 120)

;;(set-face-font 'default "fontset-default")
(set-fontset-font "fontset-default" 'bengali (font-spec :family "Kalpurush" :size 18))

(set-language-environment "Bengali")
(add-hook 'set-language-environment-hook
	  (lambda nil
	    (when (equal current-language-environment "Bengali")
	      (setq default-input-method "bengali-itrans"))))

(arif/load-file "~/.emacs.d/lisp/time-zone.el")

(use-package pyvenv
  :straight t
  :defer
  :config
  (setenv "WORKON_HOME" "~/miniconda3/envs/")
  (pyvenv-mode 1)
  (pyvenv-tracking-mode 1))

(use-package tex
  :ensure auctex
  :mode ("\\.tex\\'" . latex-mode)
  :after (tex)
  :config
  (setq TeX-source-correlate-mode t
	TeX-source-correlate-method 'synctex
	TeX-source-correlate-start-server t
	TeX-auto-save t
	TeX-parse-self t
	reftex-plug-into-AUCTeX t
	TeX-view-program-list
	'(("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")
	  ("Evince" "evince --page-index=%(outpage) %o"))
	)
  (if (string-equal system-type "darwin")
      (setq TeX-view-program-selection '((output-pdf "Skim")))
    (setq TeX-view-program-selection '((output-pdf "Evince"))))
  (add-hook 'TeX-after-compilation-finished-functions
	    #'TeX-revert-document-buffer)
  (add-hook 'LaTeX-mode-hook
	    (lambda ()
	      (reftex-mode t)
	      (flyspell-mode t)))
  :hook
  (LaTeX-mode . linum-mode)
  (LaTeX-mode . rainbow-delimiters-mode))

(use-package pdf-tools
  :straight t
  :if (display-graphic-p)
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-view-use-scaling t) ;; To increase the sharpness in retina display
  (setq pdf-annot-activate-created-annotations t)
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  (define-key pdf-view-mode-map (kbd "C-r") 'isearch-backward)
  (setq pdf-sync-minor-mode t)
  ;;(define-key pdf-view-mode-map (kbd "C-c C-g") 'pdf-sync-forward-search)
  ;;(add-hook 'pdf-view-mode-hook (lambda ()
  ;;				  (bms/pdf-midnite-amber))) ; automatically turns on midnight-mode for pdfs
  )

(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :straight t
  :commands lsp
  :hook
  (lsp-mode . efs/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :config
  (setq lsp-enable-completion-at-point t
	lsp-prefer-capf t) 
  )

(use-package lsp-ui
  :straight t)

(setq gc-cons-threshold 100000000
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-idle-delay 0.0
      company-minimum-prefix-length 1
      lsp-idle-delay 0.1)  ;; clangd is fast

(use-package lsp-python-ms 
  :straight t
  :init
  (setq
   lsp-python-ms-auto-install-server t
   lsp-python-ms-executable (executable-find "python-language-server"))
  :hook
  (python-mode . (lambda ()
			 (require 'lsp-python-ms)
			 (lsp-deferred)))
  (python-mode . linum-mode))

(use-package lsp-pyright
  :straight t
  :hook (python-mode . (lambda ()
			  (require 'lsp-pyright)
			  (lsp))))  ; or lsp-deferred

(lsp-register-client
 (make-lsp-client :new-connection (lsp-tramp-connection "pyls")
		  :major-modes '(python-mode)
		  :remote? t
		  :server-id 'pyls-remote))

(use-package highlight-indent-guides
  :straight t
  :defer t
  :config
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  (setq highlight-indent-guides-method 'character))

(use-package flycheck
  :straight t
  :config
  (global-flycheck-mode)
  (setq flycheck-indication-mode 'left-fringe)
  (setq-default flycheck-disabled-checkers '(python-pylint))
  )

(use-package company-jedi
  :straight t
  :defer)

(use-package racket-mode
  :straight t
  :defer t
  :config
  (setq racket-program "/usr/racket/bin/racket")
  :hook
  (racket-mode . linum-mode))

(use-package julia-mode
  :straight t
  :defer
  :hook (julia-mode . linum-mode))

(use-package jupyter
  :straight t
  :defer)

(use-package company
  :straight t
  :defer
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-dabbrev-downcase 0)
  (setq company-idle-delay 0.1)
  (setq company-minimum-prefix-length 1)
  (setq company-tooltip-align-annotations t)
  )

(defun arif/shell-mode-setup () 
  (when (and (fboundp 'company-mode)
	     (file-remote-p default-directory))
    (company-mode -1)))
(add-hook 'shell-mode-hook #'arif/shell-mode-setup)

(use-package counsel
  :straight t
  :config
  (ivy-mode)
  (setq ivy-use-virtual-buffers t
	ivy-count-format "(%d/%d) ")
  ;;(setq ivy-extra-directories nil)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "C-x b") 'ivy-switch-buffer))

(use-package which-key
  :straight t
  :config
  ;; Allow C-h to trigger which-key before it is done automatically
  (setq which-key-show-early-on-C-h t)
  ;; make sure which-key doesn't show normally but refreshes quickly after it is
  ;; triggered.
  (setq which-key-idle-delay 1)
  ;;(setq which-key-idle-secondary-delay 0.01)
  (which-key-setup-side-window-bottom)
  (which-key-mode))

(use-package dired-x
  :after (dired)
  :config
  (setq dired-omit-verbose nil
        dired-omit-files
        (concat dired-omit-files "\\|^\\..+$"))
  (add-hook 'dired-mode-hook #'dired-omit-mode)
  (add-hook 'dired-mode-hook #'dired-hide-details-mode))

(defun mydired-sort ()
  "Sort dired listings with directories first."
  (save-excursion
    (let (buffer-read-only)
      (forward-line 2) ;; beyond dir. header 
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
    (set-buffer-modified-p nil)))

(defadvice dired-readin
    (after dired-after-updating-hook first () activate)
  "Sort dired listings with directories first before adding marks."
  (mydired-sort))

(use-package magit
  :straight t
  :defer
  :bind ("C-x g" . magit-status))

(use-package yasnippet
  :straight t
  :defer
  :init
  (add-hook 'after-init-hook 'yas-global-mode)
  :config
  (global-set-key (kbd "C-c y y") 'yas-expand))

(defun my-org-latex-yas ()
;;  "Activate org and LaTeX yas expansion in org-mode buffers."
  (yas-minor-mode)
  (yas-activate-extra-mode 'latex-mode))

(add-hook 'org-mode-hook #'my-org-latex-yas)

(defun replace-in-string (what with in)
  (replace-regexp-in-string (regexp-quote what) with in nil 'literal))

(defun arif/latex-greek-symbols (english-symbol)
  (interactive)
  (defvar arif/greek-symbols)
  (setq arif/greek-symbols '(("a" . "\\alpha")
			     ("b" . "\\beta")
			     ("c" . "\\chi")
			     ("d" . "\\delta")
			     ("D" . "\\Delta")
			     ("e" . "\\epsilon")
			     ("f" . "\\phi")
			     ("F" . "\\Phi")
			     ("g" . "\\gamma")
			     ("G" . "\\Gamma")
			     ("i" . "\\iota")
			     ("k" . "\\kappa")
			     ("l" . "\\lambda")
			     ("L" . "\\Lambda")
			     ("m" . "\\mu")
			     ("n" . "\\nu")
			     ("o" . "\\omega")
			     ("O" . "\\Omega")
			     ("p" . "\\pi")
			     ("P" . "\\Pi")
			     ("r" . "\\rho")
			     ("s" . "\\sigma")
			     ("t" . "\\tau")
			     ("x" . "\\xi")
			     ("ve" . "\\varepsilon")
			     ("vp" . "\\varphi"))
	)
  (cdr (assoc english-symbol arif/greek-symbols))
  )

(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

(fset 'yes-or-no-p 'y-or-n-p)

(arif/load-file "~/.config/emacs/custom-commands.el")

(use-package org
  :config
  (global-set-key (kbd "C-c a") 'org-agenda)
  (setq org-agenda-files '("~/Dropbox/org"))
  ;; Basic setup
  (setq org-agenda-span 7)
  (setq org-agenda-start-day "+0d")
  (setq org-agenda-start-on-weekday nil)
  ;; Items with deadline and scheduled timestamps
  ;;(setq org-agenda-include-deadlines t)
  ;;(setq org-deadline-warning-days 5)
  (setq org-agenda-skip-scheduled-if-done t)
  ;;(setq org-agenda-skip-scheduled-if-deadline-is-shown t)
  ;;(setq org-agenda-skip-timestamp-if-deadline-is-shown t)
  (setq org-agenda-skip-deadline-if-done t)
  ;;(setq org-agenda-skip-deadline-prewarning-if-scheduled 1)
  ;;(setq org-agenda-skip-scheduled-delay-if-deadline nil)
  ;;(setq org-agenda-skip-additional-timestamps-same-entry nil)
  ;;(setq org-agenda-search-headline-for-time t)
  (setq org-scheduled-past-days 30)
  (setq org-deadline-past-days 30)
  ;;(setq org-agenda-move-date-from-past-immediately-to-today t)
  ;;(setq org-agenda-show-future-repeats t)
  ;;(setq org-agenda-prefer-last-repeat nil)
  ;;(setq org-agenda-time-leading-zero t)
  ;;(setq org-agenda-timegrid-use-ampm nil)
  (setq org-agenda-use-time-grid t)
  (setq org-agenda-show-current-time-in-grid t)
  (setq org-agenda-time-grid
	'((daily today remove-match)
	  ()
	  "......" "----------------"))
  (setq display-time-format "%H:%M")
;;  (setq org-agenda-todo-keyword-format "%-1s")
  (arif/load-file "~/.emacs.d/lisp/bn-org.el")
)

(require 'appt)
(setq appt-time-msg-list nil)    ;; clear existing appt list
(setq appt-display-interval '10) ;; warn every 10 minutes from t - appt-message-warning-time
(setq
 appt-message-warning-time '30  ;; send first warning 10 minutes before appointment
 appt-display-mode-line t     ;; don't show in the modeline
 appt-display-format 'window)   ;; pass warnings to the designated window function
(appt-activate 1)                ;; activate appointment notification
(display-time)                   ;; activate time display

(org-agenda-to-appt)             ;; generate the appt list from org agenda files on emacs launch
(run-at-time "24:01" 3600 'org-agenda-to-appt)           ;; update appt list hourly
(add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt) ;; update appt list on agenda view

;; set up the call to terminal-notifier
(defvar my-notifier-path 
  "/usr/bin/notify-send")  ;; path to libnotify binary notify-send
(defun my-appt-send-notification (msg)
  (shell-command (concat my-notifier-path " -t" " 0 " " -i" " ~/.emacs.d/icons/emacs.png "  msg))) ;; see notify-send help to understand the options

;; designate the window function for my-appt-send-notification
(defun my-appt-display (min-to-app new-time msg)
  (my-appt-send-notification 
   (format "'Appointment in %s minutes\n %s'" min-to-app msg)))
(setq appt-disp-window-function (function my-appt-display))

(defun appt-mode-line-bangla (min-to-app &optional abbrev)
  "Return an appointment string suitable for use in the mode-line.
MIN-TO-APP is a list of minutes, as strings.
If ABBREV is non-nil, abbreviates some text."
  ;; All this silliness is just to make the formatting slightly nicer.
  (let* ((multiple (> (length min-to-app) 1))
	 (imin (if (or (not multiple)
		       (not (delete (car min-to-app) min-to-app)))
		   (car min-to-app))))
    (format "%s%s %s"
	    (if abbrev "এপয়েন্টমেন্ট" "এপয়েন্টমেন্ট")
	    (if multiple "স" "")
	    (if (equal imin "0") "এখন"
	      (format "%s %s"
		      (or (number-to-bn (string-to-number imin)) (mapconcat #'identity (mapcar #'number-to-bn (mapcar #'string-to-number min-to-app)) ","))
		      (if abbrev "মিনিটে"
			(format "মিনিটে" (if (equal imin "1") "" ""))))))))

(advice-add 'appt-mode-line :override #'appt-mode-line-bangla)

(setq org-hide-emphasis-markers t)

(font-lock-add-keywords 'org-mode
			'(("^ *\\([-]\\) "
			   (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(use-package org-bullets
  :straight t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(add-hook 'org-mode-hook 'visual-line-mode)

(setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))

(straight-use-package 'toc-org)
(if (require 'toc-org nil t)
    (add-hook 'org-mode-hook 'toc-org-mode)
  (warn "toc-org not found"))

(use-package org-tree-slide
  :straight t
  :custom
  (org-image-actual-width nil))

(use-package perfect-margin
  :straight t)

(arif/load-file "~/.emacs.d/lisp/bgt.el")

(setq auth-sources
      '((:source "~/.config/emacs/.authinfo.gpg")))

(use-package ob-ipython
  :straight t
  :defer t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (ipython . t)
   (jupyter . t)))

;;;;;;;;;;;;;;;;;;;;
;;; set up unicode
(prefer-coding-system       'utf-8)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8)                      
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

(arif/load-file "~/.emacs.d/lisp/download-url.el")

(arif/load-file "~/.emacs.d/lisp/arxiv-search.el")

(defadvice term (after advise-term-coding-system)
  (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
(ad-activate 'term)

(use-package markdown-toc
  :straight t)

(use-package scribble-mode
  :straight t
  :defer t
  :hook (scribble-mode . linum-mode))

(defun add-preceding-zero (number-string)
  (if (= (length number-string) 1)
      (string-join (list "০" number-string))
    number-string))

(setq display-time-string-forms
      ;;'((calendar-julian-date-string)))
      '((add-preceding-zero (number-to-bn (string-to-number day))) "/" (month-name-to-bn monthname) "/" (add-preceding-zero (number-to-bn (string-to-number (substring year -2))))
	    " " (number-to-bn (string-to-number 24-hours)) ":" (add-preceding-zero (number-to-bn (string-to-number minutes)))
	    (if time-zone " (") time-zone (if time-zone ")")
	    (if mail " Mail" "")
	    )
      )

(display-time-mode 1)

(display-battery-mode 1)
(defun doom-modeline-update-battery-status-bangla ()
  "Update battery status."
  (setq doom-modeline--battery-status
	(when (bound-and-true-p display-battery-mode)
	  (let* ((data (and battery-status-function
			    (functionp battery-status-function)
			    (funcall battery-status-function)))
		 (charging? (string-equal "AC" (cdr (assoc ?L data))))
		 (percentage (car (read-from-string (or (cdr (assq ?p data)) "ERR"))))
		 (valid-percentage? (and (numberp percentage)
					 (>= percentage 0)
					 (<= percentage battery-mode-line-limit)))
		 (face (if valid-percentage?
			   (cond (charging? 'doom-modeline-battery-charging)
				 ((< percentage battery-load-critical) 'doom-modeline-battery-critical)
				 ((< percentage 25) 'doom-modeline-battery-warning)
				 ((< percentage 95) 'doom-modeline-battery-normal)
				 (t 'doom-modeline-battery-full))
			 'doom-modeline-battery-error))
		 (icon (if valid-percentage?
			   (cond (charging?
				  (doom-modeline-icon 'alltheicon "battery-charging" "🔋" "+"
						      :face face :height 1.4 :v-adjust -0.1))
				 ((> percentage 95)
				  (doom-modeline-icon 'faicon "battery-full" "🔋" "-"
						      :face face :v-adjust -0.0575))
				 ((> percentage 70)
				  (doom-modeline-icon 'faicon "battery-three-quarters" "🔋" "-"
						      :face face :v-adjust -0.0575))
				 ((> percentage 40)
				  (doom-modeline-icon 'faicon "battery-half" "🔋" "-"
						      :face face :v-adjust -0.0575))
				 ((> percentage battery-load-critical)
				  (doom-modeline-icon 'faicon "battery-quarter" "🔋" "-"
						      :face face :v-adjust -0.0575))
				 (t (doom-modeline-icon 'faicon "battery-empty" "🔋" "!"
							:face face :v-adjust -0.0575)))
			 (doom-modeline-icon 'faicon "battery-empty" "⚠" "N/A"
					     :face face :v-adjust -0.0575)))
		 (text (if valid-percentage? (format "%s%%%%" (substring (number-to-bn percentage) 0 2)) ""))
		 (help-echo (if (and battery-echo-area-format data valid-percentage?)
				(battery-format battery-echo-area-format data)
			      "Battery status not available")))
	    (cons (propertize icon 'help-echo help-echo)
		  (propertize text 'face face 'help-echo help-echo))))))

(advice-add 'doom-modeline-update-battery-status :override #'doom-modeline-update-battery-status-bangla)

(arif/load-file "~/.emacs.d/lisp/emacs-bn.el")
