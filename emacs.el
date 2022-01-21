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

(setq tramp-histfile-override nil)
(arif/load-file "~/.config/emacs/remote-machines.el")
(defun arif/connect-remote-dir ()
  "Connect to REMOTE-MACHINE-NAME."
  (interactive)
  (let* ((remote-machine-name (completing-read "remote machine: " remote-machine-names))
	 (remote-user-name (cdr (assoc remote-machine-name remote-user-names))))
    (set-buffer (dired (format "/sshx:%s/" remote-user-name)))
    (add-to-list 'tramp-remote-path 'tramp-own-remote-path)))

(defun arif/connect-remote-shell ()
  "Connect to REMOTE-MACHINE-SHELL."
  (interactive)
  (let* ((remote-machine-name (completing-read "remote machine: " remote-machine-names))
	 (remote-user-name (cdr (assoc remote-machine-name remote-user-names)))
	 (remote-shell-type (cdr (assoc remote-machine-name remote-shell-names)))
	 (default-directory (format "/sshx:%s/" remote-user-name)))
    (setq explicit-shell-file-name remote-shell-type)
    (shell)))

(global-set-key (kbd "C-c r d") #'arif/connect-remote-dir)
(global-set-key (kbd "C-c r s") #'arif/connect-remote-shell)

(use-package doom-themes
  :straight t
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
(add-hook 'python-mode-hook 'linum-mode)
;;(setq display-line-numbers 'relative)

(use-package autopair
  :straight t)
(autopair-global-mode)

(set-face-attribute 'default nil
		    :font "JetBrains Mono"
		    :weight 'light
		    :height (cond ((string-equal system-type "gnu/linux") 120)
				  ((string-equal system-type "darwin") 130)))

;;(set-face-font 'default "fontset-default")
(set-fontset-font "fontset-default" 'bengali (font-spec :family "Kalpurush" :size (cond ((string-equal system-type "darwin") 13)
										  ((string-equal system-type "gnu/linux") 18))))
(setq default-input-method "bengali-itrans")

(setq-default cursor-type 'bar)

(use-package tzc
  :straight (tzc :type git :host github :repo "md-arif-shaikh/tzc"))

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
  (when (string-equal system-type "darwin")
    (setenv "PATH" (concat (getenv "PATH") ":/Library/TeX/texbin/"))
    (setq exec-path (append exec-path '("/Library/TeX/texbin/")))
    (setq pdf-info-epdfinfo-program "~/.emacs.d/straight/build/pdf-tools/build/server/epdfinfo"))
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

(use-package lsp-mode
  :straight t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((python-mode . lsp)
	 (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred))

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

(lsp-register-client
 (make-lsp-client :new-connection (lsp-tramp-connection "pyls")
		  :major-modes '(python-mode)
		  :remote? t
		  :server-id 'pyls-remote))
(require 'tramp)
(add-to-list 'tramp-remote-path '"~/miniconda3/bin/")

(use-package flycheck
  :straight t
  :config
  (global-flycheck-mode)
  (setq flycheck-indication-mode 'left-fringe)
  (setq-default flycheck-disabled-checkers '(python-pylint))
  )

(use-package racket-mode
  :straight t)

(use-package jupyter
  :straight t)

(use-package julia-mode
  :straight t
  :defer
  :hook (julia-mode . linum-mode))

(use-package rust-mode
  :straight t
  :config
  (add-hook 'rust-mode-hook
	    (lambda () (setq indent-tabs-mode nil)))
  (setq rust-format-on-save t)
  (define-key rust-mode-map (kbd "C-c C-c") 'rust-run))

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

(use-package exec-path-from-shell
  :straight t
  :custom
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "HISTFILE"))

(defun turn-on-comint-history (history-file)
  (setq comint-input-ring-file-name history-file)
  (comint-read-input-ring 'silent))

(add-hook 'shell-mode-hook
	  (lambda ()
	    (turn-on-comint-history (getenv "HISTFILE"))))

(add-hook 'kill-buffer-hook #'comint-write-input-ring)
(add-hook 'kill-emacs-hook
	  (lambda ()
	    (--each (buffer-list)
	      (with-current-buffer it (comint-write-input-ring)))))

(use-package savehist
  :custom
  (savehist-file "~/.emacs.d/savehist")
  (savehist-save-minibuffer-history t)
  (history-length 10000)
  (savehist-additional-variables
   '(shell-command-history))
  :config
  (savehist-mode +1))

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

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)))

(use-package org
  :config
  (global-set-key (kbd "C-c a") 'org-agenda)
  (setq org-agenda-files '("~/Dropbox/org" "~/Dropbox/org/roam"))
  ;; Basic setup
  (setq org-agenda-span 21)
  (setq org-agenda-start-day "+0d")
  (setq org-agenda-start-on-weekday nil)
  ;; Items with deadline and scheduled timestamps
  (setq org-agenda-include-deadlines t)
  (setq org-deadline-warning-days 28)
  (setq org-agenda-skip-scheduled-if-done t)
  ;;(setq org-agenda-skip-scheduled-if-deadline-is-shown t)
  ;;(setq org-agenda-skip-timestamp-if-deadline-is-shown t)
  (setq org-agenda-skip-deadline-if-done t)
  ;;(setq org-agenda-skip-deadline-prewarning-if-scheduled 1)
  ;;(setq org-agenda-skip-scheduled-delay-if-deadline nil)
  ;;(setq org-agenda-skip-additional-timestamps-same-entry nil)
  ;;(setq org-agenda-search-headline-for-time t)
  (setq org-scheduled-past-days 14)
  (setq org-deadline-past-days 14)
  ;;(setq org-agenda-move-date-from-past-immediately-to-today t)
  ;;(setq org-agenda-show-future-repeats t)
  ;;(setq org-agenda-prefer-last-repeat nil)
  (setq org-agenda-time-leading-zero t)
  (setq org-agenda-timegrid-use-ampm t)
  (setq org-agenda-use-time-grid t)
  (setq org-agenda-show-current-time-in-grid t)
  (setq org-todo-keywords '((sequence "TODO(t)" "|" "DONE(D)" "CANCELLED(C)")
			    (sequence "MEET(m)" "|" "MET(M)" "POSTPONED(P)")
			    (sequence "ATTEND(a)" "|" "ATTENDED(A)" "UNABLE TO ATTEND(U)")
			    (sequence "READ(r)" "|" "FINISHED READING(F)")
			    (sequence "DISCUSS(d)" "|" "DONE(D)")))
  (setq org-todo-keyword-faces '(("TODO" . (:foreground "orange" :underline t :box nil  :weight extrabold))
				 ("ATTEND" . ( :foreground "orange" :underline t :box nil  :weight extrabold))
				 ("MEET" . ( :foreground "orange" :underline t :box nil  :weight extrabold))
				 ("READ" . ( :foreground "orange" :underline t :box nil  :weight extrabold))
				 ("DISCUSS" . ( :foreground "orange" :underline t :box nil  :weight extrabold))
				 ("CANCELLED" . ( :foreground "gray50" :underline t :box nil))
				 ("DONE" . ( :foreground "gray50" :underline t :box nil))
				 ("ATTENDED" . ( :foreground "gray50" :underline t :box nil))
				 ("MET" . ( :foreground "gray50" :underline t :box nil))
				 ("POSTPONED" . ( :foreground "gray50" :underline t :box nil))
				 ("FINISHED READING" . ( :foreground "gray50" :underline t :box nil))
				 ("UNABLE TO ATTEND" . ( :foreground "gray50" :underline t))))
  (setq org-agenda-prefix-format "%t%2s")
  (setq org-agenda-time-grid '((daily today remove-match)
			       (0900 1100 1300 1500 1700)
			       "      " "................"))
  ;;(setq org-agenda-todo-keyword-format "%-1s")
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
  (cond ((string-equal system-type "gnu/linux") "/usr/bin/notify-send")
	((string-equal system-type "darwin") "/usr/local/bin/terminal-notifier")))  ;; path to libnotify binary notify-send
(defun my-appt-send-notification (msg)
  (shell-command (concat my-notifier-path " -t" " 0 " " -i" " ~/.emacs.d/icons/emacs.png "  msg))) ;; see notify-send help to understand the options

;; designate the window function for my-appt-send-notification
(defun my-appt-display (min-to-app new-time msg)
  (my-appt-send-notification 
   (format "'Appointment in %s minutes\n %s'" min-to-app msg)))
(setq appt-disp-window-function (function my-appt-display))

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

(use-package org-roam
  :straight t
  :init (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/Dropbox/org/roam/")
  :bind (("C-c n l" . org-roam-buffer-toggle)
	 ("C-c n f" . org-roam-node-find)
	 ("C-c n i" . org-roam-node-insert))
  :config
  (org-roam-setup))

(setq auth-sources
      '((:source "~/.config/emacs/.authinfo.gpg")))

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

(use-package bgt
  :straight (bgt :type git :host github :repo "md-arif-shaikh/bgt")
  :config
  (setq bgt-file-name "~/Dropbox/org/bgt.org")
  (setq bgt-csv-file-name "~/Dropbox/org/bgt.csv")
  (setq bgt-python-file "~/bgt/bgt.py")
  (setq bgt-python-path "~/miniconda3/envs/teobresums/bin/python"))

(use-package company-wordfreq
  :straight t)

(defun remove-quail-show-guidance ()
  nil)
(defun remove-quail-completion ()
  (quail-select-current))
(defun bn-company-wordfreq ()
  (interactive)
  (advice-add 'quail-show-guidance :override #'remove-quail-show-guidance)
  (advice-add 'quail-completion :override #'remove-quail-completion)
  (setq ispell-local-dictionary "bengali_439")
  (setq-local company-backends '(company-wordfreq))
  (setq-local company-transformers nil))

(add-to-list 'load-path "~/Downloads/mu-1.6.6/mu4e")
(setq mu4e-mu-binary "~/Downloads/mu-1.6.6/mu/mu")
(require 'mu4e)

(setq mu4e-maildir       "~/Maildir"   ;; top-level Maildir
      ;; note that these folders below must start with /
      ;; the paths are relative to maildir root
      mu4e-sent-folder   "/Sent"
      mu4e-drafts-folder "/Drafts"
      mu4e-trash-folder  "/Trash")

(setq mu4e-get-mail-command  "mbsync -a")
(setq mu4e-update-interval (* 5 60))
;; run in the background
(setq mu4e-index-update-in-background t)
;; update when new mail arrives the headers
(setq mu4e-headers-auto-update t)

(setq mue4e-headers-skip-duplicates  t
      mu4e-view-show-images t
      mu4e-view-show-addresses t
      mu4e-compose-format-flowed t
      mu4e-date-format "%y/%m/%d"
      mu4e-headers-date-format "%Y/%m/%d"
      mu4e-change-filenames-when-moving t
      mu4e-index-cleanup t
      mu4e-index-lazy-check nil
      mu4e-attachments-dir "~/Downloads"
      user-mail-address "arifshaikh.astro@gmail.com"
      user-full-name "Md Arif Shaikh"
      mu4e-compose-signature
      (concat
       "Md Arif Shaikh\n"
       "Postdoctoral Fellow, ICTS-TIFR\n"
       "https://md-arif-shaikh.github.io\n"))

(use-package mu4e-alert
  :straight t
  :config
  (mu4e-alert-set-default-style 'libnotify)
  (add-hook 'after-init-hook #'mu4e-alert-enable-notifications)
  (add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display)
  (setq mu4e-alert-enable-mode-line-display t)
  (setq mu4e-alert-enable-notifications t))

(setq message-send-mail-function 'smtpmail-send-it
     smtpmail-stream-type 'starttls
     smtpmail-default-smtp-server "smtp.gmail.com"
     smtpmail-smtp-server "smtp.gmail.com"
     smtpmail-smtp-service 587
     mu4e-sent-messages-behavior 'delete)

(use-package soccer
  :straight (soccer :type git :host github :repo "md-arif-shaikh/soccer")
  :init
  (setq soccer-leagues-alist
	  '(("England" . "Premier League")
	    ("Spain" . "Laliga")
	    ("France" . "Ligue 1")
	    ("Italy" . "Serie A")
	    ("Germany" . "Bundesliga")
	    ("uefa" . "Champions League")
	    ;;("England" . "Championship")
	    ))
  :config
  (setq soccer-time-local-time-utc-offset "+0530")
  :bind (("C-c s f" . soccer-fixtures-next)
	 ("C-c s r" . soccer-results-last)
	 ("C-c s s" . soccer-scorecard)
	 ("C-c s t" . soccer-table)))

(use-package package-lint
  :straight t)

(when (string-equal system-type "darwin")
  (require 'epa-file)
  (custom-set-variables '(epg-gpg-program  "/opt/local/bin/gpg2"))
  (epa-file-enable))

(use-package dash
  :straight t)

(use-package ht
  :straight t)

(arif/load-file "~/.emacs.d/lisp/lunch-paper.el")

(use-package expenses
  :straight (expenses :type git :host github :repo "md-arif-shaikh/expenses")
  :config
  (setq expenses-category-list '("Grocery" "Food" "Shopping" "Travel" "Subscription" "Health" "Electronics" "Entertainment" "Rent" "Salary" "Others"))
  :bind (("C-c e a" . expenses-add-expense)
	 ("C-c e v" . expenses-view-expense)
	 ("C-c e y" . expenses-calc-expense-for-year)
	 ("C-c e m" . expenses-calc-expense-for-month)
	 ("C-c e d" . expenses-calc-expense-for-day)
	 ("C-c e c" . expenses-calc-expense-by-category))
  )

(use-package elfeed
  :straight t
  :config
  (setq elfeed-feeds
	'(("https://www.theguardian.com/football/rss"))))

(use-package crdt
  :straight t)

(tab-bar-mode)
(setq tab-bar-format '(tab-bar-format-tabs tab-bar-separator tab-bar-format-align-right tab-bar-format-global))
(set-face-attribute 'tab-bar nil :foreground "#FFFFFF")
(add-to-list 'global-mode-string "মহঃ আরিফ শেখ ")
(set-face-attribute 'tab-bar-tab nil :foreground "cyan" :background nil :bold t :box t)

(use-package bn
  :straight (bn :type git :host github :repo "md-arif-shaikh/bn")
  :config
  (setq bn-time-separator ":")
  (setq bn-date-separator "-")
  (display-time-mode 1)
  (display-battery-mode 1)
  (setq display-time-string-forms bn-display-time-string-forms)
  (advice-add 'battery-update :override #'bn-battery-update)
  (add-hook 'after-change-major-mode-hook 'bn-set-major-mode-name)
  (advice-add 'appt-mode-line :override #'bn-appt-mode-line)
  (advice-add #'vc-git-mode-line-string :filter-return #'bn-vc-git-mode-line-string))

(setq-default mode-line-format
	      '("-"
		(:eval (let ((str (if buffer-read-only
				      (if (buffer-modified-p) "%%*" "%%%%")
				    (if (buffer-modified-p) "পরিবর্তিত" "--"))))
			 str))
		mode-line-frame-identification
		;mode-line-buffer-identification
		"   "
		mode-line-position
		(vc-mode vc-mode)
		"   "
		mode-name
		(which-function-mode ("" which-func-format "--"))))

(use-package popup
  :straight t)
