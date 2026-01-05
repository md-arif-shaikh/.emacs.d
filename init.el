;;; init.el --- initialization           -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Md Arif Shaikh
;; Author: Md Arif Shaikh <arifshaikh.astro@gmail.com>
;; Keywords: init
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; 
;;; Code:

;; Set font
(set-face-attribute 'default nil :font "Fira Code Retina" :height 140)

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 2))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent-buffer)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Configure use-package to use straight.el
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; Ivy
(use-package ivy
  :diminish ivy-mode
  :config
  (ivy-mode 1))

;; Doom Modeline
(use-package doom-modeline
  :init
  (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 15))

;; Doom Themes
(use-package doom-themes
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :config
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config))

;; Rainbow Delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Which Key
(use-package which-key
  :init
  (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

;; Dired & Nerd Icons Dired
(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

(add-hook 'dired-mode-hook #'dired-hide-details-mode)
(add-hook 'dired-mode-hook #'dired-omit-mode)
(require 'dired-x)
(setq dired-omit-files
      (concat dired-omit-files
	      "\\|^\\..+$"
              "\\|^\\.DS_Store$"   ; exactly .DS_Store
              "\\|^\\.\\#.*"       ; autosave/lock files
              "\\|^\\.git$"        ; .git dir
              "\\|^__pycache__$")) ; python cache

;; Magit
(use-package magit)

;; LSP Mode
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((python-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;; Pyright
(use-package lsp-pyright
  :custom
  (lsp-pyright-langserver-command "pyright")
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred))))

;; LSP UI
(use-package lsp-ui
  :commands lsp-ui-mode)

;; LSP Ivy
(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol)

;; LSP Treemacs
(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list)

;; Company Autocompletion
(use-package company
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-backends '(company-capf))
  (setq lsp-completion-provider :capf)
  (setq company-dabbrev-downcase 0)
  (setq company-idle-delay 0.1)
  (setq company-minimum-prefix-length 1)
  (setq company-tooltip-align-annotations t))

;; Company Box
(use-package company-box
  :hook (company-mode . company-box-mode))

;;;; ===============================
;;;; LaTeX (AUCTeX)
;;;; ===============================

(use-package auctex
  :straight t
  :defer t
  :mode ("\\.tex\\'" . LaTeX-mode)
  :init
  (setq TeX-parse-self t
        TeX-auto-save t
        TeX-save-query nil
        TeX-PDF-mode t
        TeX-master nil

        ;; SyncTeX
        TeX-source-correlate-mode t
        TeX-source-correlate-start-server t)

  :config
  ;; Use PDF Tools as viewer (NO external viewer)
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view)))

  ;; Editing conveniences
  (add-hook 'LaTeX-mode-hook #'visual-line-mode)
  (add-hook 'LaTeX-mode-hook #'flyspell-mode)
  (add-hook 'LaTeX-mode-hook #'LaTeX-math-mode)

  ;; RefTeX
  (use-package reftex
    :defer t
    :hook (LaTeX-mode . reftex-mode)
    :config
    (setq reftex-plug-into-AUCTeX t
          reftex-cite-prompt-optional-args t)))

;;;; ===============================
;;;; PDF Tools
;;;; ===============================

(use-package pdf-tools
  :straight t
  :defer t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :commands (pdf-tools-install)
  :init
  (setq pdf-view-display-size 'fit-height
        pdf-view-recenter-relative nil
        auto-revert-use-notify nil
        auto-revert-verbose nil
        revert-without-query '(".*\\.pdf"))
  :config
  (pdf-tools-install :no-query)

  (add-hook 'pdf-view-mode-hook
            (lambda ()
              (setq-local auto-revert-use-notify nil)
              (setq-local auto-revert-interval 1)
              (auto-revert-mode 1)))

  (define-key pdf-view-mode-map (kbd "C-c C-v")
              #'TeX-pdf-tools-sync-view))


;;;; ===============================
;;;; Company + LaTeX
;;;; ===============================

(use-package company-auctex
  :straight t
  :after (company auctex)
  :hook (LaTeX-mode . company-auctex-init))

(use-package company-reftex
  :straight t
  :after (company reftex))

(add-hook 'LaTeX-mode-hook
          (lambda ()
            (setq-local company-backends
                        '((company-auctex
                           company-reftex-labels
                           company-reftex-citations
                           company-dabbrev
                           company-dabbrev-code)))))

;; Yasnippet + LaTeX integration
(use-package yasnippet
  :straight t
  :hook ((LaTeX-mode . yas-minor-mode)
         (prog-mode . yas-minor-mode))
  :config
  ;; Load your snippets (default is ~/.emacs.d/snippets/)
  (yas-reload-all))

(use-package yasnippet-snippets
  :straight t
  :after yasnippet)

;; expenses
(use-package expenses
  :straight (expenses :type git :host github :repo "md-arif-shaikh/expenses")
  :config
  (setq expenses-category-list '("Grocery" "Food" "Shopping" "Travel" "Subscription" "Health" "Electronics" "Entertainment" "Rent" "Salary" "Gas" "Cofee" "Others")
	expenses-directory "~/Dropbox/Important_Works/Expenses/Monthly_expenses/"
	expenses-python-path "~/miniconda3/bin/python3"
	expenses-default-user-name "arif")
  :bind (("C-c e a" . expenses-add-expense)
	 ("C-c e v" . expenses-view-expense)
	 ("C-c e y" . expenses-calc-expense-for-year)
	 ("C-c e m" . expenses-calc-expense-for-month)
	 ("C-c e d" . expenses-calc-expense-for-day)
	 ("C-c e c" . expenses-calc-expense-by-category)))

;; load custom files
(load-file "~/.emacs.d/lisp/arxiv-search.el")
(load-file "~/.config/emacs/custom-commands.el")
(load-file "~/.emacs.d/lisp/download-url.el")

;; open pdf inside emacs
(add-to-list 'org-file-apps '("\\.pdf\\'" . emacs))

;; add line numbers in python mode
(add-hook 'python-mode-hook #'display-line-numbers-mode)

;; add copilot
(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :ensure t)
(add-hook 'prog-mode-hook 'copilot-mode)
(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)

;; electric pair mode
(electric-pair-mode 1)

;; vterm
(use-package vterm
  :straight t
  :commands vterm
  :config
  (setq vterm-shell "/opt/homebrew/bin/fish"))

;; End of file
(provide 'init)
;;; init.el ends here
