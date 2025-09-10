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

;; LaTeX with AUCTeX + PDF Tools
(use-package auctex
  :straight t
  :defer t
  :mode ("\\.tex\\'" . LaTeX-mode)
  :init
  (setq TeX-parse-self t
        TeX-auto-save t
        TeX-save-query nil
        TeX-PDF-mode t
        TeX-source-correlate-mode t
        TeX-source-correlate-start-server t
        TeX-master nil)
  :config
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view)))

  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer)

  ;; Editing conveniences
  (add-hook 'LaTeX-mode-hook #'visual-line-mode)
  (add-hook 'LaTeX-mode-hook #'flyspell-mode)
  (add-hook 'LaTeX-mode-hook #'LaTeX-math-mode)

  ;; RefTeX loaded only when LaTeX-mode starts
  (use-package reftex
    :defer t
    :hook (LaTeX-mode . reftex-mode)
    :config
    (setq reftex-plug-into-AUCTeX t
          reftex-cite-prompt-optional-args t)))

;; PDF Tools
(use-package pdf-tools
  :straight t
  :defer t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :commands (pdf-tools-install)
  :init
  ;; don’t run install at startup, only when you first open a PDF
  (setq pdf-view-display-size 'fit-width)
  :config
  (pdf-tools-install :no-query)

  ;; SyncTeX + auto-revert
  (add-hook 'pdf-view-mode-hook #'pdf-sync-minor-mode)
  (add-hook 'pdf-view-mode-hook #'auto-revert-mode)

  ;; Forward search
  (define-key pdf-view-mode-map (kbd "C-c C-v")
              #'TeX-pdf-tools-sync-view))

;; Company backends for LaTeX
;; AUCTeX completions
(use-package company-auctex
  :straight t
  :after (company auctex)
  :hook (LaTeX-mode . (lambda ()
                        (company-auctex-init))))

;; RefTeX completions (labels + citations)
(use-package company-reftex
  :straight t
  :after (company reftex)
  :hook (LaTeX-mode . (lambda ()
                        (add-to-list 'company-backends 'company-reftex-labels)
                        (add-to-list 'company-backends 'company-reftex-citations))))

;; Extra completion sources in LaTeX
(add-hook 'LaTeX-mode-hook
          (lambda ()
            ;; Add math-mode completions
            (LaTeX-math-mode 1)
            ;; Use dabbrev for words in current + other buffers
            (setq-local company-backends
                        (append '((company-auctex
                                   company-reftex-labels
                                   company-reftex-citations
                                   company-dabbrev
                                   company-dabbrev-code))
                                company-backends))))

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

;; End of file
(provide 'init)
;;; init.el ends here
