;;; emacs-bn.el --- package to display messages in Bangla
;;; commentary:
;;; code:
(defun find-file-other-window-bn (filename &optional wildcards)
  "Edit file FILENAME, in another window.

Like \\[find-file] (which see), but creates a new window or reuses
an existing one.  See the function `display-buffer'.

Interactively, the default if you just type RET is the current directory,
but the visited file name is available through the minibuffer history:
type \\[next-history-element] to pull it into the minibuffer.

The first time \\[next-history-element] is used after Emacs prompts for
the file name, the result is affected by `file-name-at-point-functions',
which by default try to guess the file name by looking at point in the
current buffer.  Customize the value of `file-name-at-point-functions'
or set it to nil, if you want only the visited file name and the
current directory to be available on first \\[next-history-element]
request.

Interactively, or if WILDCARDS is non-nil in a call from Lisp,
expand wildcards (if any) and visit multiple files."
  (interactive
   (find-file-read-args "অন্য উইন্ডোতে ফাইল খুজুন: "
                        (confirm-nonexistent-file-or-buffer)))
  (let ((value (find-file-noselect filename nil nil wildcards)))
    (if (listp value)
	(progn
	  (setq value (nreverse value))
	  (switch-to-buffer-other-window (car value))
	  (mapc 'switch-to-buffer (cdr value))
	  value)
      (switch-to-buffer-other-window value))))

(defun find-file-bn (filename &optional wildcards)
  "Edit file FILENAME.
Switch to a buffer visiting file FILENAME,
creating one if none already exists.
Interactively, the default if you just type RET is the current directory,
but the visited file name is available through the minibuffer history:
type \\[next-history-element] to pull it into the minibuffer.

The first time \\[next-history-element] is used after Emacs prompts for
the file name, the result is affected by `file-name-at-point-functions',
which by default try to guess the file name by looking at point in the
current buffer.  Customize the value of `file-name-at-point-functions'
or set it to nil, if you want only the visited file name and the
current directory to be available on first \\[next-history-element]
request.

You can visit files on remote machines by specifying something
like /ssh:SOME_REMOTE_MACHINE:FILE for the file name.  You can
also visit local files as a different user by specifying
/sudo::FILE for the file name.
See the Info node `(tramp)File name Syntax' in the Tramp Info
manual, for more about this.

Interactively, or if WILDCARDS is non-nil in a call from Lisp,
expand wildcards (if any) and visit multiple files.  You can
suppress wildcard expansion by setting `find-file-wildcards' to nil.

To visit a file without any kind of conversion and without
automatically choosing a major mode, use \\[find-file-literally]."
  (interactive
   (find-file-read-args "ফাইল খুজুন: "
                        (confirm-nonexistent-file-or-buffer)))
  (let ((value (find-file-noselect filename nil nil wildcards)))
    (if (listp value)
	(mapcar 'pop-to-buffer-same-window (nreverse value))
      (pop-to-buffer-same-window value))))

(defun counsel-find-file-bn (&optional initial-input initial-directory)
  "Forward to `find-file'.
When INITIAL-INPUT is non-nil, use it in the minibuffer during completion."
  (interactive)
  (let ((default-directory (or initial-directory default-directory)))
    (counsel--find-file-1 "ফাইল খুজুন: " initial-input
                          #'counsel-find-file-action
                          'counsel-find-file)))

(defun save-buffer-bn (&optional arg)
  "Save current buffer in visited file if modified.
Variations are described below.

By default, makes the previous version into a backup file
 if previously requested or if this is the first save.
Prefixed with one \\[universal-argument], marks this version
 to become a backup when the next save is done.
Prefixed with two \\[universal-argument]'s,
 makes the previous version into a backup file.
Prefixed with three \\[universal-argument]'s, marks this version
 to become a backup when the next save is done,
 and makes the previous version into a backup file.

With a numeric prefix argument of 0, never make the previous version
into a backup file.

Note that the various variables that control backups, such
as `version-control', `backup-enable-predicate', `vc-make-backup-files',
and `backup-inhibited', to name just the more popular ones, still
control whether a backup will actually be produced, even when you
invoke this command prefixed with two or three \\[universal-argument]'s.

If a file's name is FOO, the names of its numbered backup versions are
 FOO.~i~ for various integers i.  A non-numbered backup file is called FOO~.
Numeric backups (rather than FOO~) will be made if value of
 `version-control' is not the atom `never' and either there are already
 numeric versions of the file being backed up, or `version-control' is
 non-nil.
We don't want excessive versions piling up, so there are variables
 `kept-old-versions', which tells Emacs how many oldest versions to keep,
 and `kept-new-versions', which tells how many newest versions to keep.
 Defaults are 2 old versions and 2 new.
`dired-kept-versions' controls dired's clean-directory (.) command.
If `delete-old-versions' is nil, system will query user
 before trimming versions.  Otherwise it does it silently.

If `vc-make-backup-files' is nil, which is the default,
 no backup files are made for files managed by version control.
 (This is because the version control system itself records previous versions.)

See the subroutine `basic-save-buffer' for more information."
  (interactive "p")
  (let ((modp (buffer-modified-p))
	(make-backup-files (or (and make-backup-files (not (eq arg 0)))
			       (memq arg '(16 64)))))
    (and modp (memq arg '(16 64)) (setq buffer-backed-up nil))
    ;; We used to display the message below only for files > 50KB, but
    ;; then Rmail-mbox never displays it due to buffer swapping.  If
    ;; the test is ever re-introduced, be sure to handle saving of
    ;; Rmail files.
    (if (and modp
             (buffer-file-name)
             (not noninteractive)
             (not save-silently))
	(message "সংরক্ষণ করা হচ্ছে %s..." (buffer-file-name)))
    (basic-save-buffer (called-interactively-p 'any))
    (and modp (memq arg '(4 64)) (setq buffer-backed-up nil))))


(defun basic-save-buffer-bn (&optional called-interactively)
  "Save the current buffer in its visited file, if it has been modified.

The hooks `write-contents-functions', `local-write-file-hooks'
and `write-file-functions' get a chance to do the job of saving;
if they do not, then the buffer is saved in the visited file in
the usual way.

Before and after saving the buffer, this function runs
`before-save-hook' and `after-save-hook', respectively."
  (interactive '(called-interactively))
  (save-current-buffer
    ;; In an indirect buffer, save its base buffer instead.
    (if (buffer-base-buffer)
	(set-buffer (buffer-base-buffer)))
    (if (or (buffer-modified-p)
	    ;; Handle the case when no modification has been made but
	    ;; the file disappeared since visited.
	    (and buffer-file-name
		 (not (file-exists-p buffer-file-name))))
	(let ((recent-save (recent-auto-save-p))
	      setmodes)
	  (or (null buffer-file-name)
              (verify-visited-file-modtime (current-buffer))
	      (not (file-exists-p buffer-file-name))
	      (yes-or-no-p
	       (format
		"%s শেষ সংরক্ষণ করার বা দেখার পর পরিবর্তিত হয়েছে, তবুও সংরক্ষণ করতে চান? "
		(file-name-nondirectory buffer-file-name)))
	      (user-error "সংরক্ষণ যাচাই করা যায়নি"))
	  (save-restriction
	    (widen)
	    (save-excursion
	      (and (> (point-max) (point-min))
		   (not find-file-literally)
                   (null buffer-read-only)
		   (/= (char-after (1- (point-max))) ?\n)
		   (not (and (eq selective-display t)
			     (= (char-after (1- (point-max))) ?\r)))
		   (or (eq require-final-newline t)
		       (eq require-final-newline 'visit-save)
		       (and require-final-newline
			    (y-or-n-p
			     (format "Buffer %s does not end in newline.  Add one? "
				     (buffer-name)))))
		   (save-excursion
		     (goto-char (point-max))
		     (insert ?\n))))
	    ;; Don't let errors prevent saving the buffer.
	    (with-demoted-errors (run-hooks 'before-save-hook))
            ;; Give `write-contents-functions' a chance to
            ;; short-circuit the whole process.
	    (unless (run-hook-with-args-until-success 'write-contents-functions)
              ;; If buffer has no file name, ask user for one.
              (or buffer-file-name
                  (let ((filename
                         (expand-file-name
                          (read-file-name "সংরক্ষণ করার জন্যে ফাইল: "
                                          nil (expand-file-name (buffer-name))))))
                    (if (file-exists-p filename)
                        (if (file-directory-p filename)
                            ;; Signal an error if the user specified the name of an
                            ;; existing directory.
                            (error "%s একটি ডিরেক্টরি" filename)
                          (unless (y-or-n-p (format-message
                                             "ফাইল `%s' বর্তমান; পুনর্লিখন? "
                                             filename))
                            (error "বাতিল করা হয়েছে"))))
                    (set-visited-file-name filename)))
              ;; Support VC version backups.
	      (vc-before-save)
	      (or (run-hook-with-args-until-success 'local-write-file-hooks)
	          (run-hook-with-args-until-success 'write-file-functions)
	          ;; If a hook returned t, file is already "written".
	          ;; Otherwise, write it the usual way now.
	          (let ((dir (file-name-directory
			      (expand-file-name buffer-file-name))))
		    (unless (file-exists-p dir)
		      (if (y-or-n-p
		           (format-message
                            "ডিরেক্টরি `%s' বর্তমান না; ?  তৈরী করুন?" dir))
		          (make-directory dir t)
		        (error "বাতিল করা হয়েছে")))
		    (setq setmodes (basic-save-buffer-1)))))
	    ;; Now we have saved the current buffer.  Let's make sure
	    ;; that buffer-file-coding-system is fixed to what
	    ;; actually used for saving by binding it locally.
            (when buffer-file-name
	      (if save-buffer-coding-system
		  (setq save-buffer-coding-system last-coding-system-used)
	        (setq buffer-file-coding-system last-coding-system-used))
	      (setq buffer-file-number
		    (nthcdr 10 (file-attributes buffer-file-name)))
	      (if setmodes
		  (condition-case ()
		      (progn
		        (unless
			    (with-demoted-errors
			        (set-file-modes buffer-file-name (car setmodes)))
			  (set-file-extended-attributes buffer-file-name
						        (nth 1 setmodes))))
		    (error nil)))
              ;; Support VC `implicit' locking.
	      (vc-after-save))
            ;; If the auto-save file was recent before this command,
	    ;; delete it now.
	    (delete-auto-save-file-if-necessary recent-save))
	  (run-hooks 'after-save-hook))
      (or noninteractive
          (not called-interactively)
          (files--message "(সংরক্ষণ করার মত কোন পরিবর্তন হয়নি)")))))

(defun ivy-switch-buffer-bn ()
  "Switch to another buffer."
  (interactive)
  (ivy-read "বাফার পরিবর্তন: " #'internal-complete-buffer
            :keymap ivy-switch-buffer-map
            :preselect (buffer-name (other-buffer (current-buffer)))
            :action #'ivy--switch-buffer-action
            :matcher #'ivy--switch-buffer-matcher
            :caller 'ivy-switch-buffer))

(defun doom-modeline-update-flycheck-text-bn (&optional status)
  "Update flycheck text via STATUS."
  (setq doom-modeline--flycheck-text
        (when-let
            ((text
              (pcase status
                ('finished  (when flycheck-current-errors
                              (let-alist (doom-modeline--flycheck-count-errors)
                                (if doom-modeline-checker-simple-format
                                    (doom-modeline-checker-text
                                     (number-to-bn (+ .error .warning .info))
                                     (cond ((> .error 0) 'doom-modeline-urgent)
                                           ((> .warning 0) 'doom-modeline-warning)
                                           (t 'doom-modeline-info)))
                                  (format "%s/%s/%s"
                                          (doom-modeline-checker-text (number-to-bn .error)
                                                                      'doom-modeline-urgent)
                                          (doom-modeline-checker-text (number-to-bn .warning)
                                                                      'doom-modeline-warning)
                                          (doom-modeline-checker-text (number-to-bn .info)
                                                                      'doom-modeline-info))))))
                ('running     nil)
                ('no-checker  nil)
                ('errored     (doom-modeline-checker-text "ভুল" 'doom-modeline-urgent))
                ('interrupted (doom-modeline-checker-text "বাধাপ্রাপ্ত" 'doom-modeline-debug))
                ('suspicious  (doom-modeline-checker-text "সন্দেহজনক" 'doom-modeline-urgent))
                (_ nil))))
          (propertize
           text
           'help-echo (pcase status
                        ('finished
                         (concat
                          (when flycheck-current-errors
                            (let-alist (doom-modeline--flycheck-count-errors)
                              (format "ভুল: %s, সতর্কতা: %s, তথ্য: %s\n" (number-to-bn .error) (number-to-bn .warning) (number-to-bn .info))))
                          "মাউস-১: সব ভুলগুলি দেখাও
মাউস-২: পরবর্তী ভুল"
                          (if (featurep 'mwheel)
                              "\nwheel-up/wheel-down: প্রাক/পরবর্তী ভুল")))
                        ('running "চলমান...")
                        ('no-checker "কোন চেকার নেই")
                        ('errored "ভুল")
                        ('interrupted "বাধাপ্রাপ্ত")
                        ('suspicious "সন্দেহজনক"))
           'mouse-face 'mode-line-highlight
           'local-map (let ((map (make-sparse-keymap)))
                        (define-key map [mode-line mouse-1]
                          #'flycheck-list-errors)
                        (define-key map [mode-line mouse-3]
                          #'flycheck-next-error)
                        (when (featurep 'mwheel)
                          (define-key map (vector 'mode-line
                                                  mouse-wheel-down-event)
                            (lambda (event)
                              (interactive "e")
                              (with-selected-window (posn-window (event-start event))
                                (flycheck-previous-error 1))))
                          (define-key map (vector 'mode-line
                                                  mouse-wheel-up-event)
                            (lambda (event)
                              (interactive "e")
                              (with-selected-window (posn-window (event-start event))
                                (flycheck-next-error 1))))
                          map))))))

(use-package cyphejor
  :straight t
  :config
  (setq
 cyphejor-rules
 '(:upcase
   ("bookmark"    "→")
   ("help" "হেল্প")
   ("org" "অর্গ")
   ("agenda" "এজেন্ডা")
   ("magit" "ম্যাগিট")
   ("status" "স্ট্যাটাস")
   ("buffer"      "বাফার")
   ("diff"        "ডিফ")
   ("dired"       "ডায়ার্ড")
   ("emacs"       "ইমাক্স")
   ("inferior"    "i" :prefix)
   ("interaction" "ইন্টারেক্শন" :prefix)
   ("interactive" "ইন্টারেক্টিভ" :prefix)
   ("lisp"        "লিস্প" :postfix)
   ("menu"        "▤" :postfix)
   ("mode"        "")
   ("package"     "↓")
   ("python"      "পাইথন")
   ("shell"       "শেল" :postfix)
   ("eshell" "ইশেল")
   ("text"        "টেক্স্ট")
   ("latex" "লেটেক্স")
   ("pdf" "পিডিএফ")
   ("view" "ভিউ")))
  (cyphejor-mode 1))

(advice-add 'find-file-other-window :override #'find-file-other-window-bn)
(advice-add 'find-file :override #'find-file-bn)
(advice-add 'counsel-find-file :override #'counsel-find-file-bn)
(advice-add 'save-buffer :override #'save-buffer-bn)
(advice-add 'basic-save-buffer :override #'basic-save-buffer-bn)
(advice-add 'ivy-switch-buffer :override #'ivy-switch-buffer-bn)
(advice-add 'doom-modeline-update-flycheck-text :override #'doom-modeline-update-flycheck-text-bn)

(provide 'emacs-bn)
;;; emacs-bn.el ends here
