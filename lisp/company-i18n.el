;;; From https://lifeofpenguin.blogspot.com/2021/05/transliteration-in-emacs.html
(make-variable-buffer-local 'company-minimum-prefix-length)
(defvar company-i18n-input-hash nil)
(make-variable-buffer-local 'company-i18n-input-hash)

(defun company-i18n (command &optional arg start end)
  "`company-mode' completion backend for transliteration."
  (interactive (list 'interactive))
  (setq company-minimum-prefix-length 1)
  (cl-case command
    (interactive (company-begin-backend 'company-i18n))
    (prefix (company-grab-symbol))
    (candidates
     (list (or (company-i18n 'trans arg) arg)))
    (trans
     (let* ((hash company-i18n-input-hash)
            (ascii (gethash arg (car hash)))
            (len (length arg))
            mark txt)
       (cond ((> len 3)
              ;; split after first consonant + vowel group
              (save-excursion
                (if start
                    (goto-char start)
                  (forward-word -1)
                  (setq start (point)))
                (skip-chars-forward "^aeiou")
                (setq mark
                      (re-search-forward "[^aeiou]" (+ start len) t))
                )

              (when mark
                (setq len (- mark start 1))
                ;; (message "%d %s %s" len (substring arg 0 len)
                ;;          (substring arg len))
                (setq txt (concat
                           (company-i18n 'trans (substring arg 0 len) start mark)
                           (company-i18n 'trans (substring arg len) (1- mark) end))))

              (unless mark
                (setq txt (gethash arg (cdr hash))))

              txt)
             (t
              (or (gethash arg (cdr hash))
                  (and (gethash (substring arg 0 2) (cdr hash))
                       (concat
                        (gethash (substring arg 0 2) (cdr hash))
                        (gethash (substring arg 2) (cdr hash))))
                  (concat
                   (gethash (substring arg 0 1) (cdr hash))
                   (company-i18n 'trans (substring arg 1))))
              )
             ))
     )
    (no-cache t)
    (duplicates nil)
    (sorted nil)
    ))
(add-to-list 'company-backends 'company-i18n)
(require 'ind-util)
(setq company-i18n-input-hash indian-bng-itrans-v5-hash)
