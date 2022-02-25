;; init-edit --- Initialize ui configurations.  -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;
;; Editing configurations.
;;
;;; Code:

;; sudo pacman -S aspell-en

(defun kill-lines (line)
  "Call delete range line form LINE."
  (interactive "s:")
  (let* ((line (split-string line ","))
         (begin (string-to-number (nth 0 line)))
         (end (string-to-number (nth 1 line)))
         )
    (save-excursion
      (goto-line begin)
      (kill-line (- end begin))))
  )

(global-set-key (kbd "C-c C-k") 'kill-lines)

(use-package display-line-numbers
  :ensure nil
  :hook(after-init . global-display-line-numbers-mode)
  :config
  (setq display-line-numbers-width-start t)
  )

;; Delete selection if you insert
(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode)
  )

;; use zap-up-to-char instead of zap-to-char
(use-package misc
  :ensure nil
  :bind (("M-z" . zap-up-to-char)
         ("M-Z" . zap-to-char)) ;; M-S-z
  )

;; Highlight parenthesises
(use-package paren
  :ensure nil
  :config
  (setq show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t)
  )

;; Automatic parenthesis pairing
(use-package elec-pair
  :ensure nil
  :hook (after-init . electric-pair-mode)
  :config
  (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
  )

(use-package electric
  :ensure nil
  :hook (after-init . electric-indent-mode)
  )

(use-package uniquify
  :ensure nil
  :config
  (setq uniquify-buffer-name-style 'forward
        uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers
  )

;; search
(use-package isearch
  :ensure nil
  :bind (:map isearch-mode-map
              ("C-c C-o" . isearch-occur)
              ([escape] . isearch-cancel))
  :config
  (setq isearch-allow-motion t  ;; M-< and M-> move to the first/last occurrence of the current search string.
        isearch-motion-changes-direction t
        isearch-regexp-lax-whitespace t
        isearch-resume-in-command-history t
        isearch-lazy-count t         ;; lazy isearch
        isearch-repeat-on-direction-change t
        lazy-highlight-buffer t)
  )

;; replace
(use-package replace
  :ensure nil
  :config
  (setq list-matching-lines-jump-to-current-line t)
  )

;; 多块编辑
(use-package iedit
  :bind (("C-x i" . iedit-mode)
         ("C-x r RET" . iedit-rectangle-mode))
  )

;; 多光标编辑
(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->"         . mc/mark-next-like-this)
         ("C-<"         . mc/mark-previous-like-this)
         ("C-c C-<"     . mc/mark-all-like-this)
         ("C-M->"       . mc/skip-to-next-like-this)
         ("C-M-<"       . mc/skip-to-previous-like-this))
  )

(use-package ispell
  :ensure nil
  :bind (("C-c i c" . ispell-comments-and-strings)
         ("C-c i p" . ispell-comment-or-string-at-point)
         ("C-c i k" . ispell-kill-ispell)
         ("C-c i m" . ispell-message)
         ("C-c i r" . ispell-region))
  :config
  (setq ispell-following-word t
        ispell-quietly t         ;; Supress messages in ispell-word
        ispell-program-name "aspell"
        ispell-dictionary "en_US"
        ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together" "--camel-case" "--run-together-limit=16"))
  )

;; 单词拼写检查
(use-package flyspell
  :ensure nil
  :hook (((text-mode outline-mode) . flyspell-mode)
         (prog-mode . flyspell-prog-mode))
  :config
  (setq flyspell-issue-welcome-flag nil
        flyspell-issue-message-flag nil)
  )

(provide 'init-edit)

;;; init-edit.el ends here
