;; init-edit --- Initialize ui configurations.  -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;
;; Editing configurations.
;;
;;; Code:

;; brew install enchant

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
  :bind (("M-U" . upcase-char) ;; 当前光标字母变大写
		 ("M-z" . zap-up-to-char)
		 ("M-Z" . zap-to-char)) ;; M-S-z
  )

;; Highlight parenthesises
(use-package paren
  :ensure nil
  :config
  (setopt show-paren-when-point-inside-paren t
		  show-paren-when-point-in-periphery t
          blink-matching-paren-highlight-offscreen t
		  show-paren-context-when-offscreen 'child-frame)
  )

;; Automatic parenthesis pairing
(use-package elec-pair
  :ensure nil
  :hook (after-init . electric-pair-mode)
  :config
  (setopt electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
  )

(use-package electric
  :ensure nil
  :hook (after-init . electric-indent-mode)
  )

(use-package vundo
  :bind ("C-x u" . vundo)
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols)
  )

(use-package uniquify
  :ensure nil
  :config
  (setq uniquify-buffer-name-style 'forward
		uniquify-separator " • "
		uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers
  )

;; search
(use-package isearch
  :ensure nil
  :bind (:map isearch-mode-map
			  ("C-c C-o"                   . isearch-occur)
			  ([escape]                    . isearch-cancel)
			  ;; Edit the search string instead of jumping back
			  ([remap isearch-delete-char] . isearch-del-char))
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
  :custom
  (list-matching-lines-jump-to-current-line t)
  )

(use-package visual-replace
  :bind (("C-c r" . visual-replace)
         :map isearch-mode-map
         ("C-c r" . visual-replace-from-isearch))
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

;; 单词拼写检查
(use-package jinx
  :hook (emacs-startup . global-jinx-mode)
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages))
  :config
  (setq jinx-languages "en_US")
  ;; 中文不检查
  (add-to-list 'jinx-exclude-regexps '(t "\\cc"))
  )

;; (use-package ispell
;;   :ensure nil
;;   :config
;;   (setq ispell-following-word t
;;		ispell-quietly t         ;; Supress messages in ispell-word
;;		;; ispell-program-name "aspell"
;;		ispell-program-name "enchant-2"
;;		;; ispell-dictionary "en_US"
;;		;; ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together" "--camel-case" "--run-together-limit=16"))
;;		)
;;   )

;; ;; 单词拼写检查
;; (use-package flyspell
;;   :ensure nil
;;   :hook (((text-mode outline-mode) . flyspell-mode)
;;		 (prog-mode . flyspell-prog-mode))
;;   :config
;;   (setq flyspell-issue-welcome-flag nil
;;		flyspell-issue-message-flag nil)
;;   )

(provide 'init-edit)

;;; init-edit.el ends here
