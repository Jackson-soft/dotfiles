;;; init-completion.el --- Config for completion   -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package icomplete
  :ensure nil
  :hook (after-init . icomplete-vertical-mode)
  :bind (:map icomplete-minibuffer-map
			  ("RET" . icomplete-fido-ret)
			  ("C-k" . icomplete-fido-kill)
			  ("C-d" . icomplete-fido-delete-char)
			  ("DEL" . icomplete-fido-backward-updir))
  :config
  (setq icomplete-in-buffer t
		icomplete-tidy-shadowed-file-names t
		icomplete-show-matches-on-no-input t
		icomplete-scroll t)
  )

(use-package consult
  :bind (;; C-c bindings (mode-specific-map)
		 ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
		 ;; C-x bindings (ctl-x-map)
		 ([remap repeat-complex-command] . consult-complex-command)     ;; C-x M-:
		 ([remap switch-to-buffer] . consult-buffer) ;; C-x b
		 ([remap project-switch-to-buffer] . consult-project-buffer)  ;; C-x p b
		 ("C-x C-r" . consult-recent-file)
		 ([remap bookmark-jump] . consult-bookmark)            ;; C-x r b
		 ;; Other custom bindings
		 ([remap yank-pop] . consult-yank-pop)  ;; M-y
		 ;; M-g bindings (goto-map)
		 ("M-g f" . consult-flymake)
		 ([remap goto-line] . consult-goto-line) ;; M-g g
		 ("M-g o" . consult-outline)
		 ("M-g i" . consult-imenu)
		 ("M-g I" . consult-imenu-multi)
		 ;; M-s bindings (search-map)
		 ("M-s d" . consult-find)
		 ("M-s D" . consult-locate)
		 ("M-s g" . consult-grep)
		 ("M-s G" . consult-git-grep)
		 ("M-s r" . consult-ripgrep)
		 ("M-s l" . consult-line)
		 ("M-s L" . consult-line-multi)
		 ("M-s k" . consult-keep-lines)
		 ("M-s u" . consult-focus-lines)
		 ;; Isearch integration
		 ("M-s e" . consult-isearch-history)
		 :map isearch-mode-map
		 ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
		 ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
		 ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
		 ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
		 ;; Minibuffer history
		 :map minibuffer-local-map
		 ("M-s" . consult-history)                 ;; orig. next-matching-history-element
		 ("M-r" . consult-history))                ;; orig. previous-matching-history-element
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :custom
  (register-preview-delay 0.5)
  (register-preview-function #'consult-register-format)
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  :config
  (setq consult-preview-key "M-.")

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"
  (advice-add #'register-preview :override #'consult-register-window)
  )

(use-package consult-dir
  :bind (("C-x C-d" . consult-dir)
		 :map minibuffer-local-completion-map
		 ("C-x C-d" . consult-dir)
		 ("C-x C-j" . consult-dir-jump-file))
  )

(use-package marginalia
  :hook (after-init . marginalia-mode)
  )

(use-package embark
  :bind (("C-h B" . embark-bindings) ;; alternative for `describe-bindings'
		 ("M-n" . embark-next-symbol)
		 ("M-p" . embark-previous-symbol))
  :custom
  (prefix-help-command #'embark-prefix-help-command)
  (embark-cycle-key ".")
  (embark-help-key "?")
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
			   '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
				 nil
				 (window-parameters (mode-line-format . none))))
  )

(use-package embark-consult
  :hook (embark-collect-mode . consult-preview-at-point-mode)
  )

(use-package all-the-icons-completion
  :hook (emacs-startup . all-the-icons-completion-mode)
  )

(provide 'init-completion)

;;; init-completion.el ends here
