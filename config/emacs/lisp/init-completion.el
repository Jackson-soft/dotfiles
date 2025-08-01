;;; init-completion.el --- Config for completion   -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package icomplete
  :ensure nil
  :hook
  (after-init . fido-vertical-mode)
  :custom
  (icomplete-in-buffer t)
  (icomplete-tidy-shadowed-file-names t)
  (icomplete-show-matches-on-no-input t)
  (icomplete-scroll t)
  )

;; Optionally use the `orderless' completion style.
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion))
                                   (eglot (styles orderless))))
  )

;; (use-package vertico
;;   :bind (:map vertico-map
;;               ("RET" . vertico-directory-enter)
;;               ("DEL" . vertico-directory-delete-char)
;;               ("M-DEL" . vertico-directory-delete-word))
;;   :hook ((after-init . vertico-mode)
;;          (rfn-eshadow-update-overlay . vertico-directory-tidy))
;;   :config
;;   (setq vertico-cycle t
;;         vertico-resize t)
;;   )

(use-package consult
  :bind
  (;; C-c bindings (mode-specific-map)
   ("C-c M-x" . consult-mode-command)
   ("C-c h" . consult-history)
   ("C-c k" . consult-kmacro)
   ("C-c m" . consult-man)
   ("C-c i" . consult-info)
   ;; C-x bindings (ctl-x-map)
   ([remap repeat-complex-command] . consult-complex-command)     ;; C-x M-:
   ([remap switch-to-buffer] . consult-buffer) ;; C-x b
   ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
   ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
   ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
   ([remap project-switch-to-buffer] . consult-project-buffer)  ;; C-x p b
   ("C-x C-r" . consult-recent-file)
   ([remap bookmark-jump] . consult-bookmark)            ;; C-x r b
   ;; Other custom bindings
   ([remap yank-pop] . consult-yank-pop)  ;; M-y
   ;; M-g bindings (goto-map)
   ("M-g f" . consult-flymake)
   ([remap goto-line] . consult-goto-line) ;; M-g g
   ("M-g o" . consult-outline)
   ("M-g m" . consult-mark)
   ("M-g k" . consult-global-mark)
   ("M-g i" . consult-imenu)
   ("M-g I" . consult-imenu-multi)
   ;; M-s bindings (search-map)
   ("M-s d" . consult-fd)
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
  :hook
  (completion-list-mode . consult-preview-at-point-mode)
  :custom
  ;; Tweak the register preview for `consult-register-load',
  ;; `consult-register-store' and the built-in commands.  This improves the
  ;; register formatting, adds thin separator lines, register sorting and hides
  ;; the window mode line.
  (advice-add #'register-preview :override #'consult-register-window)
  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (consult-narrow-key "<") ;; "C-+"
  (consult-preview-key "M-.")
  ;; Use Consult to select xref locations with preview
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  (register-preview-delay 0.5)
  )

(use-package consult-dir
  :bind
  (("C-x C-d" . consult-dir)
   :map minibuffer-local-completion-map
   ("C-x C-d" . consult-dir)
   ("C-x C-j" . consult-dir-jump-file))
  )

(use-package consult-todo
  :demand t
  :bind
  (("M-s t" . consult-todo)
   ("M-s T" . consult-todo-all))
  )

(use-package marginalia
  :hook
  (after-init . marginalia-mode)
  :bind
  ("M-A" . marginalia-cycle)
  )

(use-package embark
  :bind
  (("C-h B" . embark-bindings) ;; alternative for `describe-bindings'
   ("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("M-n" . embark-next-symbol)
   ("M-p" . embark-previous-symbol))
  :custom
  ;; Optionally replace the key help with a completing-read interface
  (prefix-help-command #'embark-prefix-help-command)
  ;; Show the Embark target at point via Eldoc.  You may adjust the Eldoc
  ;; strategy, if you want to see the documentation from multiple providers.
  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  (embark-cycle-key ".")
  (embark-help-key "?")
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
				 nil
				 (window-parameters (mode-line-format . none))))
  )

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode)
  )

(use-package nerd-icons-completion
  :hook
  (emacs-startup . nerd-icons-completion-mode)
  )

(provide 'init-completion)

;;; init-completion.el ends here
