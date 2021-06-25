;;; init-completion.el --- Config for completion   -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package icomplete
  :ensure nil
  :hook (after-init . icomplete-mode)
  :bind (:map icomplete-minibuffer-map
              ("RET" . icomplete-fido-ret)
              ("C-n" . icomplete-forward-completions)
              ("C-p" . icomplete-backward-completions)
              ("C-v" . icomplete-vertical-toggle)
              ("DEL" . icomplete-fido-backward-updir))
  :custom
  (icomplete-vertical-mode t)
  :config
  (setq icomplete-compute-delay 0
        icomplete-in-buffer t
        icomplete-scroll t
        icomplete-show-matches-on-no-input t
        icomplete-tidy-shadowed-file-names t)
  )

;; Use the `orderless' completion style.
;; Enable `partial-completion' for files to allow path expansion.
;; You may prefer to use `initials' instead of `partial-completion'.
(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion)))))
  )

(use-package consult
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ;; C-x bindings (ctl-x-map)
         ("C-x C-r" . consult-recent-file)
         ([remap switch-to-buffer] . consult-buffer) ;; C-x b
         ;; M-g bindings (goto-map)
         ([remap goto-line] . consult-goto-line) ;; M-g g
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-project-imenu)
         ("M-g o" . consult-outline)
         ;; Other custom bindings
         ([remap yank-pop] . consult-yank-pop)  ;; M-y
         ;; M-s bindings (search-map)
         ("M-s l" . consult-line)
         ("M-s f" . consult-find)
         ("M-s r" . consult-ripgrep)
         ("M-s e" . consult-isearch))
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)
  :config
  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (project-root project))))

  ;; use fd instead of find
  (setq consult-find-command "fd --color=never --full-path ARG OPTS"
        consult-preview-key (kbd "M-p")
        consult-narrow-key (kbd "C-+"))
  )

(use-package marginalia
  :hook (after-init . marginalia-mode)
  )

(use-package embark
  :bind (("C-S-a" . embark-act)       ;; pick some comfortable binding
         ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  )

(use-package embark-consult
  :after (embark consult)
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook (embark-collect-mode . consult-preview-at-point-mode)
  )

(provide 'init-completion)

;;; init-completion.el ends here
