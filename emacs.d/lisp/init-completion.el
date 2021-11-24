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

;; Use the `orderless' completion style.
(use-package orderless
  :custom
  (completion-styles '(basic substring initials flex partial-completion orderless))
  )

(use-package consult
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ;; C-x bindings (ctl-x-map)
         ([remap switch-to-buffer] . consult-buffer) ;; C-x b
         ;; M-g bindings (goto-map)
         ([remap goto-line] . consult-goto-line) ;; M-g g
         ("M-g o" . consult-outline)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; Other custom bindings
         ([remap yank-pop] . consult-yank-pop)  ;; M-y
         ;; M-s bindings (search-map)
         ("M-s f" . consult-find)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines))
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
  ;; Optionally replace `completing-read-multiple' with an enhanced version.
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)
  :config
  ;; (setq consult-project-root-function (lambda () (project-root (project-current t))))
  (setq consult-project-root-function #'vc-root-dir)

  ;; use fd instead of find
  (setq consult-preview-key (kbd "M-p")
        consult-narrow-key (kbd "C-+"))
  )

(use-package marginalia
  :hook (after-init . marginalia-mode)
  )

(use-package embark
  :bind (("C-h B" . embark-bindings) ;; alternative for `describe-bindings'
         :map minibuffer-local-map
         ("M-o"     . embark-act)
         ("C-c C-o" . embark-export)
         ("C-c C-c" . embark-collect-snapshot))
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  )

(use-package embark-consult
  :after (embark consult)
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook (embark-collect-mode . consult-preview-at-point-mode)
  )

(use-package all-the-icons-completion
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  )

(provide 'init-completion)

;;; init-completion.el ends here
