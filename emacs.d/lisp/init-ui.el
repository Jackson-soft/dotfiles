;;; init-ui.el --- UI  -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;
;; Visual (UI) configurations.
;;
;;; Code:

(use-package all-the-icons
  :if (display-graphic-p))

(use-package modus-themes
  :bind ("<f5>" . modus-themes-toggle)
  :init
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs t
        modus-themes-deuteranopia t
        modus-themes-bold-constructs t
        modus-themes-syntax '(faint alt-syntax green-strings yellow-comments)
        modus-themes-links '(neutral-underline background)
        modus-themes-box-buttons '(variable-pitch flat faint 0.9)
        modus-themes-prompts '(intense bold)
        modus-themes-mode-line '(moody accented borderless)
        modus-themes-completions '((matches . (extrabold background))
                                   (selection . (semibold intense accented text-also))
                                   (popup . (accented intense)))
        modus-themes-lang-checkers '(text-also background)
        modus-themes-hl-line '(intense)
        modus-themes-subtle-line-numbers t
        modus-themes-markup '(bold italic)
        modus-themes-paren-match '(bold intense)
        modus-themes-region '(bg-only no-extend)
        modus-themes-org-blocks 'tinted-background
        modus-themes-headings '((t . (variable-pitch extrabold)))
        modus-themes-variable-pitch-ui t
        )

  ;; Load the theme files before enabling a theme
  (modus-themes-load-themes)
  :config
  ;; Load the theme of your choice:
  (modus-themes-load-vivendi)
  )

;; (use-package doom-themes
;;   :config
;;   (setq doom-themes-enable-bold t
;;         doom-themes-enable-italic t)
;;   (load-theme 'doom-vibrant t)

;;   (doom-themes-visual-bell-config)
;;   (doom-themes-org-config)
;;   )

;; 显示时间
(use-package time
  :ensure nil
  :hook (after-init . display-time-mode)
  :config
  (setq display-time-format "%Y-%m-%d %H:%M"
        display-time-default-load-average nil)
  )

;; Tabs for window layouts
(use-package tab-bar
  :ensure nil
  :hook (after-init . tab-bar-history-mode)
  :bind (("C-x <right>" . tab-bar-history-forward)
         ("C-x <left>" . tab-bar-history-back))
  )

;; Mode line settings
(setq-default mode-line-format
              '("%e"
                mode-line-front-space
                mode-line-mule-info
                mode-line-modified
                "  "
                ;; the buffer name; the file name as a tool tip
                mode-line-buffer-identification

                ;; line and column
                mode-line-position
                mode-line-modes

                ;; spaces to align right
                (:eval (propertize
                        " " 'display
                        `((space :align-to (- (+ right right-fringe right-margin)
                                              ,65)))))

                ;; data and time
                mode-line-misc-info

                ;; buffer encode
                (:eval (propertize (format " %s " buffer-file-coding-system) 'face 'font-lock-comment-face))

                ;; git info
                (vc-mode vc-mode)

                mode-line-end-spaces
                ))

(provide 'init-ui)

;;; init-ui.el ends here
