;;; init-ui.el --- UI  -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;
;; Visual (UI) configurations.
;;
;;; Code:

(use-package all-the-icons)

(use-package modus-themes
  :init
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t
        modus-themes-region '(bg-only no-extend))

  ;; Load the theme files before enabling a theme
  (modus-themes-load-themes)
  :config
  ;; Load the theme of your choice:
  (modus-themes-load-vivendi)
  ;; (modus-themes-load-operandi)
  :bind ("<f5>" . modus-themes-toggle)
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
                (:eval (propertize " %Z%1*%1+ " 'face 'font-lock-preprocessor-face))
                ;; the buffer name; the file name as a tool tip
                (:eval (propertize " %12b " 'face 'font-lock-keyword-face))

                ;; line and column
                (:eval (propertize " (%l,%c) " 'face 'font-lock-type-face))

                ;; relative position, size of file
                (:eval (propertize " [%p/%I] " 'face 'font-lock-constant-face))

                ;; spaces to align right
                (:eval (propertize
                        " " 'display
                        `((space :align-to (- (+ right right-fringe right-margin)
                                              ,65)))))

                ;; data and time
                mode-line-misc-info

                ;; buffer encode
                (:eval (propertize (format " %s " buffer-file-coding-system) 'face 'font-lock-comment-face))

                ;; the current major mode
                ;; (:eval (propertize " %m " 'face 'font-lock-string-face))
                mode-name

                flycheck-mode-line

                ;; git info
                (vc-mode vc-mode)

                mode-line-end-spaces
                ))

(provide 'init-ui)

;;; init-ui.el ends here
