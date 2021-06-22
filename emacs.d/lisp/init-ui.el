;;; init-ui.el --- UI  -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;
;; Visual (UI) configurations.
;;
;;; Code:

(use-package all-the-icons)

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-vibrant t)

  (doom-themes-visual-bell-config)
  (doom-themes-org-config)
  )

;; 显示时间
(use-package time
  :ensure nil
  :hook (after-init . display-time-mode)
  :config
  (setq display-time-24hr-format t
        display-time-day-and-date t
        display-time-default-load-average nil)
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
