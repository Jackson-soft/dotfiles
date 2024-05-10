;;; init-ui.el --- UI  -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;
;; Visual (UI) configurations.
;;
;;; Code:

(use-package nerd-icons
  :demand t)

(use-package modus-themes
  :bind ("<f5>" . modus-themes-toggle)
  :init (load-theme 'modus-operandi-tinted :no-confim)
  )

;; 显示时间
(use-package time
  :ensure nil
  :hook (after-init . display-time-mode)
  :config
  (setq display-time-format "%Y-%m-%d %H:%M"
        display-time-24hr-format t
		display-time-default-load-average nil)
  )

;; Mode line settings
(setq-default mode-line-format
			  '("%e"
				mode-line-front-space
				(:propertize ("" mode-line-mule-info mode-line-modified))
                mode-line-frame-identification
                mode-line-buffer-identification
                (:propertize " %l:%c " 'face 'modus-themes-bold)
                (:propertize " %I " 'face 'modus-themes-bold)
				"  "
				mode-line-modes

				;; buffer encode
				(:eval (propertize (format " %s " buffer-file-coding-system) 'face 'font-lock-comment-face))

				;; git info
				(vc-mode vc-mode)
				"   "
				;; data and time
				mode-line-misc-info

				mode-line-end-spaces
				))

(provide 'init-ui)

;;; init-ui.el ends here
