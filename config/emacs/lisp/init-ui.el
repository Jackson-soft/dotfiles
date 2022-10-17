;;; init-ui.el --- UI  -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;
;; Visual (UI) configurations.
;;
;;; Code:

(use-package all-the-icons
  :if (display-graphic-p)
  )

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
