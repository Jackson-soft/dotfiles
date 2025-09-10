;;; init-ui.el --- UI  -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;
;; Visual (UI) configurations.
;;
;;; Code:

(use-package nerd-icons
  :demand t
  )

;; Display available keybindings in popup
(use-package which-key
  :ensure nil
  :diminish
  :hook
  (after-init . which-key-mode)
  :custom
  (which-key-show-remaining-keys t)
  )

;; 时间显示
(use-package time
  :ensure nil
  :custom
  (display-time-24hr-format t)       ;; 24 小时制
  (display-time-day-and-date t)      ;; 显示日期和星期
  (display-time-format "%Y-%m-%d %H:%M") ;; 自定义格式
  (display-time-default-load-average nil)
  :init
  (display-time-mode 1))             ;; 启用时间显示

;; Moody - 丝带与标签风格的 mode-line
(use-package moody
  :config
  ;; 替换默认的 mode-line 元素
  (moody-replace-mode-line-front-space)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode)

  ;; 去掉默认边框，让外观更干净
  (set-face-attribute 'mode-line-active nil
					  :box 'unspecified
					  :overline "blue"
					  :underline `(:color "blue" :position t))

  (set-face-attribute 'mode-line-inactive nil
					  :box 'unspecified
					  :overline "green"
					  :underline `(:color "green" :position t))
  )

(provide 'init-ui)

;;; init-ui.el ends here
