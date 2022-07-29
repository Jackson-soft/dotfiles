;;; init-web.el --- web  -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;
;; Web configurations.
;;
;;; Code:

;; typescript
(use-package typescript-mode)

(use-package tide
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode))
  )

;; json
(use-package jsonian)

(use-package web-mode
  :mode ("\\.html?\\'" . web-mode)
  :config
  (setq web-mode-enable-auto-pairing t
        web-mode-enable-css-colorization t
        web-mode-enable-part-face t
        web-mode-enable-comment-interpolation t
        web-mode-enable-current-column-highlight t
        web-mode-enable-current-element-highlight t)
  )

(provide 'init-web)

;;; init-web.el ends here
