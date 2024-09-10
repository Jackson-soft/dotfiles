;;; init-script --- script  -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;
;; script configurations.
;;
;;; Code:

(use-package flymake-sqlfluff
  :hook (sql-mode . flymake-sqlfluff-load)
  )

(use-package yaml-mode)

(use-package yaml-pro
  :hook (yaml-mode . yaml-pro-mode)
  )

(use-package protobuf-ts-mode)

(use-package nginx-mode
  :mode ("/nginx/sites-\\(?:available\\|enabled\\)/" . nginx-mode)
  )

(use-package flymake-hadolint
  :hook (dockerfile-ts-mode . flymake-hadolint-setup)
  )

(use-package systemd)

(provide 'init-script)

;;; init-script.el ends here
