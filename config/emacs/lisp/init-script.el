;;; init-script --- script  -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;
;; script configurations.
;;
;;; Code:

(use-package lua-mode
  :config
  (setq lua-indent-level tab-width
		lua-indent-string-contents t)
  )

(use-package flymake-sqlfluff
  :hook (sql-mode . flymake-sqlfluff-load)
  )

(use-package yaml-mode)

(use-package yaml-pro
  :hook (yaml-mode . yaml-pro-mode)
  )

(use-package protobuf-mode)

(use-package nginx-mode
  :mode ("/nginx/sites-\\(?:available\\|enabled\\)/" . nginx-mode)
  )

(use-package dockerfile-mode
  :mode ("Dockerfile\\'" . dockerfile-mode)
  )

(use-package flymake-hadolint
  :hook (dockerfile-mode . flymake-hadolint-setup)
  )

(use-package docker-compose-mode)

;; restclient
(use-package restclient
  :mode (("\\.http\\'" . restclient-mode)
		 ("\\.rest\\'" . restclient-mode))
  )

(use-package systemd)

(provide 'init-script)

;;; init-script.el ends here
