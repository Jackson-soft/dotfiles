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

(use-package yaml-pro
  :hook (yaml-ts-mode . yaml-pro-ts-mode)
  )

(use-package protobuf-ts-mode)

(use-package nginx-mode
  :mode ("/nginx/sites-\\(?:available\\|enabled\\)/" . nginx-mode)
  )

(use-package flymake-hadolint
  :hook (dockerfile-ts-mode . flymake-hadolint-setup)
  )

;; restclient
(use-package restclient
  :mode (("\\.http\\'" . restclient-mode)
		 ("\\.rest\\'" . restclient-mode))
  )

(use-package restclient-test
  :diminish
  :hook ((restclient-mode . restclient-test-mode)
		 (restclient-test-mode . flymake-mode))
  )

(use-package systemd)

(provide 'init-script)

;;; init-script.el ends here
