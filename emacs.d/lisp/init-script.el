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

(use-package yaml-mode
  :mode (("\\.yaml\\'" . yaml-mode)
         ("\\.yml\\'" . yaml-mode))
  )

(use-package protobuf-mode)

(use-package nginx-mode
  :mode (("nginx\\.conf\\'" . nginx-mode)
         ("/nginx/.+\\.conf\\'" . nginx-mode))
  )

(use-package company-nginx
  :hook(nginx-mode . company-nginx-keywords)
  )

(use-package dockerfile-mode
  :mode ("Dockerfile\\'" . dockerfile-mode)
  )

(use-package docker-compose-mode)

;; restclient
(use-package restclient
  :mode (("\\.http\\'" . restclient-mode)
         ("\\.rest\\'" . restclient-mode))
  )

(use-package company-restclient
  :defines company-backends
  :hook (restclient-mode . (lambda ()
                             (cl-pushnew 'company-restclient company-backends)))
  )

(use-package systemd
  :mode ("\\.service\\'" . systemd-mode)
  )

(provide 'init-script)

;;; init-script.el ends here
