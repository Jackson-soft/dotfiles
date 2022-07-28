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

(use-package yaml-mode)

(use-package yaml-pro
  :hook (yaml-mode . yaml-pro-mode)
  )

(use-package protobuf-mode
  :config
  (flycheck-define-checker protobuf-buf
    "A protobuf syntax checker using buf.
See URL `https://github.com/bufbuild/buf'."
    :command ("buf" "lint" "--path" source-original)
    :error-patterns
    ((warning line-start (file-name) ":" line ":" column ":" (message) line-end))
    :modes protobuf-mode
    :predicate flycheck-buffer-saved-p)

  (add-to-list 'flycheck-checkers 'protobuf-buf)
  )

(use-package nginx-mode
  :mode ("/nginx/sites-\\(?:available\\|enabled\\)/" . nginx-mode)
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

(use-package systemd)

(provide 'init-script)

;;; init-script.el ends here
