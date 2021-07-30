;; init-emacs-lisp.el --- Initialize Emacs Lisp configurations. -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;
;; Emacs Lisp configurations.
;;

;;; Code:

(use-package elisp-mode
  :ensure nil
  :config
  (defun indent-buffer ()
    "Call the 'indent-region' function on the entire content of the current buffer."
    (interactive)
    (when (eq major-mode 'emacs-lisp-mode)
      (indent-region (point-min) (point-max))))

  (add-hook 'emacs-lisp-mode-hook #'(lambda()
                                      (add-hook 'before-save-hook 'indent-buffer)))
  )

;; A better *Help* buffer
(use-package helpful
  :bind (([remap describe-function] . helpful-callable)  ;; C-h f
         ("C-h C" . helpful-command)
         ([remap describe-key] . helpful-key)  ;; C-h k
         ([remap describe-variable] . helpful-variable)  ;; C-h v
         ([remap describe-symbol] . helpful-symbol)
         :map emacs-lisp-mode-map
         ("C-c C-d" . helpful-at-point))
  :hook (helpful-mode . cursor-sensor-mode)
  )

(provide 'init-emacs-lisp)

;;; init-emacs-lisp.el ends here
