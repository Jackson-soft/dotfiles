;;; init-org.el --- Org ---  -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;
;; Org configurations.
;;
;;; Code:

(use-package text-mode
  :ensure nil
  :hook (text-mode . auto-fill-mode)
  :config
  (setq word-wrap-by-category t)
  )

(use-package org
  :ensure nil
  :custom
  (org-fontify-quote-and-verse-blocks t)
  (org-link-frame-setup '((file . find-file))) ;; 同一个窗口下打开org文件, 默认是在另一个窗口打
  (org-return-follows-link t)
  :config
  (setq org-modules '(org-tempo
                      org-id
                      org-toc
                      org-habit
                      ol-eww
                      ol-info)
        org-pretty-entities t
        org-startup-indented t  ;; 开启折行
        org-startup-with-inline-images t
        org-log-into-drawer t
        org-image-actual-width nil
        org-support-shift-select 'always
        org-log-done 'time
        org-catch-invisible-edits 'smart
        org-hide-macro-markers t
        org-hide-emphasis-markers t
        org-highlight-latex-and-related '(native script entities)
        org-export-backends '(ascii html md icalendar odt)
        org-yank-adjusted-subtrees t
        ;; prettify
        org-loop-over-headlines-in-active-region t
        org-fontify-todo-headline t
        org-fontify-done-headline t
        org-fontify-whole-heading-line t
        org-adapt-indentation nil
        org-special-ctrl-a/e t
        org-special-ctrl-k t
        )

  ;; 快速插入截图到文件
  (defun org-insert-image ()
    "insert a image from clipboard"
    (interactive)
    (let* ((path (concat default-directory "./"))
           (fname (read-string "Enter file name: "))
           (image-file (concat path fname)))
      (if (not (file-exists-p path))
          (mkdir path))
      (do-applescript (concat
                       "set the_path to \"" image-file "\" \n"
                       "set png_data to the clipboard as «class PNGf» \n"
                       "set the_file to open for access (POSIX file the_path as string) with write permission \n"
                       "write png_data to the_file \n"
                       "close access the_file"))
      ;; (shell-command (concat "pngpaste " image-file))
      (org-insert-link nil
                       (concat "file:" image-file)
                       "")
      (message image-file))
    (org-display-inline-images)
    )

  (use-package org-id
    :ensure nil
    :after org
    :config
    (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
    )

  (use-package org-table
    :ensure nil
    :after org
    :config
    (setq org-table-header-line-p t)
    )

  ;; Write codes in org-mode
  (use-package org-src
    :ensure nil
    :hook (org-babel-after-execute . org-redisplay-inline-images)
    :after org
    :config
    (setq org-src-preserve-indentation t
          org-src-window-setup 'other-window)
    )

  ;; export
  (use-package ox
    :ensure nil
    :after org
    :config
    (setq org-export-with-tags 'not-in-toc
          org-export-with-author nil
          org-export-with-toc nil
          org-export-with-priority t
          org-export-with-smart-quotes t
          org-export-headline-levels 5
          org-export-coding-system 'utf-8
          org-export-with-broken-links 'mark)
    )

  (use-package ox-html
    :ensure nil
    :after org
    :config
    (setq org-html-doctype "html5"
          org-html-html5-fancy t
          org-html-checkbox-type 'unicode)
    )

  (use-package ob-restclient
    :after ob)

  (org-babel-do-load-languages
   'org-babel-load-languages '((shell . t)
                               (dot . t)
                               (restclient . t)
                               (emacs-lisp . t)))
  )

(use-package toc-org
  :hook (org-mode . toc-org-mode)
  )

;; 自动显示隐藏光标所在位置的修饰符号
(use-package org-appear
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autolinks t)
  )

;; Drag and drop images to org-mode
(use-package org-download
  :hook ((org-mode dired-mode) . org-download-enable)
  :commands org-download-clipboard
  )

;; "prettier" bullets
(use-package org-superstar
  :hook (org-mode . org-superstar-mode)
  :config
  (setq org-superstar-special-todo-items t
        org-superstar-leading-bullet ?\s)
  )

;; 表格对齐
(use-package valign
  :hook ((org-mode markdown-mode) . valign-mode)
  )

(use-package org-roam
  :hook (org-mode . org-roam-db-autosync-mode)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :init
  (setq org-roam-v2-ack t)
  :config
  (setq org-roam-directory (file-truename "~/myDoc/myBlog")
        org-roam-completion-everywhere t)
  )

;; dot
(use-package graphviz-dot-mode
  :config
  (graphviz-turn-on-live-preview)

  (use-package company-graphviz-dot
    :ensure nil)
  )

(provide 'init-org)

;;; init-org.el ends here
