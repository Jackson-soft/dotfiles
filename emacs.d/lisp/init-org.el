;;; init-org.el --- Org ---  -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;
;; Org configurations.
;;
;;; Code:

(use-package org
  :ensure nil
  :hook ((org-mode . org-indent-mode)
         (org-mode . prettify-symbols-mode))
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture))
  :custom
  (org-log-done 'time)
  (org-catch-invisible-edits 'smart)
  (org-hide-macro-markers t)
  (org-hide-emphasis-markers t)
  (org-highlight-latex-and-related '(native script entities))
  (org-pretty-entities t)
  (org-startup-indented t)  ;; 开启折行
  (org-startup-with-inline-images t)
  (org-tags-column 120)
  (org-log-into-drawer t)
  (org-image-actual-width nil)
  (org-support-shift-select 'always)
  (org-html-htmlize-output-type 'css)
  ;; prettify
  (org-loop-over-headlines-in-active-region t)
  (org-fontify-todo-headline t)
  (org-fontify-whole-heading-line t)
  (org-fontify-done-headline t)
  (org-fontify-quote-and-verse-blocks t)
  (org-adapt-indentation nil)
  (org-special-ctrl-a/e t)
  (org-special-ctrl-k t)
  (org-yank-adjusted-subtrees t)
  (org-enable-table-editor 1) ;; 启用内建电子表格
  (org-link-frame-setup '((file . find-file))) ;; 同一个窗口下打开org文件, 默认是在另一个窗口打
  (org-return-follows-link t)
  :config
  (add-to-list 'org-modules 'org-tempo t)

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
    :custom
    (org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id))

  (use-package org-goto
    :ensure nil
    :after org
    :custom
    (org-goto-auto-isearch nil)
    (org-goto-interface 'outline-path-completion))

  (use-package org-table
    :ensure nil
    :after org
    :custom
    (org-table-header-line-p t)
    (org-table-export-default-format "orgtbl-to-csv")
    (org-table-formula-constants '(("PI" . "3.14159265358979323846264"))))

  ;; Write codes in org-mode
  (use-package org-src
    :ensure nil
    :after org
    :hook (org-babel-after-execute . org-redisplay-inline-images)
    :bind (:map org-src-mode-map
                ;; consistent with separedit/magit
                ("C-c C-c" . org-edit-src-exit))
    :custom
    (org-src-fontify-natively t)
    (org-src-tab-acts-natively t)
    (org-src-preserve-indentation t)
    (org-src-window-setup 'current-window)
    (org-confirm-babel-evaluate nil)
    (org-edit-src-content-indentation 0)
    (org-babel-load-languages '((shell . t)
                                (dot . t)
                                (emacs-lisp . t))))

  ;; C-c '的时候启用当前mode
  (add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))

  ;; export
  (use-package ox
    :ensure nil
    :after org
    :custom
    (org-export-with-toc nil)
    (org-export-with-title t)
    (org-export-with-tags 'not-in-toc)
    (org-export-with-email nil)
    (org-export-with-author nil)
    (org-export-with-drawers nil)
    (org-export-with-priority t)
    (org-export-with-footnotes t)
    (org-export-with-smart-quotes t)
    (org-export-with-section-numbers nil)
    (org-export-with-sub-superscripts '{})
    ;; Use :eval never-export header argument to avoid evaluating.
    (org-export-use-babel t)
    (org-export-headline-levels 5)
    (org-export-coding-system 'utf-8)
    (org-export-with-broken-links 'mark)
    (org-export-backends '(ascii html md icalendar man))
    )

  (use-package ox-html
    :ensure nil
    :after org
    :config
    (setq org-html-doctype "html5"
          org-html-html5-fancy t
          org-html-checkbox-type 'unicode
          org-html-validation-link nil)
    )

  ;; copy link
  (use-package org-cliplink
    :commands org-cliplink
    )
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
  ;; Make leading stars truly invisible, by rendering them as spaces!
  (setq org-superstar-leading-bullet ?\s
        org-superstar-leading-fallback ?\s
        org-superstar-special-todo-items t)
  )

;; 表格对齐
(use-package valign
  :hook ((org-mode markdown-mode) . valign-mode)
  )

(use-package org-roam
  :hook (org-mode . org-roam-mode)
  :bind (:map org-roam-mode-map
              ("C-c n l" . org-roam)
              ("C-c n f" . org-roam-find-file)
              ("C-c n g" . org-roam-graph)
              :map org-mode-map
              ("C-c n i" . org-roam-insert)
              ("C-c n I" . org-roam-insert-immediate))
  :config
  (setq org-roam-directory "~/myDoc/myBlog"
        org-roam-buffer-window-parameters '((no-delete-other-windows . t))
        org-roam-completion-everywhere t
        org-roam-db-update-method 'immediate)
  )

;; dot
(use-package graphviz-dot-mode
  :mode (("\\.diag\\'"      . graphviz-dot-mode)
         ("\\.blockdiag\\'" . graphviz-dot-mode)
         ("\\.nwdiag\\'"    . graphviz-dot-mode)
         ("\\.rackdiag\\'"  . graphviz-dot-mode)
         ("\\.dot\\'"       . graphviz-dot-mode)
         ("\\.gv\\'"        . graphviz-dot-mode))
  :config
  (graphviz-turn-on-live-preview)

  (setq graphviz-dot-indent-width tab-width)

  (use-package company-graphviz-dot
    :ensure nil)
  )

(provide 'init-org)

;;; init-org.el ends here
