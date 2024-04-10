;;; go-tag.el --- Edit Golang struct field tag -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Brantou

;; Author: Brantou <brantou89@gmail.com>
;; URL: https://github.com/brantou/emacs-go-tag
;; Keywords: tools
;; Version: 1.1.0
;; Package-Requires: ((emacs "24.0")(go-mode "1.5.0"))

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Edit field tags for golang struct fields, based on gomodifytags.
;; This package is inspired by GoAddTags of vim-go and go-add-tags.
;;

;;; Requirements:
;;
;; - gomodifytags :: https://github.com/fatih/gomodifytags
;;

;;; TODO
;;
;; - Provide better error feedback.
;; - Provide more configuration.
;;

;;; Code:

(defgroup go-tag nil
  "Modify field tag for struct fields."
  :prefix "go-tag-"
  :link '(url-link :tag "MELPA" "https://melpa.org/#/go-tag")
  :link '(url-link :tag "MELPA Stable" "https://stable.melpa.org/#/go-tag")
  :link '(url-link :tag "GitHub" "https://github.com/brantou/emacs-go-tag")
  :group 'go)

(defcustom go-tag-command "gomodifytags"
  "The 'gomodifytags' command.
from https://github.com/fatih/gomodifytags."
  :type 'string
  :group 'go-tag)

(defcustom go-tag-args nil
  "Additional arguments to pass to go-tag."
  :type '(repeat string)
  :group 'go-tag)

(defcustom go-tag-show-errors 'buffer
  "Where to display go-tag error output.
It can either be displayed in its own buffer, in the echo area, or not at all."
  :type '(choice
          (const :tag "Own buffer" buffer)
          (const :tag "Echo area" echo)
          (const :tag "None" nil))
  :group 'go-tag)

(defun go-tag--parse-tag (tags)
  (mapconcat
   'car
   (mapcar
    (lambda(str)
      (split-string str ","))
    (split-string tags))
   ","))

(defun go-tag--parse-option (tags)
  (mapconcat 'identity
             (delete
              ""
              (split-string
               (mapconcat
                (lambda(str-lst)
                  (when (< 1 (length str-lst))
                    (concat (car str-lst) "=" (cadr str-lst))))
                (mapcar
                 (lambda(str)
                   (split-string str ","))
                 (split-string tags))
                ",")
               ","))
             ","))

;;;###autoload
(defun go-tag-open-github()
  "go-tag open Github page."
  (interactive)
  (browse-url "https://github.com/brantou/emacs-go-tag"))

;;;###autoload
(defun go-tag-refresh (tags)
  "Refresh field TAGS for struct fields."
  (interactive "sTags: ")
  (let ((stags (go-tag--parse-tag tags))
        (options (go-tag--parse-option tags)))
    (when (string-equal stags "") (setq stags "json"))
    (if (use-region-p)
        (progn
          (go-tag--region-remove (region-beginning) (region-end) stags "")
          (go-tag--region (region-beginning) (region-end) stags options))
      (progn
        (go-tag--point-remove (position-bytes (point))  stags "")
        (go-tag--point (position-bytes (point))  stags options)))))

;;;###autoload
(defun go-tag-add (tags)
  "Add field TAGS for struct fields."
  (interactive "sTags: ")
  (let ((stags (go-tag--parse-tag tags))
        (options (go-tag--parse-option tags)))
    (if (use-region-p)
        (go-tag--region (region-beginning) (region-end) stags options)
      (go-tag--point (position-bytes (point))  stags options))))

(defun go-tag--region (start end tags &optional options)
  "Add field TAGS for the region between START and END."
  (let ((cmd-args (append
                   go-tag-args
                   (list "-line" (format "%S,%S" (line-number-at-pos start) (line-number-at-pos end))))))
    (go-tag--add cmd-args tags options)))

(defun go-tag--point (point tags &optional options)
  "Add field TAGS for the struct under the POINT."
  (let ((cmd-args (append go-tag-args
                          (list "-offset" (format "%S" point)))))
    (go-tag--add cmd-args tags options)))

(defun go-tag--add (cmd-args tags &optional options)
  "Init CMD-ARGS, add TAGS and OPTIONS to CMD-ARGS."
  (let ((tags (if (string-equal tags "") "json" tags)))
    (setq cmd-args
          (append cmd-args
                  (list "-add-tags" tags)))
    (unless (string-equal options "")
      (setq cmd-args
            (append cmd-args
                    (list "-add-options" options))))
    (go-tag--proc cmd-args)))

;;;###autoload
(defun go-tag-remove (tags)
  "Remove field TAGS for struct fields."
  (interactive "sTags: ")
  (let ((stags (go-tag--parse-tag tags))
        (options (go-tag--parse-option tags)))
    (if (use-region-p)
        (go-tag--region-remove (region-beginning) (region-end) stags options)
      (go-tag--point-remove (position-bytes (point))  stags options))))

(defun go-tag--region-remove (start end tags &optional options)
  "Remove field TAGS for the region between START and END."
  (let ((cmd-args (append
                   go-tag-args
                   (list "-line" (format "%S,%S" (line-number-at-pos start) (line-number-at-pos end))))))
    (go-tag--remove cmd-args tags options)))

(defun go-tag--point-remove (point tags &optional options)
  "Add field TAGS for the struct under the POINT."
  (let ((cmd-args (append
                   go-tag-args
                   (list "-offset" (format "%S" point)))))
    (go-tag--remove cmd-args tags options)))

(defun go-tag--remove(cmd-args tags &optional options)
  "Init CMD-ARGS, add TAGS and OPTIONS to CMD-ARGS."
  (progn
    (if (string-equal tags "")
        (setq cmd-args
              (append cmd-args
                      (list "-clear-tags")))
      (if (string-equal options "")
          (setq cmd-args
                (append cmd-args
                        (list "-remove-tags" tags)))
        (let ((tags (go-tag--filter-tags tags options)))
          (unless (string-equal tags "")
            (setq cmd-args
                  (append cmd-args
                          (list "-remove-tags" tags))))
          (setq cmd-args
                (append cmd-args
                        (list "-remove-options" options))))))
    (go-tag--proc cmd-args)))

(defun go-tag--filter-tags (tags options)
  (let ((tag-lst (split-string tags ","))
        (option-alst (mapcar
                      (lambda (opt)
                        (let ((opt-pair (split-string opt "=")))
                          (cons (car opt-pair) (cadr opt-pair))))
                      (split-string options ","))))
    (mapconcat
     'identity
     (delete "" (split-string
                 (mapconcat
                  (lambda (tag)
                    (unless (assoc tag option-alst) tag))
                  tag-lst
                  ",") ","))
     ",")))

(defun go--goto-line (line)
  (goto-char (point-min))
  (forward-line (1- line)))

(defun go--delete-whole-line (&optional arg)
  "Delete the current line without putting it in the `kill-ring'.
Derived from function `kill-whole-line'.  ARG is defined as for that
function."
  (setq arg (or arg 1))
  (if (and (> arg 0)
           (eobp)
           (save-excursion (forward-visible-line 0) (eobp)))
      (signal 'end-of-buffer nil))
  (if (and (< arg 0)
           (bobp)
           (save-excursion (end-of-visible-line) (bobp)))
      (signal 'beginning-of-buffer nil))
  (cond ((zerop arg)
         (delete-region (progn (forward-visible-line 0) (point))
                        (progn (end-of-visible-line) (point))))
        ((< arg 0)
         (delete-region (progn (end-of-visible-line) (point))
                        (progn (forward-visible-line (1+ arg))
                               (unless (bobp)
                                 (backward-char))
                               (point))))
        (t
         (delete-region (progn (forward-visible-line 0) (point))
                        (progn (forward-visible-line arg) (point))))))

(defun go--apply-rcs-patch (patch-buffer)
  "Apply an RCS-formatted diff from PATCH-BUFFER to the current buffer."
  (let ((target-buffer (current-buffer))
        ;; Relative offset between buffer line numbers and line numbers
        ;; in patch.
        ;;
        ;; Line numbers in the patch are based on the source file, so
        ;; we have to keep an offset when making changes to the
        ;; buffer.
        ;;
        ;; Appending lines decrements the offset (possibly making it
        ;; negative), deleting lines increments it. This order
        ;; simplifies the forward-line invocations.
        (line-offset 0)
        (column (current-column)))
    (save-excursion
      (with-current-buffer patch-buffer
        (goto-char (point-min))
        (while (not (eobp))
          (unless (looking-at "^\\([ad]\\)\\([0-9]+\\) \\([0-9]+\\)")
            (error "Invalid rcs patch or internal error in go--apply-rcs-patch"))
          (forward-line)
          (let ((action (match-string 1))
                (from (string-to-number (match-string 2)))
                (len  (string-to-number (match-string 3))))
            (cond
             ((equal action "a")
              (let ((start (point)))
                (forward-line len)
                (let ((text (buffer-substring start (point))))
                  (with-current-buffer target-buffer
                    (cl-decf line-offset len)
                    (goto-char (point-min))
                    (forward-line (- from len line-offset))
                    (insert text)))))
             ((equal action "d")
              (with-current-buffer target-buffer
                (go--goto-line (- from line-offset))
                (cl-incf line-offset len)
                (go--delete-whole-line len)))
             (t
              (error "Invalid rcs patch or internal error in go--apply-rcs-patch")))))))
    (move-to-column column)))

(defun go-tag--proc (cmd-args)
  "Modify field tags based on CMD-ARGS.

  The tool used can be set via ‘go-tag-command` (default: go-tag)
 and additional arguments can be set as a list via ‘go-tag-args`."
  (let ((tmpfile (make-temp-file "go-tag" nil ".go"))
        (patchbuf (get-buffer-create "*Go-Tag patch*"))
        (errbuf (if go-tag-show-errors
                    (get-buffer-create "*Go-Tag Errors*")))
        (coding-system-for-read 'utf-8)
        (coding-system-for-write 'utf-8))

    (unwind-protect
        (save-restriction
          (widen)
          (if errbuf
              (with-current-buffer errbuf
                (setq buffer-read-only nil)
                (erase-buffer)))
          (with-current-buffer patchbuf
            (erase-buffer))

          (write-region nil nil tmpfile)

          (setq cmd-args (append cmd-args (list "-file" tmpfile "-w")))

          (message "Calling go-tag: %s %s" go-tag-command cmd-args)
          (if (zerop (apply #'call-process go-tag-command nil errbuf nil cmd-args))
              (progn
                (if (zerop (call-process-region (point-min) (point-max) "diff" nil patchbuf nil "-n" "-" tmpfile))
                    (message "Buffer is already go-tag")
                  (go--apply-rcs-patch patchbuf)
                  (message "Applied go-tag"))
                (if errbuf (go-tag--kill-error-buffer errbuf)))
            (message "Could not apply go-tag")
            (if errbuf
                (progn
                  (message (with-current-buffer errbuf (buffer-string)))
                  (go-tag--kill-error-buffer errbuf)))))

      (kill-buffer patchbuf)
      (delete-file tmpfile))))

(defun go-tag--kill-error-buffer (errbuf)
  "Kill ERRBUF."
  (let ((win (get-buffer-window errbuf)))
    (if win
        (quit-window t win)
      (kill-buffer errbuf))))

(provide 'go-tag)

;;; go-tag.el ends here
