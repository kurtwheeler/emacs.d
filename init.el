(require 'package)

(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar namooh/required-packages '(better-defaults))

(dolist (p namooh/required-packages)
  (when (not (package-installed-p 'better-defaults))
    (package-install 'better-defaults)))

;; My changes:

(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (if (use-region-p)
    (let ((region-line-start (save-excursion
                               (goto-char (region-beginning))
                               (line-beginning-position)))
          (region-line-end (save-excursion
                             (goto-char (region-end))
                             (line-end-position))))
      (comment-or-uncomment-region region-line-start region-line-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))))


(global-set-key (kbd "C-'") 'toggle-comment-on-line)

(global-set-key (kbd "RET") 'newline-and-indent)

(global-set-key (kbd "M-p") 'ace-window)
(global-set-key (kbd "C-o") 'avy-goto-word-or-subword-1)

(global-unset-key (kbd "C-z"))

(global-linum-mode 1)


;; Overwrite go to beginning of line:
(defun better-beginning-of-line ()
  "if at beginning of line, skip indentation."
  (interactive)
  (if (= 0 (current-column))
    (back-to-indentation)
    (beginning-of-line)))

(global-set-key (kbd "C-a") 'better-beginning-of-line)

(require 'multiple-cursors)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-beginnings-of-lines)
(global-set-key (kbd "<M-f3>") 'mc/mark-all-like-this)

(setq-default show-trailing-whitespace 0)

(setq column-number-mode t)
(setq cider-repl-pop-to-buffer-on-connect nil)

;; Modifications made by Custom

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hippie-expand-try-functions-list (quote (try-complete-file-name-partially try-complete-file-name try-expand-all-abbrevs try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-complete-lisp-symbol-partially try-complete-lisp-symbol try-expand-line try-expand-list))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
