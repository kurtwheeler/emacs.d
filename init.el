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
