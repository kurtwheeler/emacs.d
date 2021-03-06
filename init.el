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

(defvar myPackages
  '(elpy
    py-autopep8
    ein))

(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      myPackages)

;; My changes:

(setq initial-major-mode 'fundamental-mode)
(setq initial-scratch-message nil)

(add-to-list 'load-path "~/.emacs.d/deps")
(require 'column-marker)
(column-marker-3 80)


(defun whack-whitespace (arg)
      "Delete all white space from point to the next word.  With prefix ARG
    delete across newlines as well.  The only danger in this is that you
    don't have to actually be at the end of a word to make it work.  It
    skips over to the next whitespace and then whacks it all to the next
    word."
      (interactive "P")
      (let ((regexp (if arg "[ \t\n]+" "[ \t]+")))
        (re-search-forward regexp nil t)
        (replace-match "" nil nil)))

(global-set-key (kbd "C-c C-w") 'whack-whitespace)

(setq next-screen-context-lines 10)

(setq auto-mode-alist (cons '("\\.json\\.key$" . javascript-mode) auto-mode-alist))

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(require 'paredit)
(add-hook 'clojure-mode-hook 'enable-paredit-mode)

(defun add-midje-forms-to-clojure-dedenting ()
  (put-clojure-indent 'fact-group 1)
  (put-clojure-indent 'facts 1)
  (put-clojure-indent 'fact 1)
  (put-clojure-indent 'tabular nil)
  (put-clojure-indent 'for-all 1))

(add-hook 'clojure-mode-hook 'add-midje-forms-to-clojure-dedenting)

(defun timvisher/add-compojure-forms-to-clojure-dedenting ()
  (put-clojure-indent 'context 2)
  (put-clojure-indent 'ANY 2)
  (put-clojure-indent 'PUT 2)
  (put-clojure-indent 'GET 2)
  (put-clojure-indent 'POST 2)
  (put-clojure-indent 'DELETE 2)
  (put-clojure-indent 'PATCH 2))

(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)

(defun unbind-movement ()
  (global-set-key (kbd "M-<right>") 'right-word)
  (global-set-key (kbd "M-<left>") 'left-word)
  (global-set-key (kbd "C-<right>") 'right-word)
  (global-set-key (kbd "C-<left>") 'left-word))

(defun delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument ARG, do this that many times. Based on simple.el's
kill-word, but I changed it to use delete-region instead."
  (interactive "p")
  (delete-region (point) (progn (subword-forward arg) (point))))

(defun backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument ARG, do this that many times. Based on simple.el's
backward-kill-word, but I changed it to use delete-region instead."
  (interactive "p")
  (delete-word (- arg)))

(global-set-key (kbd "C-<DEL>") 'backward-delete-word)
(global-set-key (kbd "M-<DEL>") 'backward-delete-word)
(global-set-key (kbd "M-d") 'delete-word)

(add-hook 'paredit-mode-hook 'unbind-movement)

;; Python

(elpy-enable)

(require 'python-black)
(setq python-black-extra-args '("--line-length=100"))
(add-hook 'elpy-mode-hook 'python-black-on-save-mode)
(require 'py-isort)
(add-hook 'before-save-hook 'py-isort-before-save)

;; These may not work once I run more of elpy's test commands
(setq elpy-test-django-with-manage t)
(setq elpy-test-django-runner-manage-command '("run_tests.sh"))

(elpy-set-test-runner 'elpy-test-django-runner)

;; Make compilation buffer scroll until the first error.
(setq compilation-scroll-output 'first-error)

;; Make compilation buffer appear in existing window.
;; (push '("\\*compilation\\*" . (nil (reusable-frames . t))) display-buffer-alist)
;; (push '("\\*compilation\\*" . (nil (reusable-frames . t))) display-buffer-alist)
;; display-buffer-use-some-frame
(defun compilation-hook-window () (set-window-buffer (nth 1 (window-list)) "*compilation*"))
(add-hook 'compilation-mode-hook 'compilation-hook-window)

(setq python-shell-interpreter "/home/kurt/Development/data-refinery/run_shell.sh")
(setq elpy-rpc-python-command "python3.6")

;; Setting this to nil causes indentation to happen line-by-line
(add-hook 'elpy-mode-hook
          (lambda () (setq indent-region-function nil)))

;; C
(setq-default c-basic-offset 4)
(c-set-offset 'case-label '+)

(defun open-bracket ()
  (interactive)
  (insert "{")
  (newline)
  (newline)
  (insert "}")
  (c-indent-line-or-region)
  (previous-line)
  (c-indent-line-or-region))

(define-minor-mode my-c-mode
  "Get your foos in the right places."
  :lighter " My-C"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "{") 'open-bracket)
            map))

(add-hook 'c-mode-hook 'my-c-mode)


;; PHP
(defun php-tabs ()
  (setq indent-tabs-mode t)
  (setq tab-width 4))

(add-hook 'php-mode-hook 'php-tabs)

;; R
(add-hook 'ess-mode-hook
          (lambda () (setq ess-fancy-comments nil)))
(setq ess-default-style 'DEFAULT)


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

(require 'paredit)
(defun paredit-forward-slurp-sexp-comment (&optional argument)
  "Add the S-expression following the current list into that list
  by moving the closing delimiter.
Automatically reindent the newly slurped S-expression with respect to
  its new enclosing form.
If in a string, move the opening double-quote forward by one
  S-expression and escape any intervening characters as necessary,
  without altering any indentation or formatting.
Modified from the original function to not disable functionality
  in commments."
  (interactive "P")
  (paredit-in-comment-p)
  (save-excursion
    (cond ((numberp argument)
           (if (< argument 0)
               (paredit-forward-barf-sexp (- 0 argument))
               (while (< 0 argument)
                 (paredit-forward-slurp-sexp)
                 (setq argument (- argument 1)))))
          ((paredit-in-string-p)
           ;; If there is anything to slurp into the string, take that.
           ;; Otherwise, try to slurp into the enclosing list.
           (if (save-excursion
                 (goto-char (paredit-enclosing-string-end))
                 (paredit-handle-sexp-errors (progn (forward-sexp) nil)
                   t))
               (progn
                 (goto-char (paredit-enclosing-string-end))
                 (paredit-forward-slurp-into-list argument))
               (paredit-forward-slurp-into-string argument)))
          (t
           (paredit-forward-slurp-into-list argument)))))


(defadvice hippie-expand (around hippie-expand-case-fold)
  "Try to do case-sensitive matching (not effective with all functions)."
  (let ((case-fold-search nil))
    ad-do-it))
(ad-activate 'hippie-expand)

(global-set-key (kbd "C-;") 'toggle-comment-on-line)

(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-h") 'ace-window)
(global-set-key (kbd "C-c b") 'join-line)
(global-set-key (kbd "C-o") 'avy-goto-word-or-subword-1)
(global-set-key (kbd "C-<insert>") 'yank)
(global-set-key (kbd "C-<tab>") 'hippie-expand)
(global-set-key (kbd "M-T") 'transpose-sexps)
(global-set-key (kbd "C-S-w") 'delete-region)
(global-set-key (kbd "C-)") 'paredit-forward-slurp-sexp-comment)

(global-unset-key (kbd "ESC ESC ESC"))
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-t"))
(global-unset-key (kbd "C-_"))
(global-set-key (kbd "<insert>") 'ignore)

(global-linum-mode 1)

(global-set-key (kbd "C-x f") 'fiplr-find-file)
(setq fiplr-ignored-globs
      '((directories
         (".git" ".svn" ".hg" ".bzr" "dr_env"))
        (files
         (".#*" "*~" "*.so" "*.jpg" "*.png" "*.gif" "*.pdf" "*.gz" "*.zip"))))

(require 'anzu)
(global-anzu-mode 1)

(add-hook 'cider-repl-mode-hook
          (local-set-key (kbd "M-p") 'ace-window))

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
;; I don't like this in python. I may want it in other modes though...
;; (add-hook 'prog-mode-hook #'electric-pair-mode)


(define-globalized-minor-mode my-global-subword-mode subword-mode
  (lambda () (subword-mode 1)))

(my-global-subword-mode 1)

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
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq column-number-mode t)
(setq cider-repl-pop-to-buffer-on-connect nil)
(setq cider-prompt-save-file-on-load nil)

;; Modifications made by Custom

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-enabled-themes (quote (deeper-blue)))
 '(electric-pair-text-pairs nil)
 '(elpy-company-post-completion-function (quote ignore))
 '(elpy-modules
   (quote
    (elpy-module-eldoc elpy-module-flymake elpy-module-pyvenv elpy-module-highlight-indentation elpy-module-django elpy-module-sane-defaults)))
 '(hippie-expand-try-functions-list
   (quote
    (try-complete-file-name-partially try-complete-file-name try-expand-all-abbrevs try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-complete-lisp-symbol-partially try-complete-lisp-symbol try-expand-line try-expand-list)))
 '(package-selected-packages
   (quote
    (python-black py-isort yaml-mode terraform-mode rainbow-delimiters py-autopep8 paredit multiple-cursors markdown-mode flycheck fiplr ess elpy ein dockerfile-mode clojure-mode better-defaults anzu ace-window)))
 '(python-fill-docstring-style (quote pep-257-nn))
 '(tab-stop-list (quote (4 8 12)))
 '(terraform-indent-level 2)
 '(yaml-indent-offset 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#181a26" :foreground "gray80" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "DAMA" :family "Ubuntu Mono"))))
 '(trailing-whitespace ((t (:background "midnight blue")))))

;; Customizing rainbow-delimiters:

(require 'rainbow-delimiters)
(require 'cl-lib)
(require 'color)
(cl-loop
 for index from 1 to rainbow-delimiters-max-face-count
 do
 (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
   (cl-callf color-saturate-name (face-foreground face) 30)))
(put 'upcase-region 'disabled nil)

;; Limit things to two windows:
;; (defun count-visible-buffers (&optional frame)
;;   "Count how many buffers are currently being shown. Defaults to selected frame."
;;   (length (mapcar #'window-buffer (window-list frame))))

;; (defun do-not-split-more-than-two-windows (window &optional horizontal)
;;   (if (and horizontal (> (count-visible-buffers) 1))
;;       nil
;;     t))

;; (advice-add 'window-splittable-p :before-while #'do-not-split-more-than-two-windows)

;; Prevent obnoxious autosave files littering everywhere!
(setq auto-save-default nil)
