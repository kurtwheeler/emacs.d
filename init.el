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

(setq next-screen-context-lines 10)

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

(add-hook 'paredit-mode-hook 'unbind-movement)

;; Python

(elpy-enable)

(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

(elpy-use-ipython)
(setq python-shell-interpreter "ipython2" python-shell-interpreter-args "--simple-prompt --pprint")

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


(global-set-key (kbd "C-;") 'toggle-comment-on-line)

(global-set-key (kbd "RET") 'newline-and-indent)

(global-set-key (kbd "C-h") 'ace-window)
(global-set-key (kbd "C-c b") 'join-line)
(global-set-key (kbd "C-o") 'avy-goto-word-or-subword-1)
(global-set-key (kbd "C-<insert>") 'yank)
(global-set-key (kbd "C-<tab>") 'hippie-expand)
(global-set-key (kbd "M-T") 'transpose-sexps)

(global-unset-key (kbd "ESC ESC ESC"))
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-_"))

(global-linum-mode 1)

(global-set-key (kbd "C-x f") 'fiplr-find-file)

(require 'anzu)
(global-anzu-mode 1)

(add-hook 'cider-repl-mode-hook
          (local-set-key (kbd "M-p") 'ace-window))

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

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

;; Doesn't seem to work, this sorcery is obnoxious.
;; (defun reset-cider-output-streams ()
;;   (cider-nrepl-request:eval
;;    "(do
;;       (require '[cider.nrepl.middleware.out])
;;       (alter-var-root #'*out* (constantly cider.nrepl.middleware.out/original-out)))"
;;    (lambda (_response) nil)))

;; (add-hook 'cider-mode-hook 'reset-cider-output-streams)

;; Modifications made by Custom

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-enabled-themes (quote (deeper-blue)))
 '(hippie-expand-try-functions-list (quote (try-complete-file-name-partially try-complete-file-name try-expand-all-abbrevs try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-complete-lisp-symbol-partially try-complete-lisp-symbol try-expand-line try-expand-list))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Customizing rainbow-delimiters:

(require 'rainbow-delimiters)
(require 'cl-lib)
(require 'color)
(cl-loop
 for index from 1 to rainbow-delimiters-max-face-count
 do
 (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
   (cl-callf color-saturate-name (face-foreground face) 30)))
