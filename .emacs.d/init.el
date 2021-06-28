;; ;; -*- lexical-binding: t; -*-
(setq gc-cons-threshold (* 50 1000 1000))
 (menu-bar-mode -1)
 (tool-bar-mode -1)
(setq case-fold-search nil)
;; (global-subword-mode 1);;move through camel case

;; (setq scroll-conservatively 1)
;; ;(define-key dired-mode-map [mouse-2] 'dired-mouse-find-file)

(defalias 'eb 'eval-buffer)
;; (defalias 'er 'eval-region)
(defalias 'dk 'describe-key)

(setq initial-major-mode 'fundamental-mode)
;; (global-visual-line-mode t)

(defalias 'yes-or-no-p 'y-or-n-p)
(setq search-whitespace-regexp "[-_ \t\n]+")
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

(global-set-key (kbd "<f5>") 'repeat-complex-command)
(global-set-key (kbd "<f7>") 'kill-buffer-and-window)

(defun xah-open-file-fast ()
  "Prompt to open a file from bookmark `bookmark-bmenu-list'.
This command is similar to `bookmark-jump', but use `ido-mode' interface, and ignore cursor position in bookmark.
URL `http://ergoemacs.org/emacs/emacs_hotkey_open_file_fast.html'
Version 2019-02-26"
  (interactive)
  (require 'bookmark)
  (bookmark-maybe-load-default-file)
  (let (($this-bookmark
         (ido-completing-read "Open bookmark:" (mapcar (lambda ($x) (car $x)) bookmark-alist))))
    (find-file (bookmark-get-filename $this-bookmark))
    ;; (bookmark-jump $this-bookmark)
    ))

(setq-default select-enable-clipboard t)

(setq backup-directory-alist `(("." . "~/.saves")))
 
;; (show-paren-mode 1)

;; (set-language-environment "UTF-8")
;; (set-default-coding-systems 'utf-8)

;; (setq-default indent-tabs-mode t)
;; (setq tab-width 4)
;; (setq-default tab-wdith 4)
;; (setq c-basic-offset 4)
;; (setq c-default-style
;;       '((java-mode . "java")
;;         (other . "linux")))

;; ;;bracket matching
;; (electric-pair-mode 1)
;; (setq electric-pair-pairs
;;       '(
;;         (?\".?\")
;;         (?\{.?\})))

;; (cua-mode 1)
;; (ido-mode 1)

;;i-search with more intuitive controls
(progn
  (define-key isearch-mode-map (kbd "<up>")`isearch-ring-retreat)
  (define-key isearch-mode-map (kbd "<down>")`isearch-ring-advance)
  (define-key isearch-mode-map (kbd "<left>")`isearch-repeat-backward)
  (define-key isearch-mode-map (kbd "<right>")`isearch-repeat-forwrd)
  (define-key minibuffer-local-isearch-map (kbd"<left>")`isearch-reverse-exit-minibuffer)
  (define-key minibuffer-local-isearch-map (kbd "<right>")`isearch-forward-exit-minibuffer))

;;no backups
(setq make-backp-files nil)
(setq backup-by-copying t)
(setq auto-save-default nil)

;;  ;;package manager
;;  (require `package)
;;  (setq package-enable-ar-startup nil)
;;  (add-to-list `package-archives
;;               `("melpa" . "https://melpa.org/packages/"))

;; (setq package-enable-at-startup nil ; don't auto-initialize!
;;       ;; this tells package.el not to add those pesky customized variable settings
;;       ;; at the end of your init.el
;;       package--init-file-ensured t)

;;  `use-package;;install through init file
;;  (unless (package-installed-p `use-package)
;;    (package-refresh-contents)
;;    (package-install `use-package))                                                        

;; ;;try out a cool package  
;; (use-package try
;;   :ensure t
;;   :defer 1)

;; ;; (setq xah-fly-use-control-key nil)
 (require 'xah-fly-keys)
;; ;; ;(use-package xah-fly-keys
;; ;; ;  :ensure t
;; ;; ;  :defer 1)
 (xah-fly-keys-set-layout "qwerty")
 (xah-fly-command-mode-activate);command mode on startups

(setq-default truncate-lines t)
;; (setq truncate-partial-width-windows t)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(set-background-color "#161616")
(set-face-attribute 'fringe nil :background nil)
(set-foreground-color "burlywood2")
(set-face-attribute 'default t :font "-outline-Inconsolata-normal-normal-normal-mono-12-*-*-*-c-*-iso8859-1" )
(set-face-attribute 'font-lock-builtin-face nil :foreground "#D6AF2A")
(set-face-attribute 'font-lock-comment-face nil :foreground "gray50")
(set-face-attribute 'font-lock-constant-face nil :foreground "#F0F0F0")
(set-face-attribute 'font-lock-doc-face nil :foreground "gray50")
(set-face-attribute 'font-lock-function-name-face nil :foreground "#508C83")
(set-face-attribute 'font-lock-keyword-face nil :foreground "#D6AF2A")
(set-face-attribute 'font-lock-string-face nil :foreground "olive drab")
(set-face-attribute 'font-lock-type-face nil :foreground "burlywood3")
(set-face-attribute 'font-lock-variable-name-face nil :foreground "#D4D2C8")

;; ;;decrease threshold
(setq gc-cons-threshold (* 2 1000 1000))

          

