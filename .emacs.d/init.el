;; -*- lexical-binding: t; -*-
(setq gc-cons-threshold 100000000)
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold 800000)))
(add-hook 'focus-out-hook 'garbage-collect)
(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
	   (format "%.2f seconds"
		   (float-time
		    (time-subtract after-init-time before-init-time)))
	   gcs-done))
(add-hook 'emacs-startup-hook #'efs/display-startup-time)

(menu-bar-mode -1)
(tool-bar-mode -1)
(setq case-fold-search nil)
(setq completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)
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

(defadvice split-window (after move-point-to-new-window activate)
  "Moves the point to the newly created window after splitting."
  (other-window 1))

(global-set-key (kbd "<f5>") 'repeat-complex-command)
(global-set-key (kbd "<f7>") 'kill-buffer-and-window)

(setq-default select-enable-clipboard t)

(setq backup-directory-alist `(("." . "~/.saves")))
 
;; (show-paren-mode 1)

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

(setq-default electric-indent-inhibit t)	

;;bracket matching
(electric-pair-mode 1)
(setq electric-pair-pairs
      '(
        (?\".?\")
        (?\{.?\})))

(cua-mode 1)
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

(add-to-list 'load-path "~/.emacs.d/lisp/")
(require 'xah-fly-keys)
(xah-fly-keys-set-layout "qwerty")
(xah-fly-keys 1)

(setq-default truncate-lines t)
;; (setq truncate-partial-width-windows t)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(set-background-color "#161616")
(set-foreground-color "burlywood2")
(set-face-attribute 'fringe nil :background nil)
(set-face-attribute 'default t :font "-outline-Inconsolata-normal-normal-normal-mono-12-*-*-*-c-*-iso8859-1" )
(set-face-attribute 'font-lock-builtin-face nil :foreground "#D6AF2A")
(set-face-attribute 'font-lock-comment-face nil :foreground "gray50")
(set-face-attribute 'font-lock-constant-face nil :foreground "#F0F0F0")
(set-face-attribute 'font-lock-doc-face nil :foreground "gray50")
(set-face-attribute 'font-lock-function-name-face nil :foreground "#508C83")
(set-face-attribute 'font-lock-keyword-face nil :foreground "#D4D2C8") ; "#D6AF2A"
(set-face-attribute 'font-lock-string-face nil :foreground "olive drab")
(set-face-attribute 'font-lock-type-face nil :foreground "#D6AF2A")
(set-face-attribute 'font-lock-variable-name-face nil :foreground "#burlywood2") ; "#D4D2C8"

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-basic-offset 4)
 '(menu-bar-mode nil)
 '(package-selected-packages
   '(yaml-mode xah-fly-keys xah-find which-key use-package try toml-mode s rust-mode rjsx-mode pyvenv project parent-mode page-break-lines highlight-indentation go-mode git-commit company command-log-mode color-theme-sanityinc-tomorrow ample-theme))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Inconsolata SemiExpanded" :foundry "outline" :slant normal :weight normal :height 120 :width normal)))))
 ;; shitty custom variables. Edit only through the menu thingy
