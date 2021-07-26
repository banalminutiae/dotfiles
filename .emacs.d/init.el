;; -*- lexical-binding: t; -*-
(setq gc-cons-threshold 100000000)
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold 800000)))
(add-hook 'focus-out-hook 'garbage-collect)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq ido-case-fold nil)
(setq case-fold-search nil)
(setq completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)

(setq-default truncate-lines t)
;; (setq truncate-partial-width-windows t)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(global-subword-mode 1);;move through camel case
(blink-cursor-mode 0)
(setq scroll-conservatively 1)
;; ;(define-key dired-mode-map [mouse-2] 'dired-mouse-find-file)

(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
	   (format "%.2f seconds"
		   (float-time
		    (time-subtract after-init-time before-init-time)))
	   gcs-done))
(add-hook 'emacs-startup-hook #'efs/display-startup-time)

(defadvice split-window (after move-point-to-new-window activate)
  "Moves the point to the newly created window after splitting."
  (other-window 1))

;; inhibit startup screen if a file is passed it.
(defun my-inhibit-startup-screen-always ()
  "Startup screen inhibitor for `command-line-functions`.
Inhibits startup screen on the first unrecognised option."
  (ignore (setq inhibit-startup-screen t)))

(add-hook 'command-line-functions #'my-inhibit-startup-screen-always)

(global-auto-revert-mode)

(defalias 'dk 'describe-key)
(defalias 'dt 'disable-theme)
(defalias 'er 'eval-region)

(setq initial-major-mode 'fundamental-mode)
(global-visual-line-mode t)

(defalias 'yes-or-no-p 'y-or-n-p)
(setq search-whitespace-regexp "[-_ \t\n]+")
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

(global-set-key (kbd "M-[") 'backward-paragraph)
(global-set-key (kbd "M-]") 'forward-paragraph)
(global-set-key (kbd "<f6>") 'xah-open-in-vscode)
(global-set-key (kbd "<f10>") 'align-regexp)
(global-set-key (kbd "<f5>") 'repeat-complex-command)
(global-set-key (kbd "C-s") 'save-buffer)

(global-set-key (kbd "<kp-home>") 'describe-key)

(setq-default select-enable-clipboard t)

(setq backup-directory-alist `(("." . "~/.saves")))
 
(show-paren-mode 1)

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

(setq-default electric-indent-inhibit t)	

;;bracket matching
(electric-pair-mode 1)
(setq electric-pair-pairs
      '(
        (?\".?\")
        (?\{.?\})))

;; i-search with more intuitive controls 
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

;; (require 'package)
;; (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.kt\\'" . java-mode))
(add-hook 'go-mode-hook (lambda () (setq tab-width 4)))

(add-to-list 'load-path "~/.emacs.d/lisp/") ;; probably don't load an entire directory just for the xfk file

(setq xah-fly-use-control-key nil)
(require 'xah-fly-keys)
(xah-fly-keys-set-layout "qwerty")
(xah-fly-keys 1)

(global-set-key (kbd "`") 'xah-fly-command-mode-activate) ;; if I need to backtick then alt-9-6 I guess
(global-set-key (kbd "C-d") 'xah-fly-command-mode-activate)

(set-face-attribute 'fringe nil :background nil)
(set-background-color "#161616")

(set-foreground-color "burlywood2")
(set-face-attribute 'font-lock-builtin-face nil :foreground "#DAB98F")
(set-face-attribute 'font-lock-comment-face nil :foreground "gray50")
(set-face-attribute 'font-lock-constant-face nil :foreground "olive drab")
(set-face-attribute 'font-lock-doc-face nil :foreground "gray50")
(set-face-attribute 'font-lock-function-name-face nil :foreground "burlywood3")
(set-face-attribute 'font-lock-keyword-face nil :foreground "DarkGoldenrod3") 
(set-face-attribute 'font-lock-string-face nil :foreground "olive drab")
(set-face-attribute 'font-lock-type-face nil :foreground "#D6AF2A")
(set-face-attribute 'font-lock-variable-name-face nil :foreground "burlywood2")

;; turn on font lock with maximum decoration
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

(require 'font-lock)

;; create a face for function calls
(defface font-lock-function-call-face
'((t (:foreground "DarkGoldenrod3")))
"Font Lock mode face used to highlight function calls."
:group 'font-lock-highlighting-faces)
(defvar font-lock-function-call-face 'font-lock-function-call-face)

;; add it to the font lock tables
(add-hook 'c-mode-common-hook 
	  (lambda ()
	    (font-lock-add-keywords
	     nil
	     '(("\\<\\(\\sw+\\) ?(" 1 font-lock-function-call-face)) t)))

(add-hook 'go-mode-hook 
	  (lambda ()
	    (font-lock-add-keywords
	     nil
	     '(("\\<\\(\\sw+\\) ?(" 1 font-lock-function-call-face)) t)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-basic-offset 4)
 '(menu-bar-mode nil)
 '(package-selected-packages
   '(yaml-mode xah-fly-keys xah-find which-key use-package try toml-mode s rust-mode rjsx-mode go-mode git-commit company command-log-mode))
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Inconsolata SemiExpanded" :foundry "outline" :slant normal :weight normal :height 120 :width normal)))))
 ;; shitty custom variables. Edit only through the menu thingy
