;; -*- lexical-binding: t; -*-
(setq gc-cons-threshold 100000000)
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold 800000)))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq ido-case-fold nil)
(setq case-fold-search nil)
(setq completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)

;; (setq-default truncate-lines t)
;; (setq truncate-partial-width-windows t)
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

(global-subword-mode 1)
(blink-cursor-mode 0)
(setq scroll-conservatively 1)

(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
	   (format "%.2f seconds"
		   (float-time
		    (time-subtract after-init-time before-init-time)))
	   gcs-done))
(add-hook 'emacs-startup-hook #'efs/display-startup-time)

;; inhibit startup screen if a file is passed to it.
(defun my-inhibit-startup-screen-always ()
  (ignore (setq inhibit-startup-screen t)))

(add-hook 'command-line-functions #'my-inhibit-startup-screen-always)

(defadvice split-window (after move-point-to-new-window activate)
  "Moves the point to the newly created window after splitting."
  (other-window 1))

(setq column-numbers-mode t)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

(setq delete-by-moving-to-trash t)
(global-auto-revert-mode)

;; column selection with mouse
(defun mouse-start-rectangle (start-event)
  (interactive "e")
  (deactivate-mark)
  (mouse-set-point start-event)
  (rectangle-mark-mode +1)
  (let ((drag-event))
    (track-mouse
      (while (progn
               (setq drag-event (read-event))
               (mouse-movement-p drag-event))
        (mouse-set-point drag-event)))))

(global-set-key (kbd "S-<down-mouse-1>") #'mouse-start-rectangle)

(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(defalias 'yes-or-no-p 'y-or-n-p)
(setq search-whitespace-regexp "[-_ \t\n]+")

(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

(defun cmd()
  (interactive)
  (let ((proc (start-process "cmd" nil "cmd.exe" "/C" "start" "cmd.exe")))
    (set-process-query-on-exit-flag proc nil))
)

(defun bg()
  (interactive)
  (let ((proc (start-process "baregrep" nil "baregrep.exe" "" "baregrep.exe")))
    (set-process-query-on-exist-flag proc nil))
)

(setq-default select-enable-clipboard t)

(setq make-backup-files nil)

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

(rg-use-old-defaults)

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

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(add-to-list 'auto-mode-alist '("\\.ts\\'" . javascript-mode))

(add-hook 'go-mode-hook (lambda () (setq tab-width 4)))

(add-to-list 'load-path "~/.emacs.d/lisp/")

(unless (version< emacs-version "24")
  (add-to-list 'load-path "~/.emacs.d/lisp/")
  (autoload 'zig-mode "zig-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.zig\\'" . zig-mode)))

(setq xah-fly-use-control-key nil)
(require 'xah-fly-keys)
(xah-fly-keys-set-layout "qwerty")
(xah-fly-keys 1)

(global-set-key (kbd "`") 'xah-fly-command-mode-activate) ;; if I need to backtick then alt-9-6 I guess

(set-face-attribute 'fringe nil :background nil)
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

(require 'font-lock)

(load-theme 'naysayer t)

(setq compile-command "build.bat")
(define-key xah-fly-command-map (kbd "q") 'goto-line)
(define-key xah-fly-command-map (kbd "'") 'zap-up-to-char)
(global-set-key (kbd "<f2>") 'string-rectangle)
(global-set-key (kbd "<f5>") 'repeat-complex-command)
;; (global-set-key (kbd "<f6>") 'xah-open-in-vscode)
(global-set-key (kbd "<f6>") 'indent-rigidly)
(global-set-key (kbd "<f10>") 'align-regexp) 
(global-set-key (kbd "<f12>") 'isearch-forward-symbol-at-point)

(global-set-key (kbd "<kp-8>") 'xah-next-user-buffer)
(global-set-key (kbd "<kp-2>") 'xah-previous-user-buffer)
    
(global-set-key (kbd "M-q") 'compile)
(global-set-key (kbd "M-]") 'forward-paragraph)
(global-set-key (kbd "M-[") 'backward-paragraph)
(global-set-key (kbd "M-w") 'move-line-up)
(global-set-key (kbd "M-s") 'move-line-down)

(global-set-key (kbd "C-s") 'save-buffer)        
(global-set-key (kbd "C-p") 'previous-line)        
(global-set-key (kbd "C-n") 'next-line)
(global-set-key (kbd "C-f") 'forward-char)
(global-set-key (kbd "C-b") 'backward-char)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-basic-offset 4)
 '(custom-safe-themes
   '("16ab866312f1bd47d1304b303145f339eac46bbc8d655c9bfa423b957aa23cc9" "5d59bd44c5a875566348fa44ee01c98c1d72369dc531c1c5458b0864841f887c" default))
 '(linum-format " %5i ")
 '(package-selected-packages
   '(tuareg rg naysayer-theme typescript-mode yaml-mode xah-fly-keys xah-find which-key use-package try toml-mode s rust-mode go-mode git-commit company command-log-mode)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Inconsolata SemiExpanded" :foundry "outline" :slant normal :weight normal :height 120 :width normal)))))
 
(put 'narrow-to-region 'disabled nil)
