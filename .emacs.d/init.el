(setq gc-cons-threshold (* 511 1024 1024))
(setq gc-cons-percentage 0.5)
(run-with-idle-timer 5 t #'garbage-collect)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(setq case-fold-search nil)
(global-subword-mode 1);;move through camel case

(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse 't)
(global-hl-line-mode 1)

(setq line-number-mode t)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(setq custom-file "~/.emacs.d/def.el")
(load custom-file)

(setq-default x-select-enable-clipboard t)

;; (setq-default mode-line-format
;;               '("%e"
;;                 mode-line-front-space
;;                 mode-line-mule-info
;;                 mode-line-client
;;                 mode-line-modified
;;                 mode-line-remote
;;                 mode-line-frame-identification
;;                 mode-line-buffer-identification
;;                 "  "  
;;                 "(%l,%c)"
;;                 (vc-mode vc-mode);version control
;;                 " "
;;                 "(%m)"
;;                 " "
;;                 ))

(setq backup-directory-alist `(("." . "~/.saves")))

(show-paren-mode 1)

(set-language-environment "UTF-8")

(setq x-select-enable-clipboard t)

(setq-default indent-tabs-mode nil)
(setq tab-width 4)
(setq-default tab-wdith 4)
(setq-default c-basic-offset 4)
 (setq c-default-style
           '((java-mode . "java")
             (other . "linux")))

;;aesthetic
(global-prettify-symbols-mode t)

;;completion
(add-hook 'after-init-hook 'global-company-mode)

;;bracket matching
(electric-pair-mode 1)
(setq electric-pair-pairs
      '(
        (?\".?\")
        (?\{.?\})))

(cua-mode 1)
(ido-mode 1)

(add-hook 'go-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'gofmt-before-save)
            (setq tab-width 4)
            (setq indent-tabs-mode 1)))

(add-hook 'rust-mode-hook
          (lambda () (setq indent-tabs-mode nil)))

;;i-search with more intuitive controls
(progn
  (define-key isearch-mode-map (kbd "<up>")`isearch-ring-retreat)
  (define-key isearch-mode-map (kbd "<down>")`isearch-ring-advance)
  (define-key isearch-mode-map (kbd "<left>")`isearch-repeat-backward)
  (define-key isearch-mode-map (kbd "<right>")`isearch-repeat-forward)
  (define-key minibuffer-local-isearch-map (kbd"<left>")`isearch-reverse-exit-minibuffer)
  (define-key minibuffer-local-isearch-map (kbd "<right>")`isearch-forward-exit-minibuffer))

;;no backups
(setq make-backp-files nil)
(setq backup-by-copying t)
(setq auto-save-default nil)

;;package manager
(require `package)
(setq package-enable-ar-startup nil)
(add-to-list `package-archives
             `("melpa" . "https://melpa.org/packages/"))

(package-initialize)

`use-package;;install through init file
(unless (package-installed-p `use-package)
  (package-refresh-contents)
  (package-install `use-package))

;;try out a cool package
(use-package try
  :ensure t)

;(use-package ace-jump-mode)
;(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

(use-package multiple-cursors
  :ensure t)

(yas-global-mode 1)

;;autocomplete command completion
(use-package which-key
  :ensure t
  :config)

(use-package toml-mode)

(require 'rust-mode)

(use-package cargo
  :hook (rust-mode . cargo-minor-mode))

(require 'xah-fly-keys)
(xah-fly-keys-set-layout "qwerty")
(xah-fly-command-mode-activate);command mode on startups

(load-theme 'ample-flat)
