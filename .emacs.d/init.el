 ;;quality of life stuf
;(server-start) -> spooky
;increase garbage collection at startup
(setq startup/gc-cons-threshold gc-cons-threshold)
(setq gc-cons-threshold most-positive-fixnum)
(defun startup/reset-gc () (setq gc-cons-threshold startup/gc-cons-threshold))
(add-hook 'emacs-startup-hook 'startup/reset-gc)

(menu-bar-mode -1)
(tool-bar-mode -1)
(setq case-fold-search nil)
(global-subword-mode 1);;move through camel case

(defun my-inhibit-startup-screen-always ()
  "Startup screen inhibitor for `command-line-functions`.
Inhibits startup screen on the first unrecognised option."
  (ignore (setq inhibit-startup-screen t)))

(add-hook 'command-line-functions #'my-inhibit-startup-screen-always)

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
(setq default-style '((java-mode . "java")
                      (other. "linux")))

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

(require 'rand-theme)
(setq rand-theme-wanted '(banal flatland gruvbox-dark-soft afternoon creamsody foggy-night
                                sanityinc-tomorrow-bright ample darktooth gruvbox minsk naysayer ample-flat
                                gruvbox-dark-hard nord sanityinc-tomorrow-eighties soothe default-black-theme))
(rand-theme)

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


