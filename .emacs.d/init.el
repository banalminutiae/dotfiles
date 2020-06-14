;;quality of life stuf
;(server-start) -> spooky
(menu-bar-mode -1)
(tool-bar-mode -1)
(setq case-fold-search nil)
(global-subword-mode 1);;move through camel case


(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(setq custom-file "~/.emacs.d/def.el")
(load custom-file)

(setq-default x-select-enable-clipboard t)

(setq-default mode-line-format
              '("%e"
                mode-line-front-space
                mode-line-mule-info
                mode-line-client
                mode-line-modified
                mode-line-remote
                mode-line-frame-identification
                mode-line-buffer-identification
                "  "  
                "(%l,%c)"
                (vc-mode vc-mode);version control
                " "
                "(%m)"
                " "
                mode-line-end-spaces));pad rest of modeline

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

(yas-global-mode 1)

;;autocomplete command completion
(use-package which-key
  :ensure t
  :config)

(require 'xah-fly-keys)
(xah-fly-keys-set-layout "qwerty")
(xah-fly-command-mode-activate);command mode on startups

(load-theme 'banal t);nicest theme
