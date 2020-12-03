(setq gc-cons-threshold (* 511 1024 1024))
(setq gc-cons-percentage 0.5)
(run-with-idle-timer 5 t #'garbage-collect)
;; (scroll-bar-mode -1)
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

(setq backup-directory-alist `(("." . "~/.saves")))
 
(show-paren-mode 1)

(set-language-environment "UTF-8")

(setq x-select-enable-clipboard t)
(set-terminal-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)

(setq-default indent-tabs-mode nil)
(setq tab-width 4)
(setq-default tab-wdith 4)
(setq-default c-basic-offset 4)
 (setq c-default-style
           '((java-mode . "java")
             (other . "linux")))

(setq auto-hscroll-mode 'current-line)

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
(use-package highlight-numbers
  :ensure t
  :config)

(add-hook 'prog-mode-hook 'highlight-numbers-mode)
'(highlight-numbers-number ((t (:foreground "#e95c4b"))))

`use-package;;install through init file
(unless (package-installed-p `use-package)
  (package-refresh-contents)
  (package-install `use-package))                                                        

;;try out a cool package  
(use-package try
  :ensure t)

;;autocomplete command completion
(use-package which-key
  :ensure t
  :config)

(use-package toml-mode)

(require 'rust-mode)

(use-package cargo
  :hook (rust-mode . cargo-minor-mode))

(use-package csharp-mode
  :ensure t)

(require 'xah-fly-keys)                    
(xah-fly-keys-set-layout "qwerty")
(xah-fly-command-mode-activate);command mode on startups

;SPC-

(define-key xah-fly-leader-key-map (kbd "b") 'bookmark-bmenu-list);bring up bookmarks
;; (define-key xah-fly-leader-key-map (kbd "b") 'xah-open-file-fast);bring up bookmarks with ido window, for when there's tons of relevant files 

;coppied out of xah-fly-keys.el 
;key funcitonality without leader key in command mode
(defun xah-fly-command-mode-init ()
  "Set command mode keys.
Version 2017-01-21"
  (interactive)
  (xah-fly--define-keys
   xah-fly-key-map
   '(
     ("~" . nil)
     (":" . nil)

     ("SPC" . xah-fly-leader-key-map)
     ("DEL" . xah-fly-leader-key-map)

     ("'" . xah-reformat-lines)
     ("," . xah-shrink-whitespaces)
     ("-" . xah-cycle-hyphen-underscore-space)
     ("." . xah-backward-kill-word)
     (";" . xah-comment-dwim)
     ("/" . hippie-expand)
     ("\\" . nil)
     ;; ("=" . xah-forward-equal-sign)
     ("[" . xah-backward-punct )
     ("]" . xah-forward-punct)
     ("`" . other-frame)

     ;; ("#" . xah-backward-quote)
     ;; ("$" . xah-forward-punct)

     ("1" . xah-extend-selection)
     ("2" . xah-select-line)
     ("3" . delete-other-windows)
     ("4" . split-and-follow-horizontally)
     ("5" . delete-char)
     ("6" . xah-select-block)
     ("7" . xah-select-line)
     ("8" . xah-extend-selection)
     ("9" . xah-select-text-in-quote)
     ("0" . xah-pop-local-mark-ring)

     ("a" . execute-extended-command)
     ("b" . isearch-forward)
     ("c" . previous-line)
     ("d" . xah-beginning-of-line-or-block)
     ("e" . xah-delete-backward-char-or-bracket-text)
     ("f" . undo)
     ("g" . backward-word)
     ("h" . left-char);backward-char
     ("i" . xah-delete-current-text-block)
     ("j" . xah-copy-line-or-region)
     ("k" . xah-paste-or-paste-previous)
     ;; ("l" . xah-fly-insert-mode-activate-space-before)
     ("l" . xah-insert-space-before)
     ("m" . xah-backward-left-bracket)
     ("n" . right-char);forward-char
     ("o" . goto-line)
     ("p" . xah-kill-word)
     ("q" . xah-cut-line-or-region)
     ("r" . forward-word)
     ("s" . xah-end-of-line-or-block)
     ("t" . next-line)
     ("u" . xah-fly-insert-mode-activate)
     ("v" . xah-forward-right-bracket)
     ("w" . xah-next-window-or-frame)
     ("x" . xah-toggle-letter-case)
     ("y" . set-mark-command)
     ("z" . xah-goto-matching-bracket)))

  (define-key xah-fly-key-map (kbd (xah-fly--key-char "a"))
    (cond ((fboundp 'smex) 'smex)
	  ((fboundp 'helm-M-x) 'helm-M-x)
	  ((fboundp 'counsel-M-x) 'counsel-M-x)
	  (t 'execute-extended-command)))

  ;; (when xah-fly-swapped-1-8-and-2-7-p
  ;;     (xah-fly--define-keys
  ;;      xah-fly-key-map
  ;;      '(
  ;;        ("8" . pop-global-mark)
  ;;        ("7" . xah-pop-local-mark-ring)
  ;;        ("2" . xah-select-line)
  ;;        ("1" . xah-extend-selection))))

  (progn
    (setq xah-fly-insert-state-q nil )
    (modify-all-frames-parameters (list (cons 'cursor-type 'box))))

  (setq mode-line-front-space "C")
  (force-mode-line-update)

  ;;
  )

(setq-default truncate-lines t)
(setq truncate-partial-width-windows t)
(split-window-horizontally)

(set-background-color "gray9")
(set-face-attribute 'default nil :foreground "#DAB98F")
(set-face-attribute 'hl-line nil :foreground nil :background "blue")
(set-face-attribute 'font-lock-builtin-face nil :foreground "#DAB98F")
(set-face-attribute 'font-lock-comment-face nil :foreground "dim gray")
(set-face-attribute 'font-lock-constant-face nil :foreground "olive drab")
(set-face-attribute 'font-lock-doc-face nil :foreground "dim gray")
(set-face-attribute 'font-lock-function-name-face nil :foreground "burlywood3")
(set-face-attribute 'font-lock-keyword-face nil :foreground "DarkGoldenrod3")
(set-face-attribute 'font-lock-string-face nil :foreground "olive drab")
(set-face attribute 'font-lock-type-face nil: foreground "burlywood3")
(set-face-attribute 'font-lock-variable-name-face nil :foreground "burlywood3")



;; (load-theme 'ample-flat) 
;; (set-face-attribute 'default t :font "-outline-Inconsolata-normal-normal-normal-mono-13-*-*-*-c-*-iso8859-1" )
