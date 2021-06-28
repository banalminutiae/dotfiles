;; -*- lexical-binding: t; -*-
;; (setq gc-cons-threshold (* 50 1000 1000))
(setq frame-inhibit-implied-resize t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(setq case-fold-search nil)
(global-subword-mode 1);;move through camel case

(setq scroll-conservatively 1)
;(define-key dired-mode-map [mouse-2] 'dired-mouse-find-file)

(setq doc-view-continuous t)

(defalias 'eb 'eval-buffer)
(defalias 'er 'eval-region)
(defalias 'dk 'describe-key)

(setq initial-major-mode 'fundamental-mode)
(global-visual-line-mode t)

(defalias 'yes-or-no-p 'y-or-n-p)
(setq search-whitespace-regexp "[-_ \t\n]+")
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

(global-set-key (kbd "<f5>") 'repeat-complex-command)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)
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
 
(global-set-key (kbd "<S-tab>") 'un-indent-by-removing-4-spaces)
(defun un-indent-by-removing-4-spaces ()
  "remove 4 spaces from beginning of of line"
  (interactive)
  (save-excursion
    (save-match-data
      (beginning-of-line)
      ;; get rid of tabs at beginning of line
      (when (looking-at "^\\s-+")
        (untabify (match-beginning 0) (match-end 0)))
      (when (looking-at "^    ")
        (replace-match "")))))

(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse 't)

(global-auto-revert-mode 1)
(setq line-move-visual nil)

(setq line-number-mode t)

(setq-default select-enable-clipboard t)

(setq backup-directory-alist `(("." . "~/.saves")))
 
(show-paren-mode 1)

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

(setq-default indent-tabs-mode t)
(setq tab-width 4)
(setq-default tab-wdith 4)
(setq c-basic-offset 4)
(setq c-default-style
      '((java-mode . "java")
        (other . "linux")))

;; (add-hook 'after-init-hook 'global-company-mode)

;;bracket matching
(electric-pair-mode 1)
(setq electric-pair-pairs
      '(
        (?\".?\")
        (?\{.?\})))

(cua-mode 1)
(ido-mode 1)

;; (add-hook 'go-mode-hook
;;           (lambda ()
;;             (add-hook 'before-save-hook 'gofmt-before-save)
;;             (setq tab-width 4)
;;             (setq indent-tabs-mode 1)))

;; (add-hook 'rust-mode-hook
;;           (lambda () (setq indent-tabs-mode nil)))

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

 ;;package manager
 (require `package)
 (setq package-enable-ar-startup nil)
 (add-to-list `package-archives
              `("melpa" . "https://melpa.org/packages/"))

(setq package-enable-at-startup nil ; don't auto-initialize!
      ;; this tells package.el not to add those pesky customized variable settings
      ;; at the end of your init.el
      package--init-file-ensured t)

 `use-package;;install through init file
 (unless (package-installed-p `use-package)
   (package-refresh-contents)
   (package-install `use-package))                                                        

;;try out a cool package  
(use-package try
  :ensure t
  :defer 1)

(setq xah-fly-use-control-key nil)
(require 'xah-fly-keys)
;(use-package xah-fly-keys
;  :ensure t
;  :defer 1)
(xah-fly-keys-set-layout "qwerty")
(xah-fly-command-mode-activate);command mode on startups

(setq-default truncate-lines t)
(setq truncate-partial-width-windows t)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(set-background-color "#161616")
(set-face-attribute 'fringe nil :background nil)
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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline success warning error])
 '(ansi-color-names-vector
   ["#3c3836" "#fb4933" "#b8bb26" "#fabd2f" "#83a598" "#d3869b" "#8ec07c" "#ebdbb2"])
 '(ansi-term-color-vector
   [unspecified "#2d2a2e" "#ff6188" "#a9dc76" "#ffd866" "#78dce8" "#ab9df2" "#ff6188" "#fcfcfa"] t)
 '(awesome-tray-mode-line-active-color "#2fafff")
 '(awesome-tray-mode-line-inactive-color "#323232")
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(compilation-message-face 'default)
 '(cua-global-mark-cursor-color "#7ec98f")
 '(cua-mode t nil (cua-base))
 '(cua-overwrite-cursor-color "#e5c06d")
 '(cua-read-only-cursor-color "#8ac6f2")
 '(custom-enabled-themes nil)
 '(custom-safe-themes t)
 '(exwm-floating-border-color "#646464")
 '(fci-rule-color "#383838")
 '(flymake-error-bitmap '(flymake-double-exclamation-mark modus-themes-fringe-red))
 '(flymake-note-bitmap '(exclamation-mark modus-themes-fringe-cyan))
 '(flymake-warning-bitmap '(exclamation-mark modus-themes-fringe-yellow))
 '(fringe-mode 10 nil (fringe))
 '(global-command-log-mode t)
 '(go-mode-hook
   '((lambda nil
       (add-hook 'before-save-hook 'gofmt-before-save)
       (setq tab-width 4)
       (setq indent-tabs-mode nil))))
 '(highlight-changes-colors '("#e5786d" "#834c98"))
 '(highlight-symbol-colors
   '("#55204c0039fc" "#3f0a4e4240dc" "#5a2849c746fd" "#3fd2334a42f4" "#426a4d5455d9" "#537247613a13" "#46c549b0535c"))
 '(highlight-symbol-foreground-color "#999891")
 '(highlight-tail-colors
   '(("#2f2f2e" . 0)
     ("#3d464c" . 20)
     ("#3b473c" . 30)
     ("#41434a" . 50)
     ("#4c4536" . 60)
     ("#4b4136" . 70)
     ("#4d3936" . 85)
     ("#2f2f2e" . 100)))
 '(hl-bg-colors
   '("#4c4536" "#4b4136" "#504341" "#4d3936" "#3b313d" "#41434a" "#3b473c" "#3d464c"))
 '(hl-fg-colors
   '("#2a2a29" "#2a2a29" "#2a2a29" "#2a2a29" "#2a2a29" "#2a2a29" "#2a2a29" "#2a2a29"))
 '(hl-paren-colors '("#7ec98f" "#e5c06d" "#a4b5e6" "#834c98" "#8ac6f2"))
 '(hl-todo-keyword-faces
   '(("HOLD" . "#cfdf30")
     ("TODO" . "#feacd0")
     ("NEXT" . "#b6a0ff")
     ("THEM" . "#f78fe7")
     ("PROG" . "#00d3d0")
     ("OKAY" . "#4ae8fc")
     ("DONT" . "#70c900")
     ("FAIL" . "#ff8059")
     ("BUG" . "#ff8059")
     ("DONE" . "#44bc44")
     ("NOTE" . "#f0ce43")
     ("KLUDGE" . "#eecc00")
     ("HACK" . "#eecc00")
     ("TEMP" . "#ffcccc")
     ("FIXME" . "#ff9977")
     ("XXX+" . "#f4923b")
     ("REVIEW" . "#6ae4b9")
     ("DEPRECATED" . "#bfd9ff")))
 '(ibuffer-deletion-face 'modus-themes-mark-del)
 '(ibuffer-filter-group-name-face 'modus-themes-mark-symbol)
 '(ibuffer-marked-face 'modus-themes-mark-sel)
 '(ibuffer-title-face 'modus-themes-pseudo-header)
 '(linum-format " %5i ")
 '(lsp-ui-doc-border "#999891")
 '(main-line-color1 "#222232")
 '(main-line-color2 "#333343")
 '(nrepl-message-colors
   '("#ffb4ac" "#ddaa6f" "#e5c06d" "#3d464c" "#e3eaea" "#41434a" "#7ec98f" "#e5786d" "#834c98"))
 '(org-src-block-faces 'nil)
 '(package-selected-packages
   '(rjsx-mode solarized-theme yaml-mode modus-vivendi-theme eldoc magit xah-find ample-theme rust-mode toml-mode go-mode command-log-mode command-log dashboard xah-fly-keys which-key use-package try))
 '(pdf-view-midnight-colors '("#fdf4c1" . "#282828"))
 '(pos-tip-background-color "#2f2f2e")
 '(pos-tip-foreground-color "#999891")
 '(show-paren-mode t)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#8ac6f2" "#2f2f2e" 0.2))
 '(term-default-bg-color "#2a2a29")
 '(term-default-fg-color "#8d8b86")
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   '((20 . "#ffb4ac")
     (40 . "#f3a0bb068dbb")
     (60 . "#ecf0bdf57dd8")
     (80 . "#e5c06d")
     (100 . "#d1fcc2679b35")
     (120 . "#c551c35ab143")
     (140 . "#b610c464c727")
     (160 . "#a327c588dd05")
     (180 . "#8ac6f2")
     (200 . "#89fec7dad1d0")
     (220 . "#8863c85ec150")
     (240 . "#85eec8dcb0cf")
     (260 . "#82a3c956a041")
     (280 . "#7ec98f")
     (300 . "#9131c244b2d6")
     (320 . "#98acbe43c439")
     (340 . "#9f20ba15d58f")
     (360 . "#a4b5e6")))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   '(unspecified "#2a2a29" "#2f2f2e" "#504341" "#ffb4ac" "#3d464c" "#8ac6f2" "#4c4536" "#e5c06d" "#41434a" "#a4b5e6" "#4d3936" "#e5786d" "#3b473c" "#7ec98f" "#8d8b86" "#74736f"))
 '(xterm-color-names
   ["#2f2f2e" "#ffb4ac" "#8ac6f2" "#e5c06d" "#a4b5e6" "#e5786d" "#7ec98f" "#e8e5db"])
 '(xterm-color-names-bright
   ["#2a2a29" "#ddaa6f" "#6a6a65" "#74736f" "#8d8b86" "#834c98" "#999891" "#f6f3e8"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((((class color) (min-colors 89)) (:foreground "#d4d2c8" :background "#161616" :family "Inconsolata SemiExpanded" :foundry "outline" :slant normal :weight normal :height 120 :width normal))))
 '(sml/charging ((t (:inherit sml/global :foreground "black"))))
 '(sml/client ((t nil))))

;;decrease threshold
(setq gc-cons-threshold (* 2 1000 1000))

          

