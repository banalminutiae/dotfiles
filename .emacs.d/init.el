;; -*- lexical-binding: t; -*-

(setq inhibit-splash-screen t)
(setq initial-major-mode 'fundamental-mode)
(setq frame-inhibit-implied-resize t)

(menu-bar-mode -1)
(tool-bar-mode -1)

(setq ido-case-fold nil)
(setq case-fold-search nil)
(setq completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)

(setq-default truncate-lines t)
(setq truncate-partial-width-windows t)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(blink-cursor-mode 0)
(setq scroll-conservatively 1)

(put 'set-goal-column 'disabled nil)

(prettify-symbols-mode 1) 
(global-visual-line-mode 1)
(setq-default c-basic-offset 4)

(defun efs/display-startup-time ()
    (message "Emacs loaded in %s with %d garbage collections and %d features loaded."
             (format "%.2f seconds"
                     (float-time
                       (time-subtract after-init-time before-init-time)))
             gcs-done (length features)))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)

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
(define-key key-translation-map (kbd "<apps>") (kbd "C-g"))

(defun transpose-words-left (arg)
  (interactive "*p")
  (transpose-subr 'backward-word arg))

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

(when (eq system-type 'windows-nt)
  (with-eval-after-load 'grep
    ;; findstr can handle the basic find|grep use case
    (grep-apply-setting 'grep-find-template
                        "findstr /S /N /D:. /C:<R> <F>")
    (setq find-name-arg nil)))

(setq-default select-enable-clipboard t)

(setq make-backup-files nil)

(show-paren-mode 1)

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

(setq-default electric-indent-inhibit t)	

(electric-pair-mode 1)
(setq electric-pair-pairs
      '(
        (?\".?\")
        (?\{.?\})))

;;no backups
(setq make-backp-files nil)
(setq backup-by-copying t)
(setq auto-save-default nil)

(add-to-list 'default-frame-alist '(font . "Inconsolata SemiExpanded-12"))

(set-face-attribute 'font-lock-builtin-face nil :foreground "#ffffff")
(set-face-attribute 'font-lock-comment-face nil :foreground "#44b340")
(set-face-attribute 'font-lock-doc-face nil :foreground "#2ec90c")
(set-face-attribute 'font-lock-constant-face nil :foreground "#44b340")
(set-face-attribute 'font-lock-function-name-face nil :foreground "#ffffff")
(set-face-attribute 'font-lock-keyword-face nil :foreground "#ffffff")
(set-face-attribute 'font-lock-type-face nil :foreground "#8cde94")
(set-face-attribute 'font-lock-string-face nil :foreground "#2ec09c")
(set-face-attribute 'font-lock-variable-name-face nil :foreground "#c1d1e3")
(set-face-attribute 'font-lock-preprocessor-face nil :foreground "#8cde94")
(set-face-attribute 'font-lock-warning-face nil :foreground "#ffaa00")
(set-face-attribute 'font-lock-negation-char-face nil :foreground "#ffaa00")

(add-to-list 'default-frame-alist '(foreground-color . "#d1b897")) 
(add-to-list 'default-frame-alist '(background-color . "#062329")) 

(set-face-attribute 'fringe nil :background "#062329" :foreground "#062329") 
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'odin-mode)
(add-to-list 'auto-mode-alist '("\\.odin\\'" . odin-mode))
;; close enough
(add-to-list 'auto-mode-alist '("\\.go\\'" . odin-mode))
(add-to-list 'auto-mode-alist '("\\.cs\\'" . java-mdoe)) 
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(setq xah-fly-use-control-key nil)
(require 'xah-fly-keys)
(xah-fly-keys-set-layout "qwerty")
(xah-fly-keys 1)

(global-set-key (kbd "`") 'xah-fly-command-mode-activate) ;; if I need to backtick then alt-9-6 I guess

(setq compile-command "build.bat")

;; sequences with leader key SPACE
(define-key xah-fly-leader-key-map (kbd "5") 'list-buffers)
(define-key xah-fly-leader-key-map (kbd "9") 'make-frame-command)
(define-key xah-fly-leader-key-map (kbd "[") 'hippie-sequences)

;; expand with leader key 't' i.e. k in qwerty
(define-key xah-fly-Rp2p0-key-map (kbd "a") 'list-matching-lines)

;; command mode shortcuts
(define-key xah-fly-command-map (kbd "q") 'goto-line)
(define-key xah-fly-command-map (kbd "b") 'zap-up-to-char)
(define-key xah-fly-command-map (kbd "]") 'transpose-words)
(define-key xah-fly-command-map (kbd "[") 'transpose-words-left)    
(define-key xah-fly-command-map (kbd "'") 'find-file-other-window)
(define-key xah-fly-command-map (kbd "\\") 'switch-to-buffer-other-window)

(global-set-key (kbd "<f1>") 'isearch-forward-symbol-at-point)
(global-set-key (kbd "<f2>") 'string-rectangle)
(global-set-key (kbd "<f3>") 'align-regexp)
(global-set-key (kbd "<f4>") 'xah-reformat-whitespaces-to-one-space)     
(global-set-key (kbd "<f5>") 'repeat-complex-command)
(global-set-key (kbd "<f6>") 'kmacro-start-macro)   
(global-set-key (kbd "<f7>") 'kmacro-end-and-call-macro)   
;; <f8>
;; <f9> 
;; <f10>
(global-set-key (kbd "<f11>") 'indent-rigidly)
;; <f12>

(global-set-key (kbd "M-f") 'forward-word)
(global-set-key (kbd "M-b") 'backward-word)
(global-set-key (kbd "M-]") 'forward-paragraph)
(global-set-key (kbd "M-[") 'backward-paragraph)
(global-set-key (kbd "M-n") 'next-line)
(global-set-key (kbd "M-p") 'previous-line)        
(global-set-key (kbd "M-q") 'compile)
(global-set-key (kbd "M-s") 'move-line-down)
(global-set-key (kbd "M-w") 'move-line-up)
;; C-k -> SPACE-g, replace with something
(global-set-key (kbd "C-b") 'backward-char)
(global-set-key (kbd "C-f") 'forward-char)
(global-set-key (kbd "C-n") 'xah-next-user-buffer)
(global-set-key (kbd "C-p") 'xah-previous-user-buffer)        
(global-set-key (kbd "C-s") 'save-buffer)

;; pipe custom variables into another file, never actually write it to existence
(setq custom-file (locate-user-emacs-file "custom_vars.el"))
