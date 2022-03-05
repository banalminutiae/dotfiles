;; -*- lexical-binding: t; -*-
(setq gc-cons-threshold 10000000)

(menu-bar-mode -1)
(tool-bar-mode -1)

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

(add-to-list 'load-path "~/.emacs.d/lisp/")

(setq xah-fly-use-control-key nil)
(require 'xah-fly-keys)
(xah-fly-keys-set-layout "qwerty")
(xah-fly-keys 1)

(global-set-key (kbd "`") 'xah-fly-command-mode-activate) ;; if I need to backtick then alt-9-6 I guess

(add-to-list 'default-frame-alist '(foreground-color . "#d1b897"))
(add-to-list 'default-frame-alist '(background-color . "#062329"))
(set-face-attribute 'font-lock-builtin-face nil :foreground "#ffffff")
(set-face-attribute 'font-lock-comment-face nil :foreground "#44b340")
(set-face-attribute 'font-lock-constant-face nil :foreground "#44b340")
(set-face-attribute 'font-lock-function-name-face nil :foreground "#ffffff")
(set-face-attribute 'font-lock-keyword-face nil :foreground "#ffffff")
(set-face-attribute 'font-lock-type-face nil :foreground "#8cde94")
(set-face-attribute 'font-lock-string-face nil :foreground "#2ec09c")
(set-face-attribute 'font-lock-variable-name-face nil :foreground "#c1d1e3")

(set-face-attribute 'fringe nil :background nil)
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

(setq compile-command "build.bat")
(define-key xah-fly-command-map (kbd "q") 'goto-line)
(define-key xah-fly-command-map (kbd "b") 'zap-up-to-char)

(global-set-key (kbd "<f1>") 'isearch-forward-symbol-at-point)
(global-set-key (kbd "<f2>") 'string-rectangle)
;; <f3>
;; <f4>
(global-set-key (kbd "<f5>") 'repeat-complex-command)
(global-set-key (kbd "<f6>") 'indent-rigidly)
;; <f7>
;; <f8>
;; <f9> 
;; <f10>
;; <f11>
(global-set-key (kbd "<f12>") 'align-regexp) 

(global-set-key (kbd "M-q") 'compile)
(global-set-key (kbd "M-s") 'move-line-down)
(global-set-key (kbd "M-w") 'move-line-up)
(global-set-key (kbd "M-]") 'forward-paragraph)
(global-set-key (kbd "M-[") 'backward-paragraph)

(global-set-key (kbd "C-b") 'backward-char)
(global-set-key (kbd "C-f") 'forward-char)
(global-set-key (kbd "C-n") 'next-line)
(global-set-key (kbd "C-p") 'previous-line)        
(global-set-key (kbd "C-s") 'save-buffer)        

(global-set-key (kbd "<kp-8>") 'xah-next-user-buffer)
(global-set-key (kbd "<kp-2>") 'xah-previous-user-buffer)

(setq custom-file (locate-user-emacs-file "custom_vars.el"))
(load custom-file 'noerror 'message) 

(setq gc-cons-threshold (* 2 1000 000))
