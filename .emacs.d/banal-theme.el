;good theme on the eyes
(deftheme banal
  "Created 2020-03-20.")


(custom-theme-set-faces
 'banal
 '(default ((t (:inherit nil :stipple nil :background "gray15" :foreground "#d2b58d" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 110 :width normal :foundry "outline"))))
 '(cursor ((t (:background "#ffffff"))))
 '(fixed-pitch ((t (:family "Monospace"))))
 '(variable-pitch ((((type w32)) (:foundry "outline" :family "Arial")) (t (:family "Sans Serif"))))
 '(escape-glyph ((((background dark)) (:foreground "cyan")) (((type pc)) (:foreground "magenta")) (t (:foreground "brown"))))
 '(homoglyph ((((background dark)) (:foreground "cyan")) (((type pc)) (:foreground "magenta")) (t (:foreground "brown"))))
 '(minibuffer-prompt ((t (:weight normal :foreground "#3FD7E5" :background "#282828"))))
 '(highlight ((t (:background "#0000ff"))))
 '(region ((t (:background "#0000ff"))))
 '(shadow ((((class color grayscale) (min-colors 88) (background light)) (:foreground "grey50")) (((class color grayscale) (min-colors 88) (background dark)) (:foreground "grey70")) (((class color) (min-colors 8) (background light)) (:foreground "green")) (((class color) (min-colors 8) (background dark)) (:foreground "yellow"))))
 '(secondary-selection ((t (:background "#2B3C44"))))
 '(trailing-whitespace ((t (:background "#ffaa00"))))
 '(font-lock-builtin-face ((t (:foreground "#ffffff"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "gray38"))))
 '(font-lock-comment-face ((t (:foreground "gray38"))))
 '(font-lock-constant-face ((t (:foreground "#8fe1c8"))))
 '(font-lock-doc-face ((t (:inherit (font-lock-string-face)))))
 '(font-lock-function-name-face ((t (:foreground "#d4d4d4"))))
 '(font-lock-keyword-face ((t (:weight normal :foreground "#ffffff"))))
 '(font-lock-negation-char-face ((t nil)))
 '(font-lock-preprocessor-face ((t (:foreground "#86E08F"))))
 '(font-lock-regexp-grouping-backslash ((t (:inherit (bold)))))
 '(font-lock-regexp-grouping-construct ((t (:inherit (bold)))))
 '(font-lock-string-face ((t (:foreground "#2ec09c"))))
 '(font-lock-type-face ((t (:foreground "#86E08F"))))
 '(font-lock-variable-name-face ((t (:foreground "#d4d4d4"))))
 '(font-lock-warning-face ((t (:weight bold :foreground "#ffaa00"))))
 '(button ((t (:inherit (link)))))
 '(link ((t (:underline (:color foreground-color :style line) :foreground "#83A598"))))
 '(link-visited ((t (:underline nil :foreground "#83A598"))))
 '(fringe ((t (:background "#gray14"))))
 '(header-line ((t (:weight normal :foreground "#61ACBB" :background "#282828"))))
 '(tooltip ((((class color)) (:inherit (variable-pitch) :foreground "black" :background "lightyellow")) (t (:inherit (variable-pitch)))))
 '(mode-line ((t (:background "#d2b58d" :foreground "black" :box nil))))
 '(mode-line-buffer-id ((t (:weight bold))))
 '(mode-line-emphasis ((t (:weight bold))))
 '(mode-line-highlight ((((class color) (min-colors 88)) (:box (:line-width 2 :color "grey40" :style released-button))) (t (:inherit (highlight)))))
 '(mode-line-inactive ((t (:box nil :foreground "black" :background "dark gray"))))
 '(isearch ((t (:foreground "#FDF4C1" :background "#427B58"))))
 '(isearch-fail ((t (:foreground "#FFFFC8" :background "#9D0006"))))
 '(lazy-highlight ((t (:foreground "#FDF4C1" :background "#504945"))))
 '(match ((t (:foreground "#FDF4C1" :background "#504945"))))
 '(next-error ((t (:inherit (region)))))
 '(query-replace ((t (:inherit (isearch))))))

(provide-theme 'banal)
