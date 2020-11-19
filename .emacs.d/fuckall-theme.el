(deftheme fuckall
  "Created 2020-11-19.")

(custom-theme-set-faces
 'fuckall
 '(default ((t (:family "Inconsolata" :foundry "outline" :width normal :height 128 :weight normal :slant normal :underline nil :overline nil :strike-through nil :box nil :inverse-video nil :foreground "#bdbdb3" :background "gray15" :stipple nil :inherit nil))))
 '(cursor ((t (:foreground "gray15" :background "#afffef"))))
 '(fixed-pitch ((t (:family "Monospace"))))
 '(variable-pitch ((((type w32)) (:foundry "outline" :family "Arial")) (t (:family "Sans Serif"))))
 '(escape-glyph ((((background dark)) (:foreground "cyan")) (((type pc)) (:foreground "magenta")) (t (:foreground "brown"))))
 '(homoglyph ((((background dark)) (:foreground "cyan")) (((type pc)) (:foreground "magenta")) (t (:foreground "brown"))))
 '(minibuffer-prompt ((t (:weight bold :foreground "#caca86"))))
 '(highlight ((((class color) (min-colors 88) (background light)) (:background "gainsboro")) (((class color) (min-colors 88) (background dark)) (:background "darkolivegreen")) (((class color) (min-colors 16) (background light)) (:background "gainsboro")) (((class color) (min-colors 16) (background dark)) (:background "darkolivegreen")) (((class color) (min-colors 8)) (:foreground "black" :background "green")) (t (:inverse-video t))))
 '(region ((t (:background "#343030"))))
 '(shadow ((((class color grayscale) (min-colors 88) (background light)) (:foreground "grey50")) (((class color grayscale) (min-colors 88) (background dark)) (:foreground "grey70")) (((class color) (min-colors 8) (background light)) (:foreground "green")) (((class color) (min-colors 8) (background dark)) (:foreground "yellow"))))
 '(secondary-selection ((((class color) (min-colors 88) (background light)) (:background "yellow1")) (((class color) (min-colors 88) (background dark)) (:background "SkyBlue4")) (((class color) (min-colors 16) (background light)) (:background "yellow")) (((class color) (min-colors 16) (background dark)) (:background "SkyBlue4")) (((class color) (min-colors 8)) (:foreground "black" :background "cyan")) (t (:inverse-video t))))
 '(trailing-whitespace ((t (:weight bold :background "white"))))
 '(font-lock-builtin-face ((t (:foreground "#9fbfdf"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "#706565"))))
 '(font-lock-comment-face ((t (:foreground "#857575"))))
 '(font-lock-constant-face ((t (:foreground "#bdbdb3"))))
 '(font-lock-doc-face ((t (:foreground "#7c7565"))))
 '(font-lock-function-name-face ((t (:foreground "#bdbdb3"))))
 '(font-lock-keyword-face ((t (:foreground "#91a0b3"))))
 '(font-lock-negation-char-face ((t nil)))
 '(font-lock-preprocessor-face ((t (:foreground "#E96245"))))
 '(font-lock-regexp-grouping-backslash ((t (:inherit (bold)))))
 '(font-lock-regexp-grouping-construct ((t (:inherit (bold)))))
 '(font-lock-string-face ((t (:foreground "#a9df90"))))
 '(font-lock-type-face ((t (:foreground "#bdbdb3"))))
 '(font-lock-variable-name-face ((t (:foreground "#bdbdb3"))))
 '(font-lock-warning-face ((t (:weight bold :foreground "red"))))
 '(button ((t (:underline (:color foreground-color :style line) :foreground "#afcfef"))))
 '(link ((t (:underline (:color foreground-color :style line) :foreground "#afcfef"))))
 '(link-visited ((default (:inherit (link))) (((class color) (background light)) (:foreground "magenta4")) (((class color) (background dark)) (:foreground "violet"))))
 '(fringe ((t (:background "#262424"))))
 '(header-line ((t (:foreground "gray15" :background "#bdbdb3"))))
 '(tooltip ((t (:foreground "systeminfotext" :background "systeminfowindow" :inherit (variable-pitch)))))
 '(mode-line ((t (:foreground "#302525" :background "cornsilk4"))))
 '(mode-line-buffer-id ((t (:weight bold))))
 '(mode-line-emphasis ((t (:weight bold))))
 '(mode-line-highlight ((((class color) (min-colors 88)) (:box (:line-width 2 :color "grey40" :style released-button))) (t (:inherit (highlight)))))
 '(mode-line-inactive ((t (:foreground "cornsilk4" :background "#504545"))))
 '(isearch ((t (:foreground "gray15" :background "#91a0b3"))))
 '(isearch-fail ((((class color) (min-colors 88) (background light)) (:background "RosyBrown1")) (((class color) (min-colors 88) (background dark)) (:background "red4")) (((class color) (min-colors 16)) (:background "red")) (((class color) (min-colors 8)) (:background "red")) (((class color grayscale)) (:foreground "grey")) (t (:inverse-video t))))
 '(lazy-highlight ((t (:underline (:color foreground-color :style line) :foreground "#ab85a3" :background "gray15"))))
 '(match ((((class color) (min-colors 88) (background light)) (:background "yellow1")) (((class color) (min-colors 88) (background dark)) (:background "RoyalBlue3")) (((class color) (min-colors 8) (background light)) (:foreground "black" :background "yellow")) (((class color) (min-colors 8) (background dark)) (:foreground "white" :background "blue")) (((type tty) (class mono)) (:inverse-video t)) (t (:background "gray"))))
 '(next-error ((t (:inherit (region)))))
 '(query-replace ((t (:inherit (isearch))))))

(provide-theme 'fuckall)
