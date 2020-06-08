(defalias 'eb 'eval-buffer)

(defalias 'yes-or-no-p 'y-or-n-p)

(defun split-and-follow-horizontally ()
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)

(defun split-and-follow-vertically ()
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 3") 'split-and-follow-vertically)

(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

(global-set-key (kbd "<backtab>") 'un-indent-by-removing-4-spaces)

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
 

(global-set-key (kbd "M-w") 'xah-open-file-fast);bookmark lookup

(global-set-key (kbd "C-p") 'find-file-other-window);open file in new window vertical split

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


(defalias 'cfp 'xah-copy-file-path)

(defun xah-copy-file-path (&optional @dir-path-only-p)
  "Copy the current buffer's file path or dired path to `kill-ring'.
Result is full path.
If `universal-argument' is called first, copy only the dir path.

If in dired, copy the file/dir cursor is on, or marked files.

If a buffer is not file and not dired, copy value of `default-directory' (which is usually the “current” dir when that buffer was created)

URL `http://ergoemacs.org/emacs/emacs_copy_file_path.html'
Version 2017-09-01"
  (interactive "P")
  (let (($fpath
         (if (string-equal major-mode 'dired-mode)
             (progn
               (let (($result (mapconcat 'identity (dired-get-marked-files) "\n")))
                 (if (equal (length $result) 0)
                     (progn default-directory )
                   (progn $result))))
           (if (buffer-file-name)
               (buffer-file-name)
             (expand-file-name default-directory)))))
    (kill-new
     (if @dir-path-only-p
         (progn
           (message "Directory path copied: 「%s」" (file-name-directory $fpath))
           (file-name-directory $fpath))
       (progn
         (message "File path copied: 「%s」" $fpath)
         $fpath )))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#3c3836" "#fb4933" "#b8bb26" "#fabd2f" "#83a598" "#d3869b" "#8ec07c" "#ebdbb2"])
 '(ansi-term-color-vector
   [unspecified "#2d2a2e" "#ff6188" "#a9dc76" "#ffd866" "#78dce8" "#ab9df2" "#ff6188" "#fcfcfa"] t)
 '(blink-cursor-mode nil)
 '(cua-mode t nil (cua-base))
 '(custom-safe-themes
   (quote
    ("d001dd1ee11cabb1a93a09cde742c562b139a324d40084fb0b2844d91d3c5e16" "ebbf0454c5df5d4b0baecd61a8ad2468a6c9d58b28a599f6a80a7470ec3ecb5d" "c7b58d4760bfb481667296a140aa4504daf39c685b2dca4d70900028495a0a72" "eac03b5ef51f3c6acc7eb3ff1f379f21477db2d930b4fbaf0b63a56b509f871e" "bc6477865f39c55882c3ee70fdf96042f3adf80b85ce6ebaf19e15949ea1b5eb" "214fdb2a8c4b9b7875181c3624db47bc271237343aa32a60e379548db6651db7" "b7ae72ff309e80c508cf696cfe5e13c72294aa104e3ffb3818c5fbb01ce4a536" "224ad59f2e4c01fb96801db6fab02cd94a9aa71a8d92a6c9c32ca2d240b7abb3" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "cabe783ea5fd284781fd667a2f3e1ea8ab05089d3b896b40431e1d53718b9229" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "9025bbdbb434bfdca93d6a2e3c4b3fddd80e0b9392c933d151cb839b8c03b9be" "7f6d4aebcc44c264a64e714c3d9d1e903284305fd7e319e7cb73345a9994f5ef" "9dcbb444903c772af300a1a5e76c2dfdb0721f945aa03a679b63fec833793ce1" "918f1f99d01e6c36297f98d6a4f00874bfe79c9606aab4b813340464881448c7" "dd7aa9e161e9db521da41356a547cb7359ce1fbb879f719b6c6c84275a65b319" "badc1885914fe60a015017d8a7b80f8bbd0aeb0291992013d2d1a623a8ff94b3" "6515fcc302292f29a94f6ac0c5795c57a396127d5ea31f37fc5f9f0308bbe19f" "5a45c8bf60607dfa077b3e23edfb8df0f37c4759356682adf7ab762ba6b10600" "de1f10725856538a8c373b3a314d41b450b8eba21d653c4a4498d52bb801ecd2" "30289fa8d502f71a392f40a0941a83842152a68c54ad69e0638ef52f04777a4c" "47a7d07b2e928571c1620ce0c5a116ea2ba67c34e66b88d3fced3cffacce2c5f" "16dc67129cc4ce81dbfd0973e79c112af6aa2579f426ffb2f332542179ecda5d" "01d01a4734658f397869735b7da6a42adb4b9ac422c0c840ebab846202cdc457" "b4619e719bc05fef10530b6779ca22852e3e6fd4de13e4ef03fc2009b11eb8bd" "a085ea064d1a327b489c44e9f0e3675d9ed82a9033ba21465a79c70ce914c9cb" "a838e7e363467126036b477a6d91a6782d0bfe85f73a79ffb1bfa2aa32ece66e" "aded61687237d1dff6325edb492bde536f40b048eab7246c61d5c6643c696b7f" "939ea070fb0141cd035608b2baabc4bd50d8ecc86af8528df9d41f4d83664c6a" "4cf9ed30ea575fb0ca3cff6ef34b1b87192965245776afa9e9e20c17d115f3fb" "e9f26be79399d0e787a8ecf3c9f0ec5840083e9f712393b1378657c9ce55b2e7" "0c5197f4d62ab38b172dd47cae9ac9be374a18c94611d33f12ebf74734680474" "88049c35e4a6cedd4437ff6b093230b687d8a1fb65408ef17bfcf9b7338734f6" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "c5ad91387427abc66af38b8d6ea74cade4e3734129cbcb0c34cc90985d06dcb3" "2d835b43e2614762893dc40cbf220482d617d3d4e2c35f7100ca697f1a388a0e" "d2799255a136de4d77456f04a6d0061cc4136d50f7ce71810c3a9dfdba6c4bf6" "7d72470d1d354eb6b1cded54329c24433ea23cee847b29925fbd4b1a77e4af28" "ee3d0c76472b1117c4fd807a723576075f7e197edc0a3c7200a5bead5693fbb7" "f583772fe1039a8c514b2a4bf2f4c0c2898d94c175f9513dc011e073d1e90a29" "c1f77730dc4cb08493d8c71d67bf0bf30847b813b310b64436c46efa615fb74b" "b28d95330b708eb7bcf7f135fcc107a38423bc02731652904bed2a653c89a54e" "29a9a879ce4b0a8922d9337badd58c896d6f7b9c8a22eadab71411079f4061e2" "1a1cdd9b407ceb299b73e4afd1b63d01bbf2e056ec47a9d95901f4198a0d2428" "51ec7bfa54adf5fff5d466248ea6431097f5a18224788d0bd7eb1257a4f7b773" "c433c87bd4b64b8ba9890e8ed64597ea0f8eb0396f4c9a9e01bd20a04d15d358" "7f1d414afda803f3244c6fb4c2c64bea44dac040ed3731ec9d75275b9e831fe5" "13a8eaddb003fd0d561096e11e1a91b029d3c9d64554f8e897b2513dbf14b277" "285d1bf306091644fb49993341e0ad8bafe57130d9981b680c1dbd974475c5c7" "2809bcb77ad21312897b541134981282dc455ccd7c14d74cc333b6e549b824f3" "00445e6f15d31e9afaa23ed0d765850e9cd5e929be5e8e63b114a3346236c44c" "0fffa9669425ff140ff2ae8568c7719705ef33b7a927a0ba7c5e2ffcfac09b75" "830877f4aab227556548dc0a28bf395d0abe0e3a0ab95455731c9ea5ab5fe4e1" "751730a5477ae505e340aff89b9c4df5f1db585befda4851ce2c93206e6f5824" "f6edc44377068eed532f773c2ff7856535aeee844ac9cf35bd9f7575e8cbb6d1" "852d20944ca8f179050da915912041c12b7a9ad0fbea04ef9c2e5bbfdd510d87" "5e5e8cf51f9915866c096e0e68d5fd40a3c85e4ce8aa63aac5f59301ed417250" "914f30fdd2e9ea51f6307f1f6218cb7824b2f3b512564bfa5ab16ebdefa1ec5e" "97e39dddc0a96fdac7567ab58b22304fd94f60a93f4e932eaa5efb3c2af7e159" "39546362fed4d5201b2b386dc21f21439497c9eec5fee323d953b3e230e4083e" "d74fe1508cff43708fa2f97c4bf58d19f0e002b2e0c92bf958bf483113b7d89d" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "09cadcc2784baa744c6a7c5ebf2a30df59c275414768b0719b800cabd8d1b842" "1caa3499292e3bcc438bfc3d2983087b5970b987434480ae08549459d4a2b126" "b374cf418400fd9a34775d3ce66db6ee0fb1f9ab8e13682db5c9016146196e9c" default)))
 '(global-command-log-mode t)
 '(line-number-mode nil)
 '(linum-format " %5i ")
 '(package-selected-packages
   (quote
    (caml command-log-mode command-log yasnippet-snippets nord-theme moe-theme plan9-theme solarized-theme gruvbox-theme darktooth-theme naysayer-theme dashboard xah-fly-keys which-key use-package try)))
 '(pdf-view-midnight-colors (quote ("#fdf4c1" . "#282828")))
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Roboto Mono" :foundry "outline" :slant normal :weight normal :height 120 :width normal))))
 '(sml/charging ((t (:inherit sml/global :foreground "black"))))
 '(sml/client ((t nil))))
