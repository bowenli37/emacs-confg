;;; init.el --- Where all the magic begins
;;
;; This file loads Org-mode and then loads the rest
;; of our Emacs initialization from Emacs lisp embedded
;; in literate Org-mode files.

(setq BASEDIR (file-name-directory (or (buffer-file-name) load-file-name)))
(add-to-list 'load-path (concat BASEDIR "lib/org/lisp/"))
(add-to-list 'load-path (concat BASEDIR "lib/org/contrib/lisp/"))

;; load up Org-mode and Org-babel
(require 'org)
(require 'ob-tangle)

(org-babel-load-file (concat BASEDIR "emacs.org"))
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(background-color "#202020")
 '(background-mode dark)
 '(cursor-color "#cccccc")
 '(custom-safe-themes (quote ("f41fd682a3cd1e16796068a2ca96e82cfd274e58b978156da0acce4d56f2b0d5" "978ff9496928cc94639cb1084004bf64235c5c7fb0cfbcc38a3871eb95fa88f6" "41b6698b5f9ab241ad6c30aea8c9f53d539e23ad4e3963abff4b57c0f8bf6730" "e53cc4144192bb4e4ed10a3fa3e7442cae4c3d231df8822f6c02f1220a0d259a" "405fda54905200f202dd2e6ccbf94c1b7cc1312671894bc8eca7e6ec9e8a41a2" "ae8d0f1f36460f3705b583970188e4fbb145805b7accce0adb41031d99bd2580" "51bea7765ddaee2aac2983fac8099ec7d62dff47b708aa3595ad29899e9e9e44" "9bac44c2b4dfbb723906b8c491ec06801feb57aa60448d047dbfdbd1a8650897" "aadee996d4f28625cea5a20bdfd8a10c2be41b09e7a9e864a5bf76ae19cf3ce3" "88d556f828e4ec17ac074077ef9dcaa36a59dccbaa6f2de553d6528b4df79cbd" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "6cfe5b2f818c7b52723f3e121d1157cf9d95ed8923dbc1b47f392da80ef7495d" default)))
 '(doc-view-resolution 300)
 '(elpy-default-minor-modes (quote (eldoc-mode highlight-indentation-mode yas-minor-mode auto-complete-mode)))
 '(fci-rule-color "#383838")
 '(foreground-color "#cccccc")
 '(grep-command "grep -rin -e ")
 '(ido-default-buffer-method (quote selected-window))
 '(inferior-STA-program-name "/usr/local/stata13/stata")
 '(org-export-backends (quote (ascii html icalendar latex md odt)))
 '(org-format-latex-options (quote (:foreground default :background default :scale 1.4 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers ("begin" "$1" "$" "$$" "\\(" "\\["))))
 '(org-id-link-to-org-use-id t)
 '(safe-local-variable-values (quote ((python-shell-interpreter . "ipython") (python-shell-virtualenv-path . "~/.envs/nltk/") (org-clock-into-drawer . t))))
 '(smtpmail-stream-type nil)
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map (quote ((20 . "#BC8383") (40 . "#CC9393") (60 . "#DFAF8F") (80 . "#D0BF8F") (100 . "#E0CF9F") (120 . "#F0DFAF") (140 . "#5F7F5F") (160 . "#7F9F7F") (180 . "#8FB28F") (200 . "#9FC59F") (220 . "#AFD8AF") (240 . "#BFEBBF") (260 . "#93E0E3") (280 . "#6CA0A3") (300 . "#7CB8BB") (320 . "#8CD0D3") (340 . "#94BFF3") (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(erc-input-face ((t (:foreground "light gray"))) t)
 '(erc-my-nick-face ((t (:foreground "light gray" :weight bold))) t)
 '(highlight-indentation-face ((t (:inherit org-hide))))
 '(jabber-roster-user-away ((t (:foreground "cornflower blue" :slant italic :weight normal))))
 '(jabber-roster-user-online ((t (:foreground "deep sky blue" :slant normal :weight bold))))
 '(mu4e-header-highlight-face ((t (:inherit default :underline t :weight bold))))
 '(term-color-blue ((t (:background "deep sky blue" :foreground "deep sky blue"))) t)
 '(yas-field-highlight-face ((t (:inherit (quote region))))))
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
