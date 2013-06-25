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
 '(safe-local-variable-values (quote ((org-clock-into-drawer . t)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(jabber-roster-user-away ((t (:foreground "cornflower blue" :slant italic :weight normal))))
 '(jabber-roster-user-online ((t (:foreground "deep sky blue" :slant normal :weight bold)))))
(put 'downcase-region 'disabled nil)
