
;;;;
;;;;   Some default config changes
;;;;

(setq inhibit-startup-message t)
(show-paren-mode t)
(menu-bar-mode -1)
(column-number-mode t)


;;;;
;;;;    Org-Mode Configs
;;;;

(add-to-list 'load-path "~/.emacs.d/lib/org/")
(add-to-list 'load-path "~/.emacs.d/lib/org/contrib/lisp/")
(require 'org)

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;;;;
;;;;   IDO config
;;;;

(require 'ido)                                            
(ido-mode 'both) ; for buffers and files
(setq 
 ido-save-directory-list-file "~/.emacs.d/cache/ido.last"
 ido-ignore-buffers '("\\` " "^\*Mess" "^\*Back" ".*Completion" "^\*Ido" "^\*trace" "^\*compilation" "^\*GTAGS" "^session\.*" "^\*")
 ido-work-directory-list '("~/" "~/Desktop" "~/Documents" "~src")
 ido-case-fold  t                 ; be case-insensitive
 ido-enable-last-directory-history t ; remember last used dirs
 ido-max-work-directory-list 30   ; should be enough
 ido-max-work-file-list      50   ; remember many
 ido-use-filename-at-point nil    ; don't use filename at point (annoying)
 ido-use-url-at-point nil         ; don't use url at point (annoying)
 ido-max-prospects 10             ; don't spam my minibuffer
 ido-confirm-unique-completion nil)

					; when using ido, the confirmation is rather annoying...
(setq confirm-nonexistent-file-or-buffer nil)

(provide 'ido-config)


;;;;
;;;;   Adding ELPA Package support
;;;;
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")))

;;;;
;;;;   Custom Theme Support
;;;;

;(add-to-list 'load-path "~/.emacs.d/elpa/color-theme-20070910") 
;(add-to-list 'load-path "~/.emacs.d/elpa/color-theme-sanityinc-tomorrow-20120720") 
;(require 'color-theme-sanityinc-tomorrow-autoloads)
;
;(require 'color-theme)
;(require 'color-theme-sanityinc-tomorrow-autoloads)


;;;;
;;;;  Bitlbee support
;;;;

(add-to-list 'load-path "~/.emacs.d/lib/bitlbee/")
(require 'bitlbee)

(defun my-reformat-jabber-backlog ()
  "Fix \"unkown participant\" backlog messages from bitlbee."
  (save-excursion
    (goto-char (point-min))
    (if (looking-at
	 "^<root> Message from unknown participant \\([^:]+\\):")
	(replace-match "<\\1>"))))
(add-hook 'erc-insert-modify-hook 'my-reformat-jabber-backlog)

(defun social ()
  "Connect to fearthecloud.net with erc"
  (interactive)
  (let ((pw (read-passwd "Password:")))
    (erc :server "fearthecloud.net" :port 6667 :nick "kotfic"
	 :password pw )))

;;;;
;;;;   Geben support
;;;;

(add-to-list 'load-path "~/.emacs.d/lib/geben-0.26/")
(autoload 'geben "geben" "PHP Debugger on Emacs" t)


;;;;
;;;;    W3m support
;;;;

(add-to-list 'load-path "~/.emacs.d/lib/w3m/")
(require 'w3m-load)
(setq browse-url-browser-function 'w3m-browse-url)


;;;;
;;;;   PHP Support
;;;;

(add-to-list 'load-path "~/.emacs.d/lib/php/")
(autoload 'php-mode "php-mode" "Major mode for PHP files." t)
(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))

;;;;
;;;;   LISP/SLIME Support
;;;;


(add-to-list 'load-path "~/.emacs.d/lib/slime/")  
(require 'slime)
(slime-setup '(slime-fancy))
(setq inferior-lisp-program (executable-find "sbcl"))

;(load (expand-file-name "~/quicklisp/slime-helper.el"))

(if (file-exists-p (expand-file-name "~/quicklisp/slime-helper.el"))
    (load (expand-file-name "~/quicklisp/slime-helper.el")))


;;;;
;;;;   Python Support
;;;;
(add-to-list 'load-path "~/.emacs.d/lib/python/")

(autoload 'python-mode "python-init" "init python" t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

;;;;
;;;;   Manage Autosave
;;;;
	  
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))


;;;;
;;;;   Third party vendor defcustoms
;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector (vector "#c5c8c6" "#cc6666" "#b5bd68" "#f0c674" "#81a2be" "#b294bb" "#8abeb7" "#1d1f21"))
 '(custom-enabled-themes nil)
 '(custom-safe-themes (quote ("82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default)))
 '(fci-rule-color "#282a2e")
 '(geben-dbgp-feature-list (quote ((:set max_data 32768) (:set max_depth 2) (:set max_children 32) (:get breakpoint_types geben-dbgp-breakpoint-store-types))))
 '(py-shell-name "ipython"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


