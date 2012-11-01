
;;;;
;;;;   Some default config changes
;;;;

(setq inhibit-startup-message t)
(show-paren-mode t)
(menu-bar-mode -1)
(column-number-mode t)

(setq fill-column 120)

;;;;
;;;;    Modeline
;;;;

; (set-face-background 'mode-line nil)
; (set-face-background 'mode-line-inactive nil)


; (set-face-foreground 'mode-line "#ffff87")
; (set-face-foreground 'mode-line-inactive "#ffff87")



;;;;
;;;;    Magit
;;;;

(add-to-list 'load-path "~/.emacs.d/lib/magit")
(require 'magit)

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

; log when we mark a todo as DONE
(setq org-log-done 'time)

(if (file-exists-p (expand-file-name "~/classes/.agenda-files"))
    (setq org-agenda-files "~/classes/.agenda-files" ))

;;;;
;;;;  Uniqueify
;;;;

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

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


(add-hook 'w3m-mode-hook
	  (lambda ()
	    (local-set-key "\C-n" 'w3m-next-anchor)
	    (local-set-key "\C-p" 'w3m-previous-anchor)
	    (local-set-key '[up] 'previous-line)
	    (local-set-key '[down] 'next-line)
	    (local-set-key '[right] 'forward-char)
	    (local-set-key '[left] 'backward-char)))

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
;;;;   defcustoms
;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#2e3436" "#a40000" "#4e9a06" "#c4a000" "#204a87" "#5c3566" "#729fcf" "#eeeeec"])
 '(custom-enabled-themes (quote (wombat)))
 '(geben-dbgp-feature-list (quote ((:set max_data 32768) (:set max_depth 2) (:set max_children 32) (:get breakpoint_types geben-dbgp-breakpoint-store-types))))
 '(org-file-apps (quote ((auto-mode . emacs) ("\\.mm\\'" . default) ("\\.x?html?\\'" . default) ("\\.pdf\\'" . "/usr/bin/evince"))))
 '(py-shell-name "ipython"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(put 'scroll-left 'disabled nil)
