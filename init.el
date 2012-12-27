
;;;;
;;;;   Some default config changes
;;;;

(setq inhibit-startup-message t)
(show-paren-mode t)
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(column-number-mode t)
(global-auto-revert-mode t)

; "fuck tabs, use spaces" - dan
(setq indent-tabs-mode nil)


;;;;
;;;;   Mu4e
;;;;

(add-to-list 'load-path "~/.emacs.d/lib/mu4e")

(require 'mu4e)
(require 'org-mu4e)

; fix weird html2text is out of range error 'error in process filter: Args out of range: "Email\"", 7, 6'
; see: https://github.com/djcb/mu/issues/73
(setq mu4e-html2text-command "html2text -utf8 -width 72")
;(setq mu4e-view-prefer-html t)              ;; prefer html

(setq
 mu4e-use-fancy-chars t
 mu4e-get-mail-command "offlineimap"   ;; or fetchmail, or ...
 mu4e-update-interval 180)              ;; update every 3 minutes


;; these are actually the defaults
(setq
 mu4e-maildir       "~/mail"   ;; top-level Maildir
; mu4e-sent-folder   "/sent"       ;; folder for sent messages
; mu4e-drafts-folder "/drafts"     ;; unfinished messages
; mu4e-trash-folder  "/trash"      ;; trashed messages
; mu4e-refile-folder "/archive"   ;; saved messages
)


;; stuff from the internet,  yay!

(setq mu4e-account-alist 
      '(("burstmarketing" 
         (mu4e-sent-folder "/burstmarketing/Sent Items") 
         (mu4e-drafts-folder "/burstmarketing/Drafts") 
	 (mu4e-trash-folder "/burstmarketing/Trash")
         (user-mail-address "ckotfila@burstmarketing.net") 
	 (smtpmail-smtp-user "chris@intellisites.com")
         (smtpmail-smtp-server "mail.thoughtbus.com")
         ;; add other variables here 
         ) 
        ("gmail" 
         (mu4e-sent-folder "/gmail/[Gmail].Sent") 
         (mu4e-drafts-folder "/gmail/[Gmail].Draft") 
	 (mu4e-trash-folder "/gmail/[Gmail].Trash")
         (user-mail-address "kotfic@gmail.com") 
	 (smtpmail-smtp-user "kotfic@gmail.com")
         (smtpmail-smtp-server "smtp.gmail.com")
	 (mu4e-sent-messages-behavior delete)
         ;; add other variables here 
         ) 
        ("ualbany" 
         (mu4e-sent-folder "/ualbany/Sent Items") 
         (mu4e-drafts-folder "/ualbany/Drafts") 
	 (mu4e-trash-folder "/ualbany/Trash")
         (user-mail-address "ckotfila@albany.edu") 
	 (smtpmail-smtp-user "ckotfila@albany.edu")
;         (smtpmail-local-domain "pod51009.outlook.com")
         (smtpmail-smtp-server "pod51009.outlook.com")
         ;; add other variables here 
         )))

(defun mu4e-set-account () 
  "Set the account for composing a message." 
  (let* ((account 
          (if mu4e-compose-parent-message 
              (let ((maildir (mu4e-msg-field mu4e-compose-parent-message :maildir))) 
                (string-match "/\\(.*?\\)/" maildir) 
                (match-string 1 maildir)) 
            (completing-read (format "Compose with account: (%s) " 
                                     (mapconcat #'(lambda (var) (car var)) mu4e-account-alist "/")) 
                             (mapcar #'(lambda (var) (car var)) mu4e-account-alist) 
                             nil t nil nil (caar mu4e-account-alist)))) 
         (account-vars (cdr (assoc account mu4e-account-alist)))) 
    (if account-vars 
        (mapc #'(lambda (var) 
                  (set (car var) (cadr var))) 
              account-vars))))

(add-hook 'mu4e-compose-pre-hook 'mu4e-set-account) 



;; don't save messages to Sent Messages, Gmail/IMAP takes care of this
; (setq mu4e-sent-messages-behavior 'delete)

;;;;
;;;;    Fullscreen
;;;;
(defun fullscreen ()
       (interactive)
       (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                 '(2 "_NET_WM_STATE_FULLSCREEN" 0)))


(global-set-key [f11] 'fullscreen)
(global-set-key [XF86Save] 'fullscreen)

;;;;
;;;;    Tramp
;;;;

(require 'tramp)
(setq tramp-default-method "ssh")


;;;;
;;;;    Chromium support
;;;;
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "chromium-browser")
;;;;
;;;;    Magit
;;;;

(add-to-list 'load-path "~/.emacs.d/lib/magit")
(require 'magit)

;;;;
;;;;   Pandoc-Mode
;;;;

(add-to-list 'load-path "~/.emacs.d/lib/pandoc")
(require 'pandoc-mode)

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
(require 'package)
(package-initialize)
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
    (erc :server "96.126.106.68" :port 6667 :nick "kotfic"
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
;(setq browse-url-browser-function 'w3m-browse-url)


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
 '(doc-view-continuous t)
 '(doc-view-ghostscript-options (quote ("-dSAFER" "-dNOPAUSE" "-sDEVICE=png16m" "-dTextAlphaBits=4" "-dBATCH" "-dGraphicsAlphaBits=4" "-dQUIET" "-dNOPLATFONTS")))
 '(doc-view-resolution 300)
 '(geben-dbgp-feature-list (quote ((:set max_data 32768) (:set max_depth 2) (:set max_children 32) (:get breakpoint_types geben-dbgp-breakpoint-store-types))))
 '(org-file-apps (quote ((auto-mode . emacs) ("\\.mm\\'" . default) ("\\.x?html?\\'" . default) ("\\.pdf\\'" . "/usr/bin/evince"))))
 '(py-shell-name "ipython")
 '(send-mail-function (quote smtpmail-send-it)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(put 'scroll-left 'disabled nil)
(fullscreen)
