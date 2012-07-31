;;;;
;;;;   Some default config changes
;;;;

(setq inhibit-startup-message t)
(show-paren-mode t)
(menu-bar-mode -1)
(column-number-mode t)

;;;;
;;;;   Geben support
;;;;

(add-to-list 'load-path "~/.emacs.d/lib/geben-0.26/")
(autoload 'geben "geben" "PHP Debugger on Emacs" t)


;;;;
;;;;    Helm Support
;;;;
(add-to-list 'load-path "~/.emacs.d/lib/helm/")

(require 'helm-config)
(global-set-key (kbd "C-c h") 'helm-mini)

;(global-set-key (kbd "C-x C-f") 'helm-find-files)


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
;;;;   Python Support
;;;;
(add-to-list 'load-path "~/.emacs.d/lib/python/")

(autoload 'python-mode "python-mode" "Load Python Mode 6.0.8" t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

(add-hook 'python-mode-hook
	  (lambda () 
		      ;; iPython not currently supported by iPython project.
		      ;; (setq ipython-command "ipython")                                                         
		      ;; (require 'ipython)
		      
		      ;; fixes bug with python-mode/ipython variable name incompatability 
		      ;; see: https://bugs.launchpad.net/python-mode/+bug/912919
		      ;(setq py-mode-map python-mode-map)
		      


		      (require 'lambda-mode)
		      (add-hook 'python-mode-hook (lambda () (lambda-mode 1) ) )
		      (setq lambda-symbol (string (make-char 'greek-iso8859-7 107)))
		      
		      
		      (require 'comint)
		      (define-key comint-mode-map (kbd "M-") 'comint-next-input)
		      (define-key comint-mode-map (kbd "M-") 'comint-previous-input)
		      (define-key comint-mode-map [down] 'comint-next-matching-input-from-input)
		      (define-key comint-mode-map [up] 'comint-previous-matching-input-from-input)
		      
		      
		      (require 'pylookup)
		      (autoload 'pylookup-lookup "pylookup")
		      (autoload 'pylookup-update "pylookup")
		      (setq pylookup-program "~/.emacs.d/lib/python/pylookup/pylookup.py")
		      (setq pylookup-db-file "~/.emacs.d/lib/python/docs/pylookup.db")
					;
		      (global-set-key "\C-cd" 'pylookup-lookup)
))	  
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
 '(geben-dbgp-feature-list (quote ((:set max_data 32768) (:set max_depth 2) (:set max_children 32) (:get breakpoint_types geben-dbgp-breakpoint-store-types))))
 '(py-shell-name "ipython"))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
