(require 'python-mode)
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
