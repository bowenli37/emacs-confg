(require 'ob)
(eval-when-compile (require 'cl))


(declare-function orgtbl-to-tsv "org-table" (table params))
(declare-function stata "ext:essd-stata" (&optional start-args))
(declare-function inferior-ess-send-input "ext:ess-inf" ())
(declare-function ess-make-buffer-current "ext:ess-inf" ())
(declare-function ess-eval-buffer "ext:ess-inf" (vis))
(declare-function org-number-sequence "org-compat" (from &optional to inc))
(declare-function org-remove-if-not "org" (predicate seq))


(defvar org-babel-default-header-args:stata '())

(defun org-babel-execute:stata (body params)
  "Execute a block of CSS code.
This function is called by `org-babel-execute-src-block'."
  (let* ((session (org-babel-stata-initiate-session 
                   (cdr (assoc :session params)) params))
         (result (org-babel-stata-evaluate session body))
         )
  result))

;; get a session if it exists or initiate it,  but also handle any variables
(defun org-babel-prep-session:stata (session params)
  "Return an error if the :session header argument is set.
CSS does not support sessions."
  "Prepare SESSION according to the header arguments specified in PARAMS."
  (let* ((session (org-babel-stata-initiate-session session params))
         (var-lines (org-babel-variable-assignments:stata params)))
    (org-babel-comint-in-buffer session
      (mapc (lambda (var)
              (end-of-line 1) (insert var) (comint-send-input nil t)
              (org-babel-comint-wait-for-output session)) var-lines))
    session))


(defun org-babel-variable-assignments:stata (params)
"Currently not implemented"
nil)


(defvar org-babel-stata-eoe-indicator "display \"org_babel_stata_eoe\"")
(defvar org-babel-stata-eoe-output "org_babel_stata_eoe")


(defun org-babel-stata-evaluate-session (session body)
(mapconcat
#'org-babel-trim
(butlast
  (org-babel-comint-with-output (session org-babel-stata-eoe-output t body)
    (insert (mapconcat #'org-babel-chomp
                       (list body org-babel-stata-eoe-indicator)
                       "\n"))
    (inferior-ess-send-input)) 2) "\n"))

(defun org-babel-stata-evaluate
  (session body)
  "Evaluate stata code in BODY."
  (if session
      (org-babel-stata-evaluate-session session body)
    (org-babel-stata-evaluate-external-process body )))


(defun org-babel-stata-evaluate-external-process (body)
"Stub function until we can get a better way to run this"
  (error "Must be run with session"))

(defvar ess-ask-for-ess-directory) ; dynamically scoped
;; session - string containing a session name
;; params - assoc list that can currently contain :dir param
(defun org-babel-stata-initiate-session (session params)
  "If there is not a current R process then create one."
  (unless (string= session "none")
    (let ((session (or session "*stata*"))
          (ess-ask-for-ess-directory
           (and (and (boundp 'ess-ask-for-ess-directory) ess-ask-for-ess-directory)
                (not (cdr (assoc :dir params))))))
      (if (org-babel-comint-buffer-livep session)
          (org-babel-comint-buffer-livep session)
        (save-window-excursion
          (when (get-buffer session)
            ;; Session buffer exists, but with dead process
            (set-buffer session))
          (require 'ess) (stata)
          (rename-buffer
           (if (bufferp session)
               (buffer-name session)
             (if (stringp session)
                 session
               (buffer-name))))
          (current-buffer))))))


(provide 'ob-stata)
