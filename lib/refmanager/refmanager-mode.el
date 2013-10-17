;;; refmanager-mode.el --- A minor mode for using emacs org-mode as a reference manager
;; Copyright (C) 2013 Christopher Kotfila

;; Author: Christopher Kotfila <kotfic@gmail.com>
;; Keywords: emacs org-mode reference citation bibtex

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Todo

;;; Code:

(require 's)
(require 'bibtex)

(bibtex-set-dialect "biblatex")

(define-minor-mode refmanager-mode
  "Refmanager mode provides some basic functions for managing
an org-mode file of citations based on org entry properties. "
  nil
  " RefMan"
  `((,(kbd "C-c r o") . refmanager-open-entry-file)
    (,(kbd "C-c r c") . refmanager-bibtext-to-org)
    (,(kbd "<prior>") . refmanager-scroll-other-docview-window-down)
    (,(kbd "<next>") . refmanager-scroll-other-docview-window-up))

  :group 'refman)



(defun refmanager-scroll-other-docview-window-up ()
  (interactive)
  (save-excursion 
    (other-window +1)
    (if (eq major-mode 'doc-view-mode)
	(doc-view-next-line-or-next-page 1))
    (other-window -1)
    ))


(defun refmanager-scroll-other-docview-window-down ()
  (interactive)
  (save-excursion 
    (other-window +1)
    (if (eq major-mode 'doc-view-mode)
	(doc-view-previous-line-or-previous-page 1))
    (other-window -1)
    ))

;(defadvice scroll-other-window (around doc-view-scroll-up-or-next-page activate)
;  "When next buffer is `doc-view-mode', do `doc-view-scroll-up-or-next-page'."
;  (when (derived-mode-p 'refmanager-mode)
;    (other-window +1)
;    (if (eq major-mode 'doc-view-mode)
;	(let ((arg (ad-get-arg 0)))
;	  (if (null arg)
;	      (doc-view-scroll-up-or-next-page)
;	    (doc-view-next-line-or-next-page arg))
;	  (other-window -1))
;    (other-window -1)
;    ad-do-it)))
;
;(defadvice scroll-other-window-down (around doc-view-scroll-down-or-previous-page activate)
;  "When next buffer is `doc-view-mode', do `doc-view-scroll-down-or-previous-page'."
;  (when (derived-mode-p 'refmanager-mode)
;    (other-window +1)
;    (if (eq major-mode 'doc-view-mode)
;	(let ((arg (ad-get-arg 0)))
;	  (if (null arg)
;	      (doc-view-scroll-down-or-previous-page)
;	    (doc-view-previous-line-or-previous-page arg))
;	  (other-window -1))
;      (other-window -1)
;      ad-do-it)))
;  


(defun mapfilter (condp lst)
  "Filter out elements that do not meet the condp condition"
  (delq nil
          (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

(defun commafy (lst)
  "Take a list of strings and put commas on the end of each but the very last"
  (reverse (cons (car (reverse lst)) (mapcar (lambda (str) (concat str "," ) ) (cdr (reverse lst))))))


(defvar refmanager-org-template "\
** $\{author\} :: $\{title\} ($\{year\})

#+BEGIN_SRC bibtex :tangle references.bib

$\{entry\}

#+END_SRC 

*** Notes
*** Tasks [/]
*** Questions [/]

")




(defun refmanager-bibtext-generate-org-header ( &optional indent)
  (interactive)
  (save-excursion
    (let ((title (or (bibtex-text-in-field "title")  "[TITLE]"))
          (author (or (bibtex-text-in-field "author")  "[AUTHOR]"))
          (year (or (bibtex-text-in-field "year")  "[YEAR]")))
      (bibtex-beginning-of-entry)
      (previous-line)
      ;; TODO use indent to determin number of stars
      (insert (s-lex-format "** ${author} :: ${title} (${year})" ))
      (org-set-tags-to ":unread:citation:")
      )))

(defun refmanager-generate-id ()
  "Add an 'id' property to an org entity based on author/year/title properties"
  (interactive)
  (let ((author (car (s-split-words (org-entry-get (point) "author"))))
        (year (org-entry-get (point) "year"))
        ;; NOTE - this should probably gracefully handle titles that start with "A" or "The"
        (title-frag (car (s-split-words (org-entry-get (point) "title")))))
    (org-entry-put (point) "id" (format "%s%s%s" author year title-frag  ))))

(defun refmanager-bibtext-to-org ()
  "Convert a bibtex entry to an org entry with properties from the
bibtex entry. Mark the new org entry with tags 'unread' and 'citation'"
  (interactive)
  (save-excursion

    (let ((fields (bibtex-parse-entry))
	  (fmtlist `(("entry"  . ,(buffer-substring (bibtex-beginning-of-entry) (bibtex-end-of-entry)))
		     ("title"  . ,(or (bibtex-text-in-field "title")  "[TITLE]"))
		     ("author" . ,(or (bibtex-text-in-field "author")  "[AUTHOR]"))
		     ("year"  . ,(or (bibtex-text-in-field "year")  "[YEAR]")))))

      
      ;; remove the bibtex entry
      (save-excursion
	(delete-region (bibtex-beginning-of-entry) (bibtex-end-of-entry))
	(insert (s-format refmanager-org-template 'aget fmtlist )))

      (org-set-tags-to ":unread:citation:")      
      (org-insert-property-drawer)

      (while fields
        (let ((key (car (car fields)))
              (value (s-with (cdr (car fields)) (s-chop-prefix "{") (s-chop-suffix "}"))))
          (cond ((s-equals? key "=type=")
                 (org-entry-put (point) "type" value))
                ((s-equals? key "=key=")
                 (org-entry-put (point) "bibtex_id" value))
                (t
                 (org-entry-put (point) key value)))
        (setq fields (cdr fields))))
      (org-entry-put (point) "file" "")
      (org-entry-put (point) "KEYWORDS" ""))))


;; TODO - Write functions that allow you to see a set of all keywords in a seperate buffer
;;        Make it so you can select those keywords and add them to an org entry "KEYWORDS" property
;;        Allow you to create a new Keyword on the fly

(defun refmanager-list-keywords ()
  "return the set of current keywords"
  (remove-duplicates
   (apply 'append
          (org-map-entries (lambda () (org-entry-get-multivalued-property (point) "KEYWORDS")) "citation"))
   :test (lambda (s1 s2) (s-equals? s1 s2))))


(defun refmanager-open-entry-file ()
  (interactive)
  (let ((f (org-entry-get (point) "file")))
         (if (stringp f)
             (find-file-other-window f)
           (message (format "Unable to locate file: %s" f)))))




;; FROM - org-sparse-tree
;;       (setq kwd (org-icompleting-read "Property: "
;;                               (mapcar 'list (org-buffer-property-keys))))
;;    (setq value (org-icompleting-read "Value: "
;;                                 (mapcar 'list (org-property-values kwd))))
;;    (unless (string-match "\\`{.*}\\'" value)
;;      (setq value (concat "\"" value "\"")))
;;    (org-match-sparse-tree arg (concat kwd "=" value)))



(defun refmanager-test ()
  (interactive "P")
  (setq kwd "KEYWORDS")
  (setq value (org-icompleting-read "Value: "
                                    (mapcar 'list (org-property-values kwd))))

  (let ((matcher '(and
                   (member "citation" (org-get-tags-at (point)))
                   (string-match value (org-entry-get (point) kwd))

;                   (progn
;                    (setq org-cached-props nil)
;                    (string-match
;                     (or
;                      (org-cached-entry-get nil "author")
;                      "")
;                     "Uzuner"))
                  t))
        (todo-only nil))
    (org-scan-tags 'sparse-tree matcher nil)))

(defun tmp (s) (cdr (let ((todo-only nil)) (org-make-tags-matcher s))))

(defun refmanager-entry-to-bibtex-string (pom)
  (let ((property-blacklist '("CATEGORY" "FILE" "BLOCKED" "id" "type" "KEYWORDS" "file" "TAGS" "ALLTAGS" "CLOCK")))
    (let ((id (org-entry-get pom "id"))
          (type (org-entry-get pom "type" t))
          (properties (mapfilter (lambda (e) (not (member (car e) property-blacklist)))
                                 (org-entry-properties pom))))
       (concat (format "@%s{%s,\n" type id)
               (apply 'concat (mapcar (lambda (str) (concat str "\n")) 
				      (commafy (mapcar (lambda (prop) (format "%s = { %s }" (car prop) (cdr prop))) properties))))
               "}")
       )))

(defun refmanager-insert-bibtext-from-properties ()
  (interactive)
  (insert (refmanager-entry-to-bibtex-string (point))))


(defun refmanager-custom-bibliography (&optional arg)
  (interactive)
  (let* ((block-counter 0)       
	 (todo-only nil)
	 (matcherstr (read-from-minibuffer "Match Prop: "))
	 (matcher (cdr (org-make-tags-matcher matcherstr)))
	 specs )
    
    (org-scan-tags 'sparse-tree matcher todo-only)
    
    (if (y-or-n-p "Continue?")
	(progn 
	  (org-map-entries 
	   (lambda () 
	     (let ((end (plist-get (cadr (org-element-at-point)) :end)))
	       (save-restriction
		 (org-next-block nil)
		 (setq specs (cons (org-babel-tangle-single-block block-counter) specs))
		  )))
	   matcherstr)
      (refmanager-tangle-specs specs)))))
		      




(defun refmanager-tangle-specs (specs)
  (let ((ext ".bib")
	(lang-f (intern "bibtex-mode"))
	(block-counter 0)
	path-collector)
    (mapc
     (lambda (spec)
       (let ((get-spec (lambda (name) (cdr (assoc name (nth 4 spec))))))
       (let* ((tangle (funcall get-spec :tangle))
	      (she-bang ((lambda (sheb) (when (> (length sheb) 0) sheb))
			 (funcall get-spec :shebang)))
	      (tangle-mode (funcall get-spec :tangle-mode))
	      (base-name (cond
			  ((string= "yes" tangle)
			   (file-name-sans-extension
			    (buffer-file-name)))
			  ((string= "no" tangle) nil)
			  ((> (length tangle) 0) tangle)))
	      (file-name (when base-name
			   ;; decide if we want to add ext to base-name
			   (if (and ext (string= "yes" tangle))
			    (concat base-name "." ext) base-name))))
	 (when file-name
	   ;; possibly create the parent directories for file
	   (when ((lambda (m) (and m (not (string= m "no"))))
		  (funcall get-spec :mkdirp))
	     (make-directory (file-name-directory file-name) 'parents))
	   ;; delete any old versions of file
	   (when (and (file-exists-p file-name)
		      (not (member file-name (mapcar #'car path-collector))))
	     (delete-file file-name))
	   ;; drop source-block to file
	   (with-temp-buffer
	     (when (fboundp lang-f) (ignore-errors (funcall lang-f)))
	     (when (and she-bang (not (member file-name she-banged)))
	       (insert (concat she-bang "\n"))
	       (setq she-banged (cons file-name she-banged)))
	     (org-babel-spec-to-string spec)
	     ;; We avoid append-to-file as it does not work with tramp.
	     (let ((content (buffer-string)))
	       (with-temp-buffer
		 (if (file-exists-p file-name)
		     (insert-file-contents file-name))
		 (goto-char (point-max))
		 ;; Handle :padlines unless first line in file
		 (unless (or (string= "no" (cdr (assoc :padline (nth 4 spec))))
			     (= (point) (point-min)))
		   (insert "\n"))
		 (insert content)
		 (write-region nil nil file-name))))
	   ;; if files contain she-bangs, then make the executable
	   (when she-bang
	     (unless tangle-mode (setq tangle-mode #o755)))
	   ;; update counter
	   (setq block-counter (+ 1 block-counter))
	   (add-to-list 'path-collector
			(cons file-name tangle-mode)
			nil
			(lambda (a b) (equal (car a) (car b))))))))
   specs)))
   
(provide 'refmanager-mode)
