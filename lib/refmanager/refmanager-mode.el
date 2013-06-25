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
    (,(kbd "C-c r c") . refmanager-bibtext-to-org))

  :group 'refman)


(defun mapfilter (condp lst)
  "Filter out elements that do not meet the condp condition"
  (delq nil
          (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

(defun commafy (lst)
  "Take a list of strings and put commas on the end of each but the very last"
  (reverse (cons (car (reverse lst)) (mapcar (lambda (str) (concat str "," ) ) (cdr (reverse lst))))))

(defun refmanager-entry-to-bibtex-string (pom)
  (let ((property-blacklist '("CATEGORY" "FILE" "BLOCKED" "id" "type" "KEYWORDS" "file")))
    (let ((id (org-entry-get pom "id"))
          (type (org-entry-get pom "type" t))
          (properties (mapfilter (lambda (e) (not (member (car e) property-blacklist)))
                                 (org-entry-properties pom))))
       (concat (format "@%s{%s,\n" type id)
               (apply 'concat (mapcar (lambda (str) (concat str "\n")) (commafy (mapcar (lambda (prop) (format "%s = { %s }" (car prop) (cdr prop))) properties))))
               "}")
       )))


; Depricated
; (defun refmanager-bibtext-line-to-property ()
;  "Given a line in the format of a bibtex turn it into the format for an org-property"
;   (let ((line (thing-at-point 'line)))
;     (save-excursion
;       (let ((key (s-trim (car (s-split "=" line))))
;             ;; probably a much better regexie sort of way to do this
;             ;; than the chop-prefix/chop-suffix
;             (value (s-with (car (cdr (s-split "=" line))) s-trim (s-chop-suffix "},") (s-chop-prefix "{" ))))
;         (kill-line)
;         (insert (s-lex-format ":${key}: ${value}"))))))


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
    ;; generate the header of the org-mode file
    (refmanager-bibtext-generate-org-header)

    (let ((fields (bibtex-parse-entry))
          (beg (bibtex-beginning-of-entry))
          (end (bibtex-end-of-entry)))

      ;; remove the bibtex entry
      (delete-region beg end)
      (org-insert-drawer nil "PROPERTIES")

      (while fields
        (let ((key (car (car fields)))
              (value (s-with (cdr (car fields)) (s-chop-prefix "{") (s-chop-suffix "}"))))
          (cond ((s-equals? key "=type=")
                 (org-entry-put (point) "type" value))
                ((s-equals? key "=key=")
                 (org-entry-put (point) "id" value))
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





(provide 'refmanager-mode)
