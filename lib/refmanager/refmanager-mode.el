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

(define-minor-mode refmanager-mode
  "Refmanager mode provides some basic functions for managing
an org-mode file of citations based on org entry properties. "
  nil
  " RefMan"
  `((,(kbd "C-c r o") . refmanager-open-entry-file))
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
