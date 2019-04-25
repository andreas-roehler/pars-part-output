;;; pars-part-output.el --- explain value-list returned by `parse-partial-sexp' -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Andreas Röhler

;; Author: Andreas Röhler  <andreas.roehler@easy-emacs.de>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; List the result of calling ‘parse-partial-sexp’ resp. ‘syntax-ppss’
;; alongside with the explanation from its docu.

;; First column displays the position in resulting list
;; For example

;; 0  depth in parens.
;; 	 ====> 1 <====

;; might relate to a result of
;; (nth 0 (parse-partial-sexp (point-min) (point)))

;; Suggestion for key-setting:
;; (global-set-key (kbd "<M-f6>") 'parse-partial-sexp-iac)
;; (global-set-key (kbd "<M-f7>") 'syntax-ppss-iac)

;;; Code:

(defun parse-partial-sexp-commentstop (&optional arg)
  "Interactive form of parse-partial-sexp
output listed with documentation"
  (interactive "P")
  (save-excursion
    (ppse-documented-base 'parse-partial-sexp (point) arg t)))

(defun ppsh-end-of-docstring ()
  "Go to the end of docstring displayed."
  (re-search-forward "^ *[0-9]+\." nil 'move 1)
  (beginning-of-line)
  (skip-chars-backward " \t\r\n\f"))

(defun ppse-documented-base (funktion end arg commentstop)
  "Parse partial symbolic expression
list results below its documentation "
  (let* ((scan (condition-case nil
                   (scan-lists (point) 1 0)
                 (error nil)))
	 (rekord
	  (cond
	   ((or (eq this-command 'parse-partial-sexp-iac)
		(eq this-command 'ppsi)
                (eq this-command 'parse-partial-sexp-commentstop))
	    (funcall funktion (point-min) end nil nil nil commentstop))
	   ((eq funktion 'syntax-ppss)
	    (funcall funktion))
	   (t (funcall funktion (point-min) end))))
	 (doku (documentation 'parse-partial-sexp))
	 (doku-abr (list (substring doku (1+ (string-match ":" doku)))))
	 (doku-ppss (and (eq funktion 'syntax-ppss)
			 (documentation 'syntax-ppss)))
         (count 0))
    (with-output-to-temp-buffer (concat (format "%s" funktion) "-output")
      (set-buffer standard-output)
      (insert (car doku-abr))
      (goto-char (point-min))
      (switch-to-buffer (current-buffer))
      (re-search-forward "^ *\\([0-9]+\\)\." nil t 1)
      ;; (replace-match (concat (prin1-to-string count) "-"))
      (replace-match (concat (match-string-no-properties 1) " "))
      ;; (replace-match "-")
      (setq count (1+ count))
      (ppsh-end-of-docstring)
      (newline)
      (dolist (elt rekord)
	(insert (format "\t ====> %s <====" elt))
	(re-search-forward "^ *\\([0-9]+\\)\." nil t 1)
	(replace-match (concat (match-string-no-properties 1) " "))
	(ppsh-end-of-docstring)
	(newline)
        (setq count (1+ count)))
      (when doku-ppss
	(goto-char (point-max))(newline 2)
	(insert doku-ppss))))
  (toggle-read-only -1)
  (when (looking-at "^[ \t]*$")
    (delete-region (point) (progn (forward-line) (point)))))

(defun scan-lists-iac ()
  " "
  (interactive)
  (message "%s" (scan-lists (defun-beginning-position) (point) 0)))

(defalias 'ppsi 'parse-partial-sexp-iac)
(defun parse-partial-sexp-iac (&optional arg)
  "Interactive form of parse-partial-sexp
output listed with documentation"
  (interactive "p")
  (save-excursion
    (ppse-documented-base 'parse-partial-sexp (point) (or arg 1) nil)))

(defun syntax-ppss-iac (&optional arg)
  "Syntax-ppss made interactive output listed with documentation"
  (interactive "P")
  (ppse-documented-base 'syntax-ppss (point) arg nil))

(provide 'pars-part-output)
;;; pars-part-output.el ends here
