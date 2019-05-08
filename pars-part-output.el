;;; pars-part-output.el --- explain value-list returned by `parse-partial-sexp'

;; Author: Andreas Röhler <andreas.roehler@easy-emacs.de>, unless indicated otherwise

;; Keywords: tools, convenience

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; ‘parse-partial-sexp’ is a powerful developing tool in Emacs Lisp.
;; However its return value is rather hard to interpret when
;; encountering first. The docu says its value being a list of
;; elements describing final state of parsing:

;;  0. depth in parens.
;;  1. character address of start of innermost containing list; nil if none.
;;  2. character address of start of last complete sexp terminated.
;;  3. non-nil if inside a string.
;;     (it is the character that will terminate the string,
;;      or t if the string should be terminated by a generic string delimiter.)
;;  4. nil if outside a comment, t if inside a non-nestable comment,
;;     else an integer (the current comment nesting).
;;  5. t if following a quote character.
;;  6. the minimum paren-depth encountered during this scan.
;;  7. style of comment, if any.
;;  8. character address of start of comment or string; nil if not in one.
;;  9. List of positions of currently open parens, outermost first.
;; 10. When the last position scanned holds the first character of a
;;     (potential) two character construct, the syntax of that position,
;;     otherwise nil.  That construct can be a two character comment
;;     delimiter or an Escaped or Char-quoted character.
;; 11..... Possible further internal information used by ‘parse-partial-sexp’.

;; The commands provided here list the result of calling
;; ‘parse-partial-sexp’ resp. ‘syntax-ppss’ at point alongside with
;; the explanation from its docu.

;; First column displays the position in resulting list
;; Follows the explanation, result shown in arrows below
;; For example

;; 0  depth in parens.
;; 	 ====> 1 <====

;; might relate to a result of
;; (nth 0 (parse-partial-sexp (point-min) (point)))

;; Suggestion for key-setting:
;; (global-set-key (kbd "<M-f6>") 'parse-partial-sexp-iac)
;; (global-set-key (kbd "<M-f7>") 'syntax-ppss-iac)

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
	   (t (funcall funktion (point-min) end)))
	  ;; (if
	  ;;     (or (eq this-command 'parse-partial-sexp-iac)
	  ;; 	  (eq this-command 'ppsi)
          ;;         (eq this-command 'parse-partial-sexp-commentstop))
	  ;;     (funcall funktion (point-min) end nil nil nil commentstop)
	  ;;   (funcall funktion (point-min) end))
	  )
	 (doku (documentation 'parse-partial-sexp))
	 (doku-abr (list (substring doku (1+ (string-match ":" doku)))))
	 (doku-ppss (and (eq funktion 'syntax-ppss)
			 (documentation 'syntax-ppss)))
	 (ppo (concat (format "%s" funktion) "-output")))
    (with-output-to-temp-buffer ppo
      (save-excursion
	(set-buffer ppo)
	(erase-buffer)
	(insert (car doku-abr))
	(goto-char (point-min))
	(switch-to-buffer (current-buffer))
	(re-search-forward "^ *\\([0-9]+\\)\." nil t 1)
	(replace-match (concat (match-string-no-properties 1) "th "))
	(ppsh-end-of-docstring)
	(newline)
	(dolist (elt rekord)
	  (insert (format "\t ====> %s <====" elt))
	  (re-search-forward "^ *\\([0-9]+\\)\." nil t 1)
	  (replace-match (concat (match-string-no-properties 1) "th "))
	  (ppsh-end-of-docstring)
	  (newline))
	(when doku-ppss
	  (goto-char (point-max))(newline 2)
	  (insert doku-ppss))))
    (toggle-read-only -1)
    (when (looking-at "^[ \t]*$")
      (delete-region (point) (progn (forward-line) (point))))))

(defun scan-lists-iac ()
  " "
  (interactive)
  (message "%s" (scan-lists (defun-beginning-position) (point) 0)))

(defalias 'ppsi 'parse-partial-sexp-iac)

(defalias 'ar-parse-partial-sexp-iac 'parse-partial-sexp-iac)
(defun parse-partial-sexp-iac (&optional arg)
  "Interactive form of parse-partial-sexp
output listed with documentation"
  (interactive "p")
  (save-excursion
    (ppse-documented-base 'parse-partial-sexp (point) (or arg 1) nil)))

(defalias 'ar-syntax-ppss-iac 'syntax-ppss-iac)
(defun syntax-ppss-iac (&optional arg)
  "Syntax-ppss made interactive output listed with documentation"
  (interactive "P")
  (ppse-documented-base 'syntax-ppss (point) arg nil))

(provide 'pars-part-output)
;;; pars-part-output.el ends here
