;;; mmm-cfml.el --- CFML editing support

;; Copyright (C) 2017- Andrew Myers

;; Author: Andrew Myers <andrew@abm.id.au>

;;{{{ GPL

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
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;}}}

;;; Commentary:

;; This file contains definitions of CFML submode classes, and well as
;; support functions for proper indentation.

;; Usage:

;; (require 'mmm-auto)

;; (setq mmm-global-mode 'auto)

;; (mmm-add-mode-ext-class 'html-cfml-mode "\\.cfm\\''" 'html-cfm)
;; (mmm-add-mode-ext-class 'html-cfml-mode "\\.cfc\\'" 'html-cfm)
;; (mmm-add-mode-ext-class 'html-cfml-mode "\\.cfc\\'" 'cfc-script)

;; (add-to-list 'auto-mode-alist '("\\.cfm\\'" . html-cfml-mode))
;; (add-to-list 'auto-mode-alist '("\\.cfc\\'"  . html-cfml-mode))

;; Optional settings:

;; (setq mmm-submode-decoration-level 2
;;       mmm-parse-when-idle t)

;;; Code:

(require 'sgml-mode)
(eval-when-compile (require 'cl))
(require 'mmm-vars)
(require 'mmm-region)

(mmm-add-classes
 '((html-cfm
    :submode cfscript-mode
    :front "<cfscript>"
    :back "[ \t]*</cfscript>")
   (cfc-script
    :submode cfscript-mode
    :front "\\`\\(component\\|\\/\\*\\)"
    :back "\\'")))

(defun indent-to-previous ()
  "Inserts a newline character then indents the new line just like the previous line"
  (interactive)
  (newline)
  (unless (looking-back "\\`\n*")
  (indent-relative-maybe)))

(defun unindent-by-removing-n-spaces (n)
  "remove n spaces from beginning of of line"
  (interactive)
  (save-excursion
    (save-match-data
      (beginning-of-line)
      ;; get rid of tabs at beginning of line
      (when (looking-at "^\\s-+")
        (untabify (match-beginning 0) (match-end 0)))
      (when (looking-at (concat "^" (make-string n ?\s)))
        (replace-match "")))))

;;;###autoload
(define-derived-mode cfscript-mode js-mode "cfscript"
  (setq indent-line-function 'indent-relative)
  (local-set-key (kbd "RET") 'newline-and-indent)
  (local-set-key (kbd "S-<tab>") (lambda () (interactive) (unindent-by-removing-n-spaces sgml-basic-offset)))
  (add-to-list 'mmm-save-local-variables 'js--quick-match-re)
  (add-to-list 'mmm-save-local-variables 'js--quick-match-re-func))

;;;###autoload
(define-derived-mode html-cfml-mode html-mode "CFML"
  (setq tab-stop-list (number-sequence sgml-basic-offset 120 sgml-basic-offset))
  (local-set-key (kbd "RET") 'indent-to-previous)
  (local-set-key (kbd "S-<tab>") (lambda () (interactive) (unindent-by-removing-n-spaces sgml-basic-offset)))
  (setq indent-line-function 'tab-to-tab-stop)
  (electric-indent-local-mode -1))

(provide 'mmm-cfml)

;;; mmm-cfml.el ends here