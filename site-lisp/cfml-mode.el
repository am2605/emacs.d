;;; cfml-mode.el --- Emacs mode for editing CFML files

;; Copyright 2017 Andrew Myers

;; Author: Andrew Myers <am2605@gmail.com>
;; URL: https://github.com/am2605/cfml-mode
;; Version: 2.0.0
;; Package-Requires: ((emacs "25") (mmm-mode "0.5.4") (cfscript-mode "2.0.0")

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

;;

;;; Code:

(require 'sgml-mode)
(require 'cfscript-mode)

(mmm-add-classes
 '((cfml-cfscript
    :submode cfscript-mode
    :front "<cfscript>"
    :back "[ \t]*</cfscript>")
   (cfml-js
    :submode js-mode
    :front "<script[^>]*>[ \t]*\n?"
    :back "[ \t]*</script>")))

(defun cfml-indent-to-previous ()
  "Insert a newline character then indent the new line just like the previous line."
  (interactive)
  (newline)
  (unless (looking-back "\\`\n*")
    (indent-relative-maybe)))

;;;###autoload

(define-derived-mode cfml-mode html-mode "CFML"
  (setq-local sgml-empty-tags
              ;; From HTML-4.01's loose.dtd, parsed with
              ;; `sgml-parse-dtd', plus manual addition of "wbr".
              '("area" "base" "basefont" "br" "col" "frame" "hr" "img" "input"
                "isindex" "link" "meta" "param" "wbr"
                "cfdump" "cfset" "cfinclude" "cfargument" "cfqueryparam" "cfparam" "cfsetting"))
  (setq-local sgml-unclosed-tags
              ;; From HTML-4.01's loose.dtd, parsed with `sgml-parse-dtd'.
              '("body" "colgroup" "dd" "dt" "head" "html" "li" "option"
                "p" "tbody" "td" "tfoot" "th" "thead" "tr"
                "cfelse" "cfelseif" "cfreturn"))

  (setq tab-stop-list (number-sequence sgml-basic-offset 120 sgml-basic-offset))
  (local-set-key (kbd "RET") 'cfml-indent-to-previous)
  (setq indent-line-function 'sgml-indent-line)
  (setq comment-start "<!---")
  (setq comment-end " --->")
  (setq-local comment-start-skip "<!---[ \t]*")
  (setq-local comment-end-skip "[ \t]*---[ \t\n]*>")
  (electric-indent-local-mode -1))

(provide 'cfml-mode)

;;; cfml-mode.el ends here
