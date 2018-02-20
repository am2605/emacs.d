;;; cfscript-mode.el --- Emacs mode for editing CFML files

;; Copyright 2017 Andrew Myers

;; Author: Andrew Myers <am2605@gmail.com>
;; URL: https://github.com/am2605/cfml-mode
;; Version: 2.0.0
;; Package-Requires: ((emacs "25"))

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
;; Boston, MA 02111-1307, nn
;;}}}

;;; Commentary:

;;; Code:

(defface cfml-text-face '((t ())) "Standard text face")

(defun cfscript-indent-to-previous ()
  "Insert a newline character then indent the new line just like the previous line."
  (interactive)
  (newline)
  (unless (looking-back "\\`\n*")
    (indent-relative-maybe)))

;;;###autoload
(define-derived-mode cfscript-mode js-mode "cfscript"
  (setq tab-stop-list (number-sequence c-basic-offset 120 c-basic-offset))
  (local-set-key (kbd "RET") 'cfscript-indent-to-previous)
  (electric-indent-local-mode -1))

(font-lock-add-keywords 'cfscript-mode
                        '(
                          ("\\<component\\>" 0 'font-lock-keyword-face prepend)
                          ("\\<default\\>" 0 'cfml-text-face prepend)
                          ))

(provide 'cfscript-mode)

;;; cfscript-mode.el ends here
