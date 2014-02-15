(defconst *is-windows* (string-equal system-type "windows-nt"))

;; ensure theme package is installed
(require-package 'monokai-theme)

;; markdown mode
(add-to-list 'auto-mode-alist
             '("\\.md\\'" . markdown-mode))
;; choose modes for CFML automatically
(add-to-list 'auto-mode-alist
             '("\\.cfm\\'" . html-mode))
(add-to-list 'auto-mode-alist
             '("\\.cfc\\'" . html-mode))

(require 'mmm-mode)

(setq mmm-global-mode 'maybe)
(mmm-add-mode-ext-class nil "\\.php3?\\'" 'html-php)
(mmm-add-classes
 '((html-php
    :submode php-mode
    :front "<\\?\\(php\\)?"
    :back "\\?>")))

(autoload 'php-mode "php-mode" "PHP editing mode" t)
(add-to-list 'auto-mode-alist '("\\.php3?\\'" . html-mode))

;; Use mmm-mode for highlighting of cfscript blocks in cfml files

(mmm-add-mode-ext-class nil "\\.cfm\\'" 'html-cfm)
(mmm-add-mode-ext-class nil "\\.cfc\\'" 'html-cfm)
(mmm-add-mode-ext-class nil "\\.cfc\\'" 'cfc-script)

(mmm-add-classes
 '((html-cfm
    :submode js-mode
    :front "<cfscript>"
    :back "</cfscript>")
   (cfc-script
    :submode js-mode
    :front "\\`\\(component\\|\\/\\*\\)"
    :back "\\'")))

(setq initial-scratch-message "")
(when *is-windows*
  (set-face-attribute 'default nil :font "Cosmic Sans Neue Mono 12"))
(load-theme 'monokai t)

(menu-bar-mode -1)

(provide 'init-local)
