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

(setq mmm-submode-decoration-level 0)
(setq initial-scratch-message "")
(when *is-windows*
  (set-face-attribute 'default nil :font "Fantasque Sans Mono 12"))
(load-theme 'monokai t)

(menu-bar-mode -1)

(setq cider-repl-use-pretty-printing nil)

;; use spaces instead of tabs
(setq sgml-basic-offset 4)
;;(setq c-basic-indent 2)
(setq tab-width 4)
(setq indent-tabs-mode nil)

(add-hook 'html-mode-hook
          (lambda ()
            (turn-off-auto-fill)
            (set (make-local-variable 'electric-indent-functions)
                 (list (lambda (arg) 'no-indent)))))

;; override the default keybindings in paredit

(eval-after-load 'paredit
  '(progn
     (define-key paredit-mode-map (kbd "<M-right>") 'paredit-forward-slurp-sexp)
     (define-key paredit-mode-map (kbd "<M-left>") 'paredit-forward-barf-sexp)
     (define-key paredit-mode-map (kbd "<C-right>") nil)
     (define-key paredit-mode-map (kbd "<C-left>") nil)))

(provide 'init-local)
