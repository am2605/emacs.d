(defconst *is-windows* (string-equal system-type "windows-nt"))

;; ensure theme package is installed
;;(require-package 'monokai-theme)

;;(require 'cfml-mode)

;; markdown mode
(add-to-list 'auto-mode-alist
             '("\\.md\\'" . markdown-mode))
;; choose modes for CFML automatically
(add-to-list 'auto-mode-alist
             '("\\.cfm\\'" . web-mode))
(add-to-list 'auto-mode-alist
             '("\\.cfc\\'" . web-mode))

;; (require 'mmm-mode)


;; (setq mmm-global-mode 'maybe)
;; (mmm-add-mode-ext-class nil "\\.php3?\\'" 'html-php)
;; (mmm-add-classes
;;  '((html-php
;;     :submode php-mode
;;     :front "<\\?\\(php\\)?"
;;     :back "\\?>")))

;; Use mmm-mode for highlighting of cfscript blocks in cfml files

;;(mmm-add-mode-ext-class nil "\\.cfm\\'" 'html-cfm)
;;(mmm-add-mode-ext-class nil "\\.cfc\\'" 'html-cfm)
;;(mmm-add-mode-ext-class nil "\\.cfc\\'" 'cfc-script)
;;(mmm-add-mode-ext-class nil "\\.cfc\\'" 'cfml-sql)

;; (mmm-add-classes
;;  '((html-cfm
;;     :submode js-mode
;;     :front "<cfscript>"
;;     :back "</cfscript>")
;;    (cfc-script
;;     :submode js-mode
;;     :front "\\`\\(component\\|\\/\\*\\)"
;;     :back "\\'")
;;    (cfml-sql
;;     :submode sql-mode
;;     :front "<cfquery[^>]*>"
;;     :back "</cfquery>")
;;    ))

;;(setq mmm-submode-decoration-level 0)
(setq initial-scratch-message "")

(when *is-windows*
  (set-face-attribute 'default nil :font "Source Code Pro 10"))
;;(load-theme 'monokai t)

(menu-bar-mode -1)

(setq cider-repl-use-pretty-printing nil)

;; use spaces instead of tabs
(setq sgml-basic-offset 4)
;; (setq-default c-basic-offset 4)
;; (setq c-basic-offset 4)
;; (setq tab-width 4)
;; (setq js-indent-level 4)
;; (setq indent-tabs-mode nil)
;; (setq nxml-child-indent 4)
;; (setq nxml-attribute-indent 4)

;;(set 'clean-aindent-is-simple-indent t)

;; (defun eclipse-forward-word ()
;;   (interactive)
;;   (let ((go-back (looking-at-p "\\W")))
;;     (forward-word)
;;     (when go-back
;;       (backward-word))))

;; (defun eclipse-backward-word ()
;;   (interactive)
;;   (let ((go-forward (looking-at-p "\\<")))
;;     (backward-word)
;;     (when go-forward
;;       (forward-word))))

;;(setq tab-stop-list (number-sequence 4 120 4))

(add-hook 'web-mode-hook
          (lambda ()
            (setq tab-stop-list (number-sequence 4 120 4))
            (setq indent-line-function 'indent-relative)
            ))

;; (add-hook 'html-mode-hook
;;           (lambda ()
;;             (turn-off-auto-fill)
;;             (set (make-local-variable 'electric-indent-functions)
;;                  (list (lambda (arg) 'no-indent)))))

;; (global-set-key (kbd "<C-left>") 'eclipse-backward-word)
;; (global-set-key (kbd "<C-right>") 'eclipse-forward-word)

;; override the default keybindings in paredit

;; (add-hook 'nxml-web-mode-hook
;;           (lambda ()
;;             (rng-validate-mode 0)))

(eval-after-load 'paredit
  '(progn
     (define-key paredit-mode-map (kbd "<M-right>") 'paredit-forward-slurp-sexp)
     (define-key paredit-mode-map (kbd "<M-left>") 'paredit-forward-barf-sexp)
     (define-key paredit-mode-map (kbd "<C-right>") nil)
     (define-key paredit-mode-map (kbd "<C-left>") nil)))

(provide 'init-local)
;;; init-local ends here
