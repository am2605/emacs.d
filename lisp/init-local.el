(defconst *is-windows* (string-equal system-type "windows-nt"))

;; choose modes for CFML automatically
(add-to-list 'magic-mode-alist
             '("<cfcomponent" . html-mode))
(add-to-list 'magic-mode-alist
             '("<!---" . html-mode))
(add-to-list 'auto-mode-alist
             '("\\.cfm\\'" . html-mode))
(add-to-list 'auto-mode-alist
             '("\\.cfc\\'" . js-mode))
             
(setq initial-scratch-message "")
(when *is-windows*
  (set-face-attribute 'default nil :font "Source Code Pro 11"))

(menu-bar-mode -1)

(setq cider-repl-use-pretty-printing nil)

;; use spaces instead of tabs
(setq sgml-basic-offset 4)
(setq c-basic-offset 4)
(setq js-basic-offset 4)
(setq js-indent-level 4)
(setq tab-width 4)
(setq indent-tabs-mode nil)

(add-hook 'html-mode-hook
          (lambda ()
            (setq tab-stop-list (number-sequence 4 120 4))
            (setq indent-line-function 'no-indent)
            ))

(add-hook 'js-mode-hook
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

(setq-default custom-enabled-themes '(monokai))

(provide 'init-local)
