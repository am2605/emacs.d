(defconst *is-windows* (string-equal system-type "windows-nt"))

(require 'package)

;; ensure default set of packages is installed
(when (not package-archive-contents)
  (package-refresh-contents))

;; Add in your own as you wish:
(defvar my-packages '(github-theme projectile theme-changer web-mode yasnippet)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;;(add-to-list 'load-path "~/.emacs.d/site-lisp/")
;;(add-to-list 'load-path "D:/AMyers/dev/cfml-mode")

(setq initial-scratch-message "")

(menu-bar-mode -1)

(require 'mmm-mode)
(require 'cfml-mode)

(add-to-list 'magic-mode-alist
             '("<cfcomponent" . cftag-mode))
(add-to-list 'magic-mode-alist
             '("<!---" . cftag-mode))
(add-to-list 'auto-mode-alist
             '("\\.cfm\\'" . cftag-mode))
(add-to-list 'auto-mode-alist
             '("\\.cfc\\'" . cfml-cfscript-mode))

(setq mmm-global-mode 'maybe)
(mmm-add-mode-ext-class nil "\\.cfm\\'" 'cfml-cftag)
(mmm-add-mode-ext-class nil "\\.cfc\\'" 'cfml-cftag)

(setq mmm-submode-decoration-level 0)

;; Indenting
;; (require 'js)
;; (setq js-indent-level 4)
;; (setq js-auto-indent-flag 1)
;; (setq sgml-basic-offset 4)
;; (setq tab-width 4)

;; (setq-default indent-tabs-mode nil)
;; (setq c-basic-offset 4)

;; (setq-default indent-tabs-mode t)
;; (setq-default tab-width 4) ; Assuming you want your tabs to be four spaces wide
;; (defvaralias 'c-basic-offset 'tab-width)

(when *is-windows*
  (setq projectile-indexing-method 'alien)
  (set-face-attribute 'default nil :font "Source Code Pro 10")
  ;;  (setq-default cursor-type 'bar)
  )
(when *is-a-mac*
  (set-default-font "Monaco 14"))

(setq flycheck-disabled-checkers '(php sh-shellscript sh-bash sh-zsh sh-posix-bash))

(require 'linum)
(add-hook 'web-mode-hook #'linum-on)

(require 'yasnippet)
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"))
(yas-global-mode 1)

;; Theming
(setq calendar-location-name "Tamworth, NSW, Australia")
(setq calendar-latitude -31.1)
(setq calendar-longitude 150.93)

(require 'theme-changer)
(change-theme 'sanityinc-solarized-light 'sanityinc-solarized-dark)

(desktop-save-mode 0)

;; makes autocomplete suggestions case sensitive
(setq company-dabbrev-downcase nil)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(require 'ivy)
(define-key ivy-minibuffer-map (kbd "<up>") #'ivy-previous-line)

(provide 'init-local)

(server-start)
