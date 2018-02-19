(defconst *is-windows* (string-equal system-type "windows-nt"))

(require 'package)

;; ensure default set of packages is installed
(when (not package-archive-contents)
  (package-refresh-contents))

;; Add in your own as you wish:
(defvar my-packages '(projectile theme-changer web-mode yasnippet)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(add-to-list 'load-path "d:/AMyers/emacs_home/.emacs.d/site-lisp/")
;;(load "mhtml-mode")
(setq initial-scratch-message "")

(menu-bar-mode -1)

(require 'mmm-mode)
(require 'cfml-mode)
(require 'cfscript-mode)

(setq mmm-global-mode 'auto)
(mmm-add-mode-ext-class nil "\\.cfm\\'" 'cfml-js)
(mmm-add-mode-ext-class nil "\\.cfm\\'" 'cfml-cfscript)
(mmm-add-mode-ext-class nil "\\.cfc\\'" 'cfml-cfscript)

(add-to-list 'magic-mode-alist
             '("<cfcomponent" . cfml-mode))
(add-to-list 'magic-mode-alist
             '("<!---" . cfml-mode))
(add-to-list 'auto-mode-alist
             '("\\.cfm\\'" . cfml-mode))
(add-to-list 'auto-mode-alist
             '("\\.cfc\\'" . cfscript-mode))

;; Indenting
(require 'js)
(setq js-indent-level 4)
(setq js-auto-indent-flag 1)
(setq sgml-basic-offset 4)
(setq tab-width 4)
(setq-default indent-tabs-mode nil)
(setq c-basic-offset 4)

(when *is-windows*
  (setq projectile-indexing-method 'alien)
  (set-face-attribute 'default nil :font "Consolas 11")
  )
(when *is-a-mac*
  (set-default-font "Monaco 14"))

(setq flycheck-disabled-checkers '(php sh-shellscript sh-bash sh-zsh sh-posix-bash))

(require 'linum)
(add-hook 'web-mode-hook #'linum-on)

;; (require 'yasnippet)
;; (setq yas-snippet-dirs
;;       '("~/.emacs.d/snippets"))
;; (yas-global-mode 1)

;; Theming
(setq calendar-location-name "Tamworth, NSW, Australia")
(setq calendar-latitude -31.1)
(setq calendar-longitude 150.93)

(require 'theme-changer)
(change-theme 'sanityinc-tomorrow-day 'sanityinc-solarized-dark)

(desktop-save-mode 0)

;; makes autocomplete suggestions case sensitive
(setq company-dabbrev-downcase nil)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(require 'ivy)
(define-key ivy-minibuffer-map (kbd "<up>") #'ivy-previous-line)

(require 'ryo-modal)

(global-set-key (kbd "<escape>") 'ryo-modal-mode)

(ryo-modal-keys
 ("i" ryo-modal-mode)
 ("h" backward-char)
 ("j" next-line)
 ("k" previous-line)
 ("l" forward-char))

(ryo-modal-key "0" #'move-beginning-of-line)
(ryo-modal-key "$" #'move-end-of-line)
(ryo-modal-key "SPC b b" #'ivy-switch-buffer)
(ryo-modal-key "SPC f s" #'save-buffer)
(ryo-modal-key "SPC p f" #'projectile-find-file :exit t)
(ryo-modal-key "SPC p p" #'projectile-switch-project :exit t)
;; (ryo-modal-key "SPC q q" #'save-buffers-kill-terminal)

(ryo-modal-key
 "d" '(
       ("d" kill-whole-line)))

;; (require 'neotree)
;; (global-set-key [f8] 'neotree-toggle)
;; (global-set-key (kbd "C-x o") 'other-window)
;; (setq projectile-switch-project-action 'neotree-projectile-action)

;; et ne inducas nos in temptationem
;; (require 'evil)
;; (global-evil-leader-mode)
;; (evil-leader/set-leader "<SPC>")
;; (evil-leader/set-key
;;   "b" 'switch-to-buffer
;;   "k" 'kill-buffer
;;   "s" 'save-buffer
;;   "q" 'save-buffers-kill-terminal
;;   "f" 'projectile-find-file
;;   "p" 'projectile-switch-project
;;   "b" 'counsel-recentf)
;; (evil-mode 1)
;; sed libera nos a malo

(provide 'init-local)
(server-start)
