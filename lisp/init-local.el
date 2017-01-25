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

(setq initial-scratch-message "")

(add-to-list 'load-path "~/.emacs.d/local-packages/")

(require 'mmm-cfml)
(require 'mmm-auto)
(setq mmm-global-mode 'auto)

(setq mmm-submode-decoration-level 0
      mmm-parse-when-idle t)

(mmm-add-mode-ext-class 'html-cfml-mode "\\.cfm\\''" 'html-cfm)
(mmm-add-mode-ext-class 'html-cfml-mode "\\.cfc\\'" 'html-cfm)
(mmm-add-mode-ext-class 'html-cfml-mode "\\.cfc\\'" 'cfc-script)

(add-to-list 'auto-mode-alist '("\\.cfm\\'" . html-cfml-mode))
(add-to-list 'auto-mode-alist '("\\.cfc\\'"  . html-cfml-mode))

(require 'js)
;; use spaces instead of tabs
(setq js-indent-level 4)
(setq indent-tabs-mode nil)
(setq js-auto-indent-flag 1)
(setq sgml-basic-offset 4)

;; Set C-c i and C-c u to indent and outdent the selected region by 4 chars
(defun my-indent-region (N)

  (interactive "p")
  (if (use-region-p)
      (progn (indent-rigidly (region-beginning) (region-end) (* N 4))
             (deactivate-mark))
    (self-insert-command N)))

(defun my-unindent-region (N)
  (interactive "p")
  (if (use-region-p)
      (progn (indent-rigidly (region-beginning) (region-end) (* N -4))
             (deactivate-mark))
    (self-insert-command N)))

(global-set-key (kbd "C-c i") 'my-indent-region)
(global-set-key (kbd "C-c u") 'my-unindent-region)

(when *is-windows*
  (set-face-attribute 'default nil :font "Source Code Pro 10"))

;;(menu-bar-mode -1)

(when *is-windows*
  (setq projectile-indexing-method 'alien))

(setq flycheck-disabled-checkers '(php sh-shellscript sh-bash sh-zsh sh-posix-bash))

(require 'linum)
(add-hook 'web-mode-hook #'linum-on)

(require 'yasnippet)
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"))
(yas-global-mode 1)

(setq calendar-location-name "Tamworth, NSW, Australia")
(setq calendar-latitude -31.1)
(setq calendar-longitude 150.93)

(require 'theme-changer)
(change-theme 'sanityinc-solarized-light 'sanityinc-tomorrow-night)

(provide 'init-local)
