(defconst *is-windows* (string-equal system-type "windows-nt"))

(require 'mmm-mode)
;; choose modes for CFML automatically
(add-to-list 'auto-mode-alist
             '("\\.cfm\\'" . html-mode))
(add-to-list 'auto-mode-alist
             '("\\.cfc\\'" . html-mode))
(add-to-list 'auto-mode-alist
             '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist
             '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist
             '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist
             '("\\.htm\\'" . web-mode))
(add-to-list 'auto-mode-alist
             '("\\.html\\'" . web-mode))

(require 'package)

;; ensure default set of packages is installed
(when (not package-archive-contents)
  (package-refresh-contents))

;; Add in your own as you wish:
(defvar my-packages '(scala-mode projectile web-mode ensime sbt-mode spacemacs-theme theme-changer key-chord evil-mode)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Use mmm-mode for highlighting of cfscript blocks in cfml files
(setq mmm-global-mode 'maybe)
(mmm-add-mode-ext-class nil "\\.cfm\\'" 'html-cfm)
(mmm-add-mode-ext-class nil "\\.cfm\\'" 'cfm-js)
(mmm-add-mode-ext-class nil "\\.cfc\\'" 'html-cfm)
(mmm-add-mode-ext-class nil "\\.cfc\\'" 'cfc-script)

(mmm-add-classes
 '((html-cfm
    :submode js-mode
    :front "<cfscript>"
    :back "[ \t]*</cfscript>")
   (cfc-script
    :submode js-mode
    :front "\\`\\(component\\|\\/\\*\\)"
    :back "\\'")
   (cfm-js
    :submode js-mode
    :front "<script[^>]*>[ \t]*\n?"
    :back "[ \t]*</script>")))

(setq mmm-submode-decoration-level 0)

(setq initial-scratch-message "")

(when *is-windows*
  (set-face-attribute 'default nil :font "Source Code Pro 10"))

(menu-bar-mode -1)

(setq sgml-basic-offset 4)
(setq js-indent-level 4)

(defun my-enter ()
  "Inserts a newline character then indents the new line just like the previous line"
  (interactive)
  (newline)
  (unless (looking-back "\\`\n*")
    (indent-relative-maybe)))

(add-hook 'html-mode-hook
          (lambda ()
            (local-set-key (kbd "RET") 'newline)
            (setq tab-stop-list (number-sequence sgml-basic-offset 120 sgml-basic-offset))
            (setq indent-line-function 'tab-to-tab-stop)
            (electric-indent-local-mode -1)
            ))


(add-hook 'js-mode-hook
          (lambda ()
            (turn-off-auto-fill)
            (set (make-local-variable 'electric-indent-functions)
                 (list (lambda (arg) 'indent-relative)))))

(global-set-key (kbd "<S-tab>") 'un-indent-by-removing-4-spaces)

(defun un-indent-by-removing-4-spaces ()
  "remove 4 spaces from beginning of of line"
  (interactive)
  (save-excursion
    (save-match-data
      (beginning-of-line)
      ;; get rid of tabs at beginning of line
      (when (looking-at "^\\s-+")
        (untabify (match-beginning 0) (match-end 0)))
      (when (looking-at "^    ")
        (replace-match "")))))

(global-set-key "\C-xf" 'recentf-open-files)
(add-to-list 'recentf-exclude "\\.windows\\'")
(add-to-list 'recentf-exclude "\\.revive\\'")
(add-to-list 'recentf-exclude "\\ido.last\\'")
(desktop-save-mode 0)

(defun dark ()
  "Activate a dark color theme."
  (interactive)
  (load-theme 'spacemacs-dark t))

(require 'projectile)
(projectile-global-mode)

(require 'server)
(when *is-windows*
  (setq projectile-indexing-method 'alien)
  (unless (server-running-p)
    (server-start))
  )

(setq flycheck-disabled-checkers '(php sh-shellscript sh-bash sh-zsh sh-posix-bash))

(setenv "SBT_OPTS" "-Djline.terminal=jline.UnsupportedTerminal")

(require 'linum)
(add-hook 'web-mode-hook #'linum-on)

(require 'evil)
(require 'key-chord)
(evil-mode 1)
;;Exit insert mode by pressing j and then j quickly
(setq key-chord-two-keys-delay 0.5)
(key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
(key-chord-mode 1)

(setq calendar-location-name "Tamworth, NSW, Australia")
(setq calendar-latitude -31.1)
(setq calendar-longitude 150.93)

(require 'theme-changer)
(change-theme 'sanityinc-solarized-light 'spacemacs-dark)

(provide 'init-local)
