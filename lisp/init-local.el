(defconst *is-windows* (string-equal system-type "windows-nt"))

(require 'mmm-mode)
;; choose modes for CFML automatically
(add-to-list 'auto-mode-alist
             '("\\.cfm\\'" . html-mode))
(add-to-list 'auto-mode-alist
             '("\\.cfc\\'" . html-mode))

(require 'package)

;; ensure default set of packages is installed
(when (not package-archive-contents)
  (package-refresh-contents))

;; Add in your own as you wish:
(defvar my-packages '(evil groovy-mode projectile)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Use mmm-mode for highlighting of cfscript blocks in cfml files
(setq mmm-global-mode 'maybe)
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
  (set-face-attribute 'default nil :font "Consolas 11"))

(load-theme 'sanityinc-solarized-light t)

(menu-bar-mode -1)

(setq sgml-basic-offset 4)
(setq js-indent-level 4)

(add-hook 'html-mode-hook
          (lambda ()
            (setq tab-stop-list (number-sequence sgml-basic-offset 120 sgml-basic-offset))
            (setq indent-line-function 'indent-relative)
            (set (make-local-variable 'electric-indent-mode) nil)))

(add-hook 'js-mode-hook
          (lambda ()
            (turn-off-auto-fill)
            (set (make-local-variable 'electric-indent-functions)
                 (list (lambda (arg) 'indent-relative)))))

(global-set-key (kbd "<backtab>")
                (lambda ()
                  (interactive)
                  (un-indent-by-removing-x-spaces sgml-basic-offset)))

(defun un-indent-by-removing-x-spaces (num-spaces)
  "remove spaces from beginning of of line"
  (save-excursion
    (save-match-data
      (beginning-of-line)
      get rid of tabs at beginning of line
      (when (looking-at "^\\s-+")
        (untabify (match-beginning 0) (match-end 0)))
      (when (looking-at (concat "^" (make-string num-spaces ?\s)))
        (replace-match "")))))

(require 'evil)
(evil-mode 1)

(setq-default flycheck-disabled-checkers '(php))

(global-set-key "\C-xf" 'recentf-open-files)
(add-to-list 'recentf-exclude "\\.windows\\'")
(add-to-list 'recentf-exclude "\\.revive\\'")
(add-to-list 'recentf-exclude "\\ido.last\\'")
(desktop-save-mode 0)

(defun dark ()
  "Activate a dark color theme."
  (interactive)
  (load-theme 'sanityinc-tomorrow-night t))

(require 'projectile)
(projectile-global-mode)
(setq projectile-indexing-method 'alien)

(provide 'init-local)
