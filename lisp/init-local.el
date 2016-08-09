(defconst *is-windows* (string-equal system-type "windows-nt"))

(setq exec-path (add-to-list 'exec-path "D:/Applications/Gow/bin"))
(setenv "PATH" (concat "D:\\Applications\\Gow\\bin;" (getenv "PATH")))

;; choose modes for CFML automatically
(add-to-list 'auto-mode-alist
             '("\\.md\\'" . markdown-mode))
;; choose modes for CFML automatically
(add-to-list 'magic-mode-alist
             '("<cfcomponent" . web-mode))
(add-to-list 'magic-mode-alist
             '("<!---" . web-mode))
(add-to-list 'auto-mode-alist
             '("\\.cfm\\'" . web-mode))
(add-to-list 'auto-mode-alist
             '("\\.cfc\\'" . js2-mode))

;; ensure default set of packages is installed
(when (not package-archive-contents)
  (package-refresh-contents))

;; Add in your own as you wish:
(defvar my-packages '(evil groovy-mode projectile web-mode)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(setq initial-scratch-message "")

(when *is-windows*
  (set-face-attribute 'default nil :font "Consolas 11"))

;; (load-theme 'sanityinc-solarized-dark t)

(menu-bar-mode -1)

(setq sgml-basic-offset 4)
(setq js-indent-level 4)
(setq js2-basic-offset 4)

;; (add-hook 'sgml-mode-hook
;;           (lambda ()
;;             ;; Default indentation to 2, but let SGML mode guess, too.
;;             (set (make-local-variable 'sgml-basic-offset) 2)
;;             (sgml-guess-indent)))

(add-hook 'js2-mode-hook
          'js2-mode-hide-warnings-and-errors)


(defun my-enter ()
  "Inserts a newline character then indents the new line just like the previous line"
  (interactive)
  (newline)
  (unless (looking-back "\\`\n*")
    (indent-relative-maybe)))

(add-hook 'web-mode-hook
          (lambda ()
            (local-set-key (kbd "RET") 'my-enter)
            (setq tab-stop-list (number-sequence sgml-basic-offset 120 sgml-basic-offset))
            (setq indent-line-function 'tab-to-tab-stop)
            (electric-indent-local-mode -1)))

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

;; (global-set-key (kbd "<backtab>")
;;                 (lambda ()
;;                   (interactive)
;;                   (un-indent-by-removing-x-spaces sgml-basic-offset)))

;; (defun un-indent-by-removing-x-spaces (num-spaces)
;;   "remove spaces from beginning of of line"
;;   (save-excursion
;;     (save-match-data
;;       (beginning-of-line)
;;       get rid of tabs at beginning of line
;;       (when (looking-at "^\\s-+")
;;         (untabify (match-beginning 0) (match-end 0)))
;;       (when (looking-at (concat "^" (make-string num-spaces ?\s)))
;;         (replace-match "")))))

;; (require 'evil)
;; (evil-mode 1)

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

(setq flycheck-disabled-checkers '(php sh-shellscript sh-bash sh-zsh sh-posix-bash))

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(provide 'init-local)
