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

(setq initial-scratch-message "")

(add-to-list 'load-path "~/.emacs.d/local-packages/")

;;(require 'mmm-auto)
;;(setq mmm-global-mode 'auto)

(menu-bar-mode -1)

(require 'mmm-mode)
(require 'mmm-cfml)

;; choose modes for CFML automatically
(add-to-list 'magic-mode-alist
             '("<cfcomponent" . cftag-mode))
(add-to-list 'magic-mode-alist
             '("<!---" . cftag-mode))
(add-to-list 'auto-mode-alist
             '("\\.cfm\\'" . cftag-mode))
(add-to-list 'auto-mode-alist
             '("\\.cfc\\'" . cfscript-mode))

;; Use mmm-mode for highlighting of cfscript blocks in cfml files
(setq mmm-global-mode 'maybe)
(mmm-add-mode-ext-class nil "\\.cfm\\'" 'html-cfm)
(mmm-add-mode-ext-class nil "\\.cfc\\'" 'html-cfm)

(setq mmm-submode-decoration-level 0)

(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))

(require 'js)
;; use spaces instead of tabs
(setq js-indent-level 4)
;; (setq indent-tabs-mode nil)
(setq js-auto-indent-flag 1)
(setq sgml-basic-offset 4)

;; Set C-c i and C-c u to indent and outdent the selected region by 4 chars
;; (defun my-indent-region (N)

;;   (interactive "p")
;;   (if (use-region-p)
;;       (progn (indent-rigidly (region-beginning) (region-end) (* N 4))
;;              (deactivate-mark))
;;     (self-insert-command N)))

;; (defun my-unindent-region (N)
;;   (interactive "p")
;;   (if (use-region-p)
;;       (progn (indent-rigidly (region-beginning) (region-end) (* N -4))
;;              (deactivate-mark))
;;     (self-insert-command N)))

;; (global-set-key (kbd "C-c i") 'my-indent-region)
;; (global-set-key (kbd "C-c u") 'my-unindent-region)

(defun indent-region(numSpaces)
  (progn
                                        ; default to start and end of current line
    (setq regionStart (line-beginning-position))
    (setq regionEnd (line-end-position))

                                        ; if there's a selection, use that instead of the current line
    (when (use-region-p)
      (setq regionStart (region-beginning))
      (setq regionEnd (region-end))
      )

    (save-excursion ; restore the position afterwards
      (goto-char regionStart) ; go to the start of region
      (setq start (line-beginning-position)) ; save the start of the line
      (goto-char regionEnd) ; go to the end of region
      (setq end (line-end-position)) ; save the end of the line

      (indent-rigidly start end numSpaces) ; indent between start and end
      (setq deactivate-mark nil) ; restore the selected region
      )
    )
  )

(defun untab-region (N)
  (interactive "p")
  (indent-region -4)
  )

(defun tab-region (N)
  (interactive "p")
  (if (use-region-p)
      (indent-region 4) ; region was selected, call indent-region
    (insert "    ") ; else insert four spaces as expected
    )
  )

(global-set-key (kbd "<backtab>") 'untab-region)
;; (global-set-key (kbd "<tab>") 'tab-region)

(when *is-windows*
  (setq projectile-indexing-method 'alien)
  (set-face-attribute 'default nil :font "Source Code Pro 10"))
(when *is-a-mac*
  (set-default-font "Monaco 14"))

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
(change-theme 'sanityinc-solarized-light 'sanityinc-solarized-dark)

(desktop-save-mode 0)

;; makes autocomplete suggestions case sensitive
(setq company-dabbrev-downcase nil)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(provide 'init-local)

(server-start)
