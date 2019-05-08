;;; package --- Summary:
;;; kimi chen's .emacs file
;;; Created on 14 April 2019 in Miami.

;;; Commentary:

;;; Code:
;(set gc-cons-threshold 100000000)

(setq delete-old-versions -1 )		; delete excess backup versions silently
(setq version-control t )		; use version control
(setq vc-make-backup-files t )		; make backups file even when in version controlled dir
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")) ) ; which directory to put backups file
(setq vc-follow-symlinks t )				       ; don't ask for confirmation when opening symlinked file
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)) ) ;transform backups file name
(setq inhibit-startup-screen t )	; inhibit useless and old-school startup screen
(setq ring-bell-function 'ignore )	; silent bell when you make a mistake
(setq coding-system-for-read 'utf-8 )	; use utf-8 by default
(setq coding-system-for-write 'utf-8 )
(setq sentence-end-double-space nil)	; sentence SHOULD end with only a point.
(setq default-fill-column 80)		; toggle wrapping text at the 80th character
(setq initial-scratch-message "Welcome Kimi!") ; print a default message in the empty scratch buffer opened at startup

(require 'package)

(setq package-enable-at-startup nil) ; tells emacs not to load any packages before starting up
;; the following lines tell emacs where on the internet to look up
;; for new packages.
(setq package-archives '(("org"       . "http://orgmode.org/elpa/")
                         ("gnu"       . "http://elpa.gnu.org/packages/")
                         ("melpa"     . "https://melpa.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package) ; unless it is already installed
  (package-refresh-contents) ; updage packages archive
  (package-install 'use-package)) ; and install the most recent version of use-package

(require 'use-package)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
	 (quote
		(htmlize company golden-ratio yaml-mode tide flycheck web-mode dashboard linum-relative powerline smex auto-complete magit avy general use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-block ((t (:background "#969696"))))
 '(org-block-begin-line ((t (:underline "#A7A6AA" :foreground "#008ED1" :background "#EAEAFF"))))
 '(org-block-end-line ((t (:overline "#A7A6AA" :foreground "#008ED1" :background "#EAEAFF")))))

(use-package which-key :ensure t)
(which-key-mode)

(use-package general :ensure t)

;; (use-package helm :ensure t)
;; (global-set-key (kbd "M-x") #'helm-M-x)
;; (global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
;; (global-set-key (kbd "C-x C-f") #'helm-find-files)
;; (global-set-key (kbd "M-y") 'helm-show-kill-ring)
;; (global-set-key (kbd "C-x C-b") 'helm-buffers-list)
;; (helm-mode 1)
;; (setq projectile-completion-system 'helm)
;; (helm-projectile-on)

;; (use-package helm-projectile :ensure t)

(use-package avy :ensure t)
(use-package counsel :ensure t
	:config
	(global-set-key (kbd "M-y") 'counsel-yank-pop))
(use-package swiper :ensure t
	:config
	(global-set-key "\C-s" 'swiper))

(use-package ivy :ensure t
  :diminish (ivy-mode . "")
  :bind
  :config
  (ivy-mode 1)
  ;; add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’.
  (setq ivy-use-virtual-buffers t)
	(setq enable-recursive-minibuffers t)
  ;; number of result lines to display
  (setq ivy-height 10)
  ;; does not count candidates
  (setq ivy-count-format "")
  ;; no regexp by default
  (setq ivy-initial-inputs-alist nil)
  ;; configure regexp engine.
  (setq ivy-re-builders-alist
	  ;; allow input not in order
        '((t   . ivy--regex-ignore-order))))

(use-package projectile
  :ensure t
	:config
	(define-key projectile-mode-map (kbd "M-m p") 'projectile-command-map)
	(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
	(projectile-mode +1)
	)
(setq projectile-project-search-path '("~/Documents/Program/"))


;;;; theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'dracula t)

;;;; powerline
(use-package powerline :ensure t
	:config
;	(setq powerline-image-apple-rgb t)
;	(setq powerline-default-separator 'nil)
	(powerline-default-theme)
	(display-time-mode t))

;;;; linum-relative
(use-package linum-relative :ensure t
	:config
	(setq linum-relative-backend 'display-line-numbers-mode)
	;;	(helm-linum-relative-mode 1)
	)


(use-package golden-ratio :ensure t
	:config
	(golden-ratio-mode 1))


(use-package htmlize :ensure t)

;;;; dashboard
(use-package dashboard :ensure t
	:if (< (length command-line-args) 2)
  :preface
  (defun my/dashboard-banner ()
    "Sets a dashboard banner including information on package initialization
     time and garbage collections."
    (setq dashboard-banner-logo-title
          (format "Emacs ready in %.2f seconds with %d garbage collections."
                  (float-time
                   (time-subtract after-init-time before-init-time)) gcs-done)))
  :init
  (add-hook 'after-init-hook 'dashboard-refresh-buffer)
  (add-hook 'dashboard-mode-hook 'my/dashboard-banner)
  :custom (dashboard-startup-banner 'logo)
  :config (dashboard-setup-startup-hook))

;; Set the title
(setq dashboard-banner-logo-title "Welcome to Emacs Dashboard, Kimi!")
;; Set the banner
(setq dashboard-startup-banner "~/.emacs.d/dashboard.jpeg")
;; Value can be
;; 'official which DISPLAYS the official emacs logo
;; 'logo which displays an alternative emacs logo
;; 1, 2 or 3 which displays one of the text banners
;; "path/to/your/image.png" which displays whatever image you would prefer

;; Content is not centered by default. To center, set
(setq dashboard-center-content t)

;; To disable shortcut "jump" indicators for each section, set
;; (setq dashboard-show-shortcuts nil)

(setq dashboard-items '((recents  . 10)
                        (bookmarks . 5)
                        (projects . 5)
                        (agenda . 5)
												(registers . 5)))
(setq bookmark-save-flag 1)

;;;; AutoComplete
;; (use-package auto-complete :ensure t)
;; (ac-config-default) ;;; basic configuration

;;;; company-mode
(use-package company :ensure t)
(add-hook 'after-init-hook 'global-company-mode)


;;;; Magit
(use-package magit :ensure t)
(setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)  ; full screen

;;;; web-mode
(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode))
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-css-indent-offset 4)
  (setq web-mode-code-indent-offset 4)

  (setq web-mode-content-types-alist
        '(("jsx" . ".*\\.js\\'"))
        )
  )

;;;; flycheck
(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode t)
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  )

(use-package yaml-mode :ensure t)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-hook 'yaml-mode-hook
          (lambda ()
            (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;; ;;;; tern
;; (use-package tern
;;   :ensure t
;;   :config
;;   (add-hook 'web-mode-hook (lambda () (tern-mode t)))
;;   :bind (:map tern-mode-keymap
;;               ("M-*" . tern-pop-find-definition))
;;   )

;; (use-package company-tern
;;   :ensure t
;;   :config
;;   (add-to-list 'company-backends 'company-tern)
;;   )

;;;; tide
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

(use-package tide
  :ensure t
  :bind (("M-." . tide-jump-to-definition)
         ("M-," . tide-jump-back)
         )

  :config
  (setup-tide-mode)
  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t)

  ;; formats the buffer before saving
  (add-hook 'before-save-hook 'tide-format-before-save)

  ;; configure javascript-tide checker to run after your default javascript checker
  (flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append)

  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "js" (file-name-extension buffer-file-name))
                (setup-tide-mode))))
  ;; configure jsx-tide checker to run after your default jsx checker
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-next-checker 'javascript-eslint 'jsx-tide 'append)
  )

(add-hook 'web-mode-hook
            (lambda ()
              (set (make-local-variable 'company-backends)
                   '(
                      company-tide
                      ;company-react
                      company-dabbrev-code
                      company-keywords
                      company-files
                      company-yasnippet))))


;;;; Bind keys
(setq-default tab-width 2)

(global-set-key (kbd "M-m") nil)  ; Unbind `M-m` to use it as prefix

(general-define-key
 :prefix "M-m"
 "g"  'magit-status

 "l"  '(:ignore t :which-key "linum")
 "lt" 'linum-relative-toggle

 "b"  'helm-buffer-list
 "/"  'counsel-git-grep   ; find string in git project
 ;; bind to double key press
 "f"  '(:ignore t :which-key "files")
 "ff" 'counsel-find-file
 "fr" 'counsel-recentf

 )

;; (defun my/org-inline-css-hook (exporter)
;;   "Insert custom inline css to automatically set the background of code to whatever theme I'm using's background"
;;   (when (eq exporter 'html)
;;     (let* ((my-pre-bg (face-background 'default))
;;            (my-pre-fg (face-foreground 'default)))
;;       (setq org-export-html-style-extra (format "<style type=\"text/css\">\n pre.src {background-color: %s; color: %s;}</style>\n"
;;                 my-pre-bg my-pre-fg)))))

;; (add-hook 'org-export-before-processing-hook 'my/org-inline-css-hook)

(add-to-list 'load-path (expand-file-name "~/Google Drive/MBP/Emacs/Configuration"))
(require 'org-mode-kimi)
;;; init.el ends here
