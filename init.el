;;; package --- Summary:
;;; kimi chen's .emacs file
;;; Created on 14 April 2019 in Miami.

;;; Commentary:

;;; Code:
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
		(golden-ratio yaml-mode tide flycheck web-mode dashboard linum-relative powerline smex auto-complete magit avy general use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(use-package which-key :ensure t)
(which-key-mode)

(use-package general :ensure t)

(use-package helm :ensure t)
(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(helm-mode 1)

(use-package avy :ensure t)
(use-package counsel :ensure t)
(use-package swiper :ensure t)

(use-package projectile
  :ensure t
  :config
	(define-key projectile-mode-map (kbd "M-m p") 'projectile-command-map)
	(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))
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
(use-package linum-relative :ensure t)
(setq linum-relative-backend 'display-line-numbers-mode)
;(global-linum-mode nil)
																				;(linum-relative-toggle)

(use-package golden-ratio :ensure)
(golden-ratio-mode 1)

;;;; dashboard
(use-package dashboard :ensure t
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

(setq dashboard-items '((recents  . 5)
                        ;; (bookmarks . 5)
                        (projects . 5)
                        (agenda . 5)
			(registers . 5)))

;;;; AutoComplete
;; (use-package auto-complete :ensure t)
;; (ac-config-default) ;;; basic configuration

;;;; company-mode
(use-package company :ensure t)
(add-hook 'after-init-hook 'global-company-mode)


;;;; smex
;; (use-package smex :ensure t)
;; (global-set-key (kbd "M-x") 'smex)
;; (global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is old M-x.
;; (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

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

(setq ido-separator "\n")

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

;; (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
;; (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

 
 "b"  'ivy-switch-buffer  ; change buffer, chose using ivy
 "/"  'counsel-git-grep   ; find string in git project
 ;; bind to double key press
 "f"  '(:ignore t :which-key "files")
 "ff" 'counsel-find-file
 "fr" 'counsel-recentf

 )
