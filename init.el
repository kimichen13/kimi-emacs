;;; package --- Summary:
;;; Kimi Chen's .emacs file
;;; Created on 14 April 2019 in Miami.
;;; Commentary:
;;; Code:

(defconst k/emacs-directory (concat (getenv "HOME") "/.emacs.d/"))
(defun k/emacs-subdirectory (d) "Emacs sub directory as D." (expand-file-name d k/emacs-directory))

(let* ((subdirs '("elisp" "backups" "snippets"))
       (fulldirs (mapcar (lambda (d) (k/emacs-subdirectory d)) subdirs)))
  (dolist (dir fulldirs)
    (when (not (file-exists-p dir))
      (message "Make directory: %s" dir)
      (make-directory dir))))

(setq custom-file (expand-file-name "custom.el" k/emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(add-to-list 'load-path (k/emacs-subdirectory "elisp"))
(add-to-list 'load-path (expand-file-name "~/Google Drive/MBP/Emacs/Configuration"))

(require 'init-size)
(require 'init-common)
(require 'init-basic)
(require 'init-font)
;; (require 'init-gc)

(require 'package)
(setq package-enable-at-startup nil) ; tells emacs not to load any packages before starting up
;; the following lines tell emacs where on the internet to look up
;; for new packages.
(setq package-archives '(("org"       . "http://orgmode.org/elpa/")
                         ("gnu"       . "http://elpa.gnu.org/packages/")
                         ("melpa"     . "https://melpa.org/packages/")
;;                         ("marmalade" . "http://marmalade-repo.org/packages/")
			 ))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package) ; unless it is already installed
  (package-refresh-contents) ; updage packages archive
  (package-install 'use-package)) ; and install the most recent version of use-package

(require 'exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))
(when (daemonp)
  (exec-path-from-shell-initialize))

(require 'use-package)

(require 'init-dashboard)
(require 'org-mode-kimi)
(require 'init-backup)

(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1)
  :config
  (add-to-list 'yas-snippet-dirs (k/emacs-subdirectory "snippets")))

(use-package which-key :ensure t)
(which-key-mode)

(use-package emojify
  :ensure t
  :hook (after-init . global-emojify-mode))
;(add-hook 'after-init-hook #'global-emojify-mode)


(use-package general :ensure t)

(use-package avy :ensure t)
(use-package counsel :ensure t
  :config
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> o") 'counsel-describe-symbol)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "M-y") 'counsel-yank-pop)
  (global-set-key (kbd "C-c k") 'counsel-ag)
  (global-set-key (kbd "C-x c") 'counsel-org-capture)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
)

(use-package counsel-projectile :ensure t
  :config
  (counsel-projectile-mode 1))

;; Open files in dired mode using 'open'
(eval-after-load "dired"
  '(progn
     (define-key dired-mode-map (kbd "z")
       (lambda () (interactive)
         (let ((fn (dired-get-file-for-visit)))
           (start-process "default-app" nil "open" fn))))))

(setq auth-sources '("~/.authinfo.gpg"))

(use-package swiper :ensure t
	:config
	(global-set-key "\C-s" 'swiper))

(use-package smart-comment :ensure t
  :bind ("M-;" . smart-comment))

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
	(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
	)
(setq projectile-project-search-path '("~/Documents/Program/"))

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :init
  (global-undo-tree-mode 1)
  :config
  (defalias 'redo 'undo-tree-redo)
  :bind (("C-z" . undo)     ; Zap to character isn't helpful
         ("C-S-z" . redo)))

(use-package monokai-theme :ensure t)

;;;; theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
;; (load-theme 'dracula t)
(load-theme 'monokai t)


;;;; powerline
(use-package powerline :ensure t
	:config
;	(setq powerline-image-apple-rgb t)
;	(setq powerline-default-separator 'nil)
	(powerline-default-theme)
	(display-time-mode t))

;;;; linum-relative
(use-package linum-relative
  :ensure t
  :config
  (defun linum-new-mode ()
    "If line numbers aren't displayed, then display them.
     Otherwise, toggle between absolute and relative numbers."
    (interactive)
    (if linum-mode
        (linum-relative-toggle)
      (linum-mode 1)))
)


(use-package golden-ratio :ensure t
	:config
	(golden-ratio-mode 1))

(use-package htmlize :ensure t)

(require 'init-company)

;;;; auto-sudoedit
(use-package auto-sudoedit
  :ensure t
  :config
  (auto-sudoedit-mode 1))

;;; Docker
(use-package dockerfile-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

;;; json
(use-package json-mode :ensure t)
(use-package ido-completing-read+ :ensure t)

(require 'init-git)
(require 'init-filemode)

(use-package expand-region
  :ensure t
  :bind ("M-C-w" . er/expand-region))

(use-package paredit
  :ensure t
  :config
  (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
  (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook           #'enable-paredit-mode))


;;;; Bind keys
(setq-default indent-tabs-mode nil)
(setq tab-width 2)

(global-set-key (kbd "M-m") nil)  ; Unbind `M-m` to use it as prefix

(general-define-key
 :prefix "M-m"
 "b" '(:ignore t :which-key "bookmarks")
 "bl" 'counsel-bookmark
 "bd" 'bookmark-delete
 "d"  'counsel-dired
 "g"  'magit-status
 "l"  'linum-new-mode
 "/"  'counsel-git-grep   ; find string in git project
 ;; bind to double key press
 "f"  '(:ignore t :which-key "files")
 "ff" 'counsel-find-file
 "fr" 'counsel-recentf
 "p"  'projectile-command-map
 "c"  '(:ignore t :whick-key "commons")
 "cp" 'org-publish-all
 "r"  'k/reload
 )

;; (require 'init-eshell)
;;; init.el ends here
