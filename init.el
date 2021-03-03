;;; package --- Summary:
;;; Kimi Chen's .emacs file
;;; Created on 14 April 2019 in Miami.

;;; Commentary:

;;; Code:

(when (eq system-type 'darwin)

  ;; default Latin font (e.g. Consolas)
  ;; (set-default-font "Manoaco")

  (set-face-attribute 'default nil :family "Monaco")

  ;; default font size (point * 10)
  ;;
  ;; WARNING!  Depending on the default font,
  ;; if the size is not supported very well, the frame will be clipped
  ;; so that the beginning of the buffer may not be visible correctly.
  (set-face-attribute 'default nil :height 145)

  ;; use specific font for Korean charset.
  ;; if you want to use different font size for specific charset,
  ;; add :size POINT-SIZE in the font-spec.
  (set-fontset-font t 'hangul (font-spec :name "NanumGothicCoding"))
  
  ;; you may want to add different for other charset in this way.
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font) charset
                      (font-spec :family "STFangsong" :size 18)
                      )))

(if (featurep 'cocoa)
    (progn
      (setq ns-use-native-fullscreen nil)
      (setq ns-use-fullscreen-animation nil)

      (set-frame-parameter (selected-frame) 'fullscreen 'maximized)

      (run-at-time "2sec" nil
                   (lambda ()
                     (toggle-frame-fullscreen)
                     )))
  (require 'fullscreen)
  (fullscreen))

;; (custom-set-variables
;;  '(initial-frame-alist (quote ((fullscreen . maximized)))))

(defconst kimi/emacs-directory (concat (getenv "HOME") "/.emacs.d/"))
(defun kimi/emacs-subdirectory (d) (expand-file-name d kimi/emacs-directory))

(let* ((subdirs '("elisp" "backups"))
       (fulldirs (mapcar (lambda (d) (kimi/emacs-subdirectory d)) subdirs)))
  (dolist (dir fulldirs)
    (when (not (file-exists-p dir))
      (message "Make directory: %s" dir)
      (make-directory dir))))

(setq custom-file (expand-file-name "custom.el" kimi/emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(add-to-list 'load-path (kimi/emacs-subdirectory "elisp"))
(add-to-list 'load-path (expand-file-name "~/Google Drive/MBP/Emacs/Configuration"))

(require 'org-mode-kimi)

(setq gc-cons-threshold 50000000)
(setq gnutls-min-prime-bits 4096)

;; Backup settings
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (kimi/emacs-subdirectory "backups"))))); which directory to put backups file
(setq vc-make-backup-files t )		; make backups file even when in version controlled dir
(defun save-all ()
  "Save all dirty buffers without asking for confirmation."
  (interactive)
  (save-some-buffers t))
(add-hook 'focus-out-hook 'save-all)



(setq delete-old-versions -1 )		; delete excess backup versions silently
(setq version-control t )		; use version control

(setq vc-follow-symlinks t )				       ; don't ask for confirmation when opening symlinked file
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)) ) ;transform backups file name
(setq inhibit-startup-screen t )	; inhibit useless and old-school startup screen
(setq ring-bell-function 'ignore )	; silent bell when you make a mistake
(setq coding-system-for-read 'utf-8 )	; use utf-8 by default
(setq coding-system-for-write 'utf-8 )
(setq sentence-end-double-space nil)	; sentence SHOULD end with only a point.
(setq default-fill-column 80)		; toggle wrapping text at the 80th character
(setq initial-scratch-message ";;; Welcome Kimi!\n;;; Reload Emacs\n(load-file user-init-file)\n") ; print a default message in the empty scratch buffer opened at startup

(require 'package)
(setq package-enable-at-startup nil) ; tells emacs not to load any packages before starting up
;; the following lines tell emacs where on the internet to look up
;; for new packages.
(setq package-archives '(("org"       . "http://orgmode.org/elpa/")
                         ("gnu"       . "http://elpa.gnu.org/packages/")
                         ("melpa"     . "https://melpa.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))
(package-initialize)

;; (package-refresh-contents)

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

(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1)
  :config
  (add-to-list 'yas-snippet-dirs (kimi/emacs-subdirectory "snippets")))

(use-package which-key :ensure t)
(which-key-mode)

(use-package general :ensure t)

(use-package restclient :ensure t)
(add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode))

(use-package es-mode :ensure t)
(add-to-list 'auto-mode-alist '("\\.es$" . es-mode))

(use-package avy :ensure t)
(use-package counsel :ensure t
	:config
	(global-set-key (kbd "M-y") 'counsel-yank-pop))
(use-package counsel-projectile :ensure t
  :config
  (counsel-projectile-mode 1))

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

(setq dashboard-items '((agenda . 8)
                        (recents  . 8)
                        (bookmarks . 5)
                        (projects . 5)))

(setq bookmark-save-flag 1)

;;;; AutoComplete
;; (use-package auto-complete :ensure t)
;; (ac-config-default) ;;; basic configuration

;;;; company-mode
(use-package company :ensure t)
(setq company-dabbrev-downcase nil)
(add-hook 'after-init-hook 'global-company-mode)

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

;;;; Magit
(use-package magit
  :ensure t
  :commands magit-status magit-blame
  :init
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))
  :config
  (setq magit-branch-arguments nil
        ;; use ido to look for branches
        magit-completing-read-function 'magit-ido-completing-read
        ;; don't put "origin-" in front of new branch names by default
        magit-default-tracking-name-function 'magit-default-tracking-name-branch-only
        magit-push-always-verify nil
        ;; Get rid of the previous advice to go into fullscreen
        magit-restore-window-configuration t
        )
  (setf (alist-get 'unpushed magit-section-initial-visibility-alist) 'show)
  (setf (alist-get 'stashes magit-section-initial-visibility-alist) 'show)
  :bind ("C-x g" . magit-status))

(use-package orgit :ensure t)

(use-package ido-completing-read+ :ensure t)

(use-package magit-gitflow
  :ensure t
  :config (add-hook 'magit-mode-hook 'turn-on-magit-gitflow))

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

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

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

;; (defun copy-from-osx ()
;;     (shell-command-to-string "pbpaste"))
  
;;   (defun paste-to-osx (text &optional push)
;;     (let ((process-connection-type nil))
;;       (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
;;         (process-send-string proc text)
;;         (process-send-eof proc))))
  
;;   (setq interprogram-cut-function 'paste-to-osx)
;;   (setq interprogram-paste-function 'copy-from-osx))

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
 )

;; (require 'init-eshell)
;;; init.el ends here
