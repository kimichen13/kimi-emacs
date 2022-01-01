;;; package --- Summary
;;; Commentary:
;;; Code:
;; Backup settings
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
(setq dashboard-startup-banner "~/.emacs.d/banner.txt")
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

(defvar bookmark-save-flag 1)

(provide 'init-dashboard)
;;; init-dashboard.el ends here
