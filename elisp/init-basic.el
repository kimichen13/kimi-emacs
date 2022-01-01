;;; package --- Summary:
;;; Basic Settings.
;;; Commentary:
;;; Code:


(setq gnutls-min-prime-bits 4096)

(setq delete-old-versions -1 )		; delete excess backup versions silently
(setq version-control t )		; use version control

(setq vc-follow-symlinks t )		; don't ask for confirmation when opening symlinked file
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)) ) ;transform backups file name
(setq inhibit-startup-screen t )	; inhibit useless and old-school startup screen
(setq ring-bell-function 'ignore )	; silent bell when you make a mistake
(setq coding-system-for-read 'utf-8 )	; use utf-8 by default
(setq coding-system-for-write 'utf-8 )
(setq sentence-end-double-space nil)	; sentence SHOULD end with only a point.
(setq default-fill-column 80)		; toggle wrapping text at the 80th character
(setq initial-scratch-message ";;; Welcome Kimi!\n;;; Reload Emacs\n(load-file user-init-file)\n") ; print a default message in the empty scratch buffer opened at startup

(global-display-line-numbers-mode t)
;; (add-hook 'prog-mode-hook 'linum-mode)
;; hightlight line
(global-hl-line-mode t)
(set-face-background 'hl-line "#094771")
(set-face-background 'highlight nil)
;;; disable tool bar
(tool-bar-mode -1)
;;; disable scroll bar
(scroll-bar-mode -1)
;;; disable splash screen
(setq inhibit-splash-screen -1)
;;; disable menu bar
(menu-bar-mode -1)
;;; enable column number mode
(column-number-mode t)

;; 关闭自动生产备份文件
;;; disable auto backup
(setq make-backup-files nil)

;;; enable auto revert
(global-auto-revert-mode 1)
(setq create-lockfiles nil)
(set-language-environment "UTF-8")
(show-paren-mode 1)

(display-time-mode 1)
(setq display-time-format "%H:%M")

(defun remove-scratch-buffer ()
  "Remove *scratch* buffer."
  (if (get-buffer "*scratch*")
      (kill-buffer "*scratch*")))
(add-hook 'after-change-major-mode-hook 'remove-scratch-buffer)

(provide 'init-basic)
;;; init-basic.el ends here
