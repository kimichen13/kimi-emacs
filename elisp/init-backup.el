;;; package --- Summary
;;; Commentary:
;;; Code:
;; Backup settings
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (k/emacs-subdirectory "backups"))))); which directory to put backups file
(setq vc-make-backup-files t )		; make backups file even when in version controlled dir
(defun save-all ()
  "Save all dirty buffers without asking for confirmation."
  (interactive)
  (save-some-buffers t))
(add-hook 'focus-out-hook 'save-all)

(provide 'init-backup)
;;; init-backup.el ends here
