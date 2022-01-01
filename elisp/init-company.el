;;; package --- Summary
;;; Commentary:
;;; Code:
;; Backup settings
;;;; company-mode
(use-package company
  :ensure t
  :config (progn (add-hook 'after-init-hook 'global-company-mode))
  )

(defvar company-dabbrev-downcase nil)

(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous))

(provide 'init-company)
;;; init-company.el ends here
