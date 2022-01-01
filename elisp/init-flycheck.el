;;; package --- Summary
;;; Commentary:
;;; Code:

(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode t)
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  )

(provide 'init-flycheck)
;;; init-flycheck.el ends here
