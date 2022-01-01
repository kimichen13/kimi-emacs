;;; package --- Summary
;;; Commentary:
;;; Code:
;; Backup settings

;;;; Magit
(use-package magit
  :ensure t
  ;; :commands magit-status magit-blame
  :init
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))
  :config
  (setq magit-branch-arguments nil
        ;; use ido to look for branches
        magit-completing-read-function 'ivy-completing-read
        ;; don't put "origin-" in front of new branch names by default
        magit-default-tracking-name-function 'magit-default-tracking-name-branch-only
        magit-push-always-verify nil
        ;; Get rid of the previous advice to go into fullscreen
        magit-restore-window-configuration t
        )
  (setf (alist-get 'unpushed magit-section-initial-visibility-alist) 'show)
  (setf (alist-get 'stashes magit-section-initial-visibility-alist) 'show)
  )

;;; Asiainfo PR Settings
;; (use-package forge
;;       :ensure t
;;       :after magit
;;       :config
;;       (add-to-list 'ghub-insecure-hosts "gitlab.cmss.com/api/v4")
;;       (add-to-list 'forge-alist '("gitlab.cmss.com" "gitlab.cmss.com/api/v4" "gitlab.cmss.com" forge-gitlab-repository))
;;       (setq auth-sources '((:source "~/.authinfo.gpg")))
;;       (setq gitlab.user "chenmaomao_ext")
;;       (setq gitlab.host "gitlab.cmss.com")
;;       (setq forge-topic-list-limit '(100 . 10))
;;       (setf (alist-get 'pullreq magit-section-initial-visibility-alist) 'show)
;;       )

(use-package orgit :ensure t)

(use-package magit-gitflow
  :ensure t
  :config (add-hook 'magit-mode-hook 'turn-on-magit-gitflow))

(provide 'init-git)
;;; init-git.el ends here
