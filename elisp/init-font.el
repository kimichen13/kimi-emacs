;;; package --- Summary:
;;; Fonts Settings.
;;; Commentary:
;;; Code:

(if (display-graphic-p)
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
  )

(provide 'init-font)
;;; init-font.el ends here
