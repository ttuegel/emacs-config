;;; Faces and colors


;; Use Iosevka font by default.

(set-frame-font (font-spec :family "Iosevka Type" :height 120))
(set-face-attribute 'default nil :family "Iosevka Type" :height 120)


;; Don't use italics to indicate types.

(custom-theme-set-faces 'user '(font-lock-type-face ((t :slant normal))))


;; Colors

(setq solarized-use-variable-pitch nil)
(setq solarized-high-contrast-mode-line t)
(setq solarized-height-minus-1 1.0)
(setq solarized-height-plus-1 1.0)
(setq solarized-height-plus-2 1.0)
(setq solarized-height-plus-3 1.0)
(setq solarized-height-plus-4 1.0)
(setq custom-safe-themes t)

(use-package solarized-theme
  :demand
  :load-path "./solarized-emacs"
  :config
  (load-theme 'solarized-light))


(provide 'init-faces)
