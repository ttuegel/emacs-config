;;; Faces and colors


;; Use system default monospace font in 12 pt height.

(defun ttuegel/set-font (frame)
  "Configure font on frame creation"
  (select-frame frame)
  (when (display-graphic-p)
    (set-frame-font "Monospace-12")))

;; Set font in all extant frames.
(mapc #'ttuegel/set-font (frame-list))

;; Set font in all future frames.
(add-hook 'after-make-frame-functions #'ttuegel/set-font)


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
