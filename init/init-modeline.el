(require 'spaceline-config)


;; Evil state colors
(setq spaceline-highlight-face-func #'spaceline-highlight-face-evil-state)


(spaceline-emacs-theme)


(spaceline-toggle-buffer-size-off)
(spaceline-toggle-buffer-encoding-abbrev-off)


(provide 'init-modeline)
