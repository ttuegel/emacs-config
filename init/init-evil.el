(setq-default evil-shift-width tab-width)

(evil-set-initial-state 'comint-mode 'emacs)
(evil-set-initial-state 'text-mode 'normal)
(evil-set-initial-state 'git-commit-mode 'normal)


(evil-mode t)


(require 'evil-surround)
(global-evil-surround-mode 1)


(require 'evil-indent-textobject)


(provide 'init-evil)
