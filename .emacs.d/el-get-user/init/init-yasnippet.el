(eval-after-load 'yasnippet
  '(progn
     (setq-default yas-prompt-functions '(yas-ido-prompt yas-dropdown-prompt))
     (add-hook 'after-init-hook (lambda () (yas-global-mode t)))
     (diminish 'yas-minor-mode)))
