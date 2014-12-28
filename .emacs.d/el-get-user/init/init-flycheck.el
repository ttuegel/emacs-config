(eval-after-load 'flycheck
  '(progn
     (setq flycheck-checkers (delq 'haskell-hlint flycheck-checkers))
     (add-hook 'after-init-hook #'global-flycheck-mode)))
