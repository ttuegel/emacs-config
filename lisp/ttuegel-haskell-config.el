;;; ttuegel-haskell-config.el -- ttuegel's Haskell configuration
;;; Commentary:
;;; Code:

(add-to-list 'load-path "~/.emacs.d/hs-indent")
(require 'hs-indent)

; Disable the HLint flycheck checker.
(require 'flycheck)
(setq flycheck-checkers (delq 'haskell-hlint flycheck-checkers))

; Enable ghc-mod.
(require 'ghc)
(custom-theme-set-variables 'user '(ghc-sort-key nil))

(require 'haskell-mode)

(custom-theme-set-variables
  'user
  '(haskell-literate-default 'tex)
  '(haskell-process-auto-import-loaded-modules nil)
  '(haskell-process-log t)
  '(haskell-process-suggest-remove-import-lines nil)
  '(haskell-process-type 'cabal-repl))

(add-hook 'haskell-mode-hook #'rainbow-delimiters-mode)
(add-hook 'haskell-mode-hook #'yas-minor-mode)
(add-hook 'haskell-mode-hook #'turn-on-hs-indent)

(provide 'ttuegel-haskell-config)
;;; ttuegel-haskell-config.el ends here
