(eval-after-load 'haskell-mode
  '(progn
     (require 'fill-column-indicator)
     (setq haskell-mode-hook nil)  ; Fuck you, el-get!
     (add-hook 'haskell-mode-hook (lambda () (linum-mode 1)))
     (add-hook 'haskell-mode-hook (lambda () (rainbow-delimiters-mode 1)))
     (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
     (add-hook 'haskell-mode-hook 'turn-on-fci-mode)
     (add-hook 'haskell-mode-hook (lambda () (setq fill-column 80)))

     (add-hook 'electric-indent-functions
               (lambda (c) (when (or (eq 'haskell-mode major-mode)
                                     (eq 'haskell-cabal-mode major-mode))
                             'no-indent)))))

;; Alignment rules for Haskell
(eval-after-load 'align
  '(progn
     (add-to-list 'align-rules-list
                  '(haskell-types
                    (regexp . "\\(\\s-+\\)::\\s-+")
                    (modes '(haskell-mode literate-haskell-mode))))
     (add-to-list 'align-rules-list
                  '(haskell-assignment
                    (regexp . "\\(\\s-+\\)=\\s-+")
                    (modes '(haskell-mode literate-haskell-mode))))
     (add-to-list 'align-rules-list
                  '(haskell-arrows
                    (regexp . "\\(\\s-+\\)->\\s-+")
                    (modes '(haskell-mode literate-haskell-mode))))
     (add-to-list 'align-rules-list
                  '(haskell-left-arrows
                    (regexp . "\\(\\s-+\\)<-\\s-+")
                    (modes '(haskell-mode literate-haskell-mode))))))
