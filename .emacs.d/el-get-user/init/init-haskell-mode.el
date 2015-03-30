(eval-after-load 'haskell-mode
  '(progn
     (add-hook 'haskell-mode-hook (lambda () (linum-mode 1)))
     (add-hook 'haskell-mode-hook (lambda () (rainbow-delimiters-mode 1)))
     (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

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
