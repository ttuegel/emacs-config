(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)

(eval-after-load 'ghc
  '(progn
     (setq ghc-sort-key nil)
     (add-to-list 'company-backends 'company-ghc)))
