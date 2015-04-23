(setq reftex-label-alist '(("dmath" ?e nil nil t)))
(TeX-add-style-hook
  "dmath"
  (lambda () (LaTeX-add-environments '("dmath" LaTeX-env-label))))
