(defvar my-LaTeX-no-autofill-environments
  '("align" "align*" "equation" "equation*")
  "A list of LaTeX environment names in which `auto-fill-mode' should be inhibited.")

(defun my-LaTeX-auto-fill-function ()
  "This function checks whether point is currently inside one of
the LaTeX environments listed in
`my-LaTeX-no-autofill-environments'. If so, it inhibits automatic
filling of the current paragraph."
  (let ((do-auto-fill t)
        (current-environment "")
        (level 0))
    (while (and do-auto-fill (not (string= current-environment "document")))
      (setq level (1+ level)
            current-environment (LaTeX-current-environment level)
            do-auto-fill (not (member current-environment my-LaTeX-no-autofill-environments))))
    (when do-auto-fill
      (do-auto-fill))))

(defun my-LaTeX-setup-auto-fill ()
  "This function turns on auto-fill-mode and sets the function
used to fill a paragraph to `my-LaTeX-auto-fill-function'."
  (auto-fill-mode)
  (setq auto-fill-function 'my-LaTeX-auto-fill-function))

(eval-after-load 'tex
  '(progn
     (setq reftex-label-alist '(("dmath" ?e nil nil t)))
     (TeX-add-style-hook
      "dmath"
      (lambda () (LaTeX-add-environments '("dmath" LaTeX-env-label))))
     (add-hook 'LaTeX-mode-hook 'my-LaTeX-setup-auto-fill)
     (flyspell-mode 1)
     ))
