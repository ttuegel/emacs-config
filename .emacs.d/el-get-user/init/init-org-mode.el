;; Automatically load org-mode when opening *.org files
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

(eval-after-load 'org
  '(progn
     (global-set-key (kbd "C-c l") 'org-store-link)
     (global-set-key (kbd "C-c a") 'org-agenda)
     (setq org-agenda-files '("~/org"))
     (setq org-clock-persist 'history)
     (org-clock-persistence-insinuate)
     (setq org-log-done t)
     (add-hook 'org-mode-hook 'auto-fill-mode)))
