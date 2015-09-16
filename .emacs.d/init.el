;;; init.el -- ttuegel's configuration file
;;; Commentary:
;;; Code:

(require 'package)

(package-initialize)

(require 'use-package)
(require 'diminish)
(require 'bind-key)

(add-to-list 'load-path "~/.emacs.d/")

;; Use UTF-8 everywhere
(mapc (lambda (fn) (funcall fn 'utf-8))
      '(set-terminal-coding-system
        set-keyboard-coding-system
        prefer-coding-system))

;; Turn off that damn bell!
(setq visible-bell t)

;; Don't litter annoying backups everywhere
(setq auto-save-default nil)
(setq make-backup-files nil)

;; End GUI silliness
(blink-cursor-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(custom-theme-set-variables
 'user
 '(inhibit-startup-screen t)
 '(x-select-enable-clipboard nil)
 '(default-frame-alist '((cursor-color . "white"))))

;; Set color scheme
(custom-theme-set-variables 'user '(custom-safe-themes t))
(use-package monokai-theme
  :demand t
  :config
  (load-theme 'monokai))

(defalias 'yes-or-no-p 'y-or-n-p)

;;; Config

;; Because emacs stops processing when it encounters an error, the sections of
;; this config are ordered to leave me with the most functional environment
;; possible if any errors occur.

;; Customization
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(completion-ignored-extensions
   (quote
    (".o" "~" ".bin" ".lbin" ".so" ".a" ".ln" ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".svn/" ".hg/" ".git/" ".bzr/" "CVS/" "_darcs/" "_MTN/" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".dfsl" ".pfsl" ".d64fsl" ".p64fsl" ".lx64fsl" ".lx32fsl" ".dx64fsl" ".dx32fsl" ".fx64fsl" ".fx32fsl" ".sx64fsl" ".sx32fsl" ".wx64fsl" ".wx32fsl" ".fasl" ".ufsl" ".fsl" ".dxl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo" ".hi" ".elc"))))

(custom-theme-set-faces
 'user
 '(default ((t (:family "Source Code Pro"))))
 '(font-lock-type-face ((t :slant normal))))

;; tab stop settings
(custom-theme-set-variables
 'user
 '(tab-always-indent t)
 '(tab-stop-list (number-sequence 2 120 2))
 '(tab-width 2)
 '(indent-tabs-mode nil))

;; whitespace-mode

(custom-theme-set-variables 'user '(whitespace-style '(face trailing tabs)))
(global-whitespace-mode t)
(diminish 'global-whitespace-mode)

(show-paren-mode t)
(electric-indent-mode t)

;; Make buffer names more unique
(use-package uniquify
  :demand t
  :config
  (custom-theme-set-variables 'user '(uniquify-buffer-name-style 'forward)))

(use-package evil-leader
  :commands (global-evil-leader-mode)
  :config
  (evil-leader/set-leader "<SPC>"))
(global-evil-leader-mode)

;; Be evil
(use-package evil
  :demand t
  :config
  (custom-theme-set-variables 'user '(evil-shift-width 2))

  (evil-mode t)

  (evil-define-command ttuegel/evil-shift-line (count &optional left)
    "Shift the current line right COUNT times (left if LEFT is non-nil).
  The line is shifted to the nearest tab stop. Unlike `evil-shift-right-line', the
  value of `evil-shift-width' is ignored for better emacs interoperability. Works
  even when the line is blank."
    (interactive "<c>")
    (let* ((initial-column (current-column))
           (initial-indent (current-indentation))
           (final-indent (indent-next-tab-stop initial-indent left))
           (delta-indent (- final-indent initial-indent))
           (final-column (+ initial-column delta-indent)))
      (progn
        (indent-line-to final-indent)
        (forward-char (- final-column (current-column))))))

  (evil-define-command evil-shift-right-line (count)
    (interactive "<c>")
    (ttuegel/evil-shift-line count))

  (evil-define-command evil-shift-left-line (count)
    (interactive "<c>")
    (ttuegel/evil-shift-line count 1)))

(defun evil-map (key def &rest bindings)
  (evil-leader--def-keys evil-normal-state-map key def bindings)
  (evil-leader--def-keys evil-visual-state-map key def bindings)
  (evil-leader--def-keys evil-motion-state-map key def bindings)
  (evil-leader--def-keys evil-operator-state-map key def bindings))

;;; Up/down/left/right
(evil-map "t" 'evil-previous-visual-line
          "h" 'evil-next-visual-line
          "d" 'evil-backward-char
          "n" 'evil-forward-char)
(global-set-key (kbd "C-t") 'previous-line)
(global-set-key (kbd "C-h") 'next-line)
(global-set-key (kbd "C-d") 'backward-char)
(global-set-key (kbd "C-n") 'forward-char)

;;; Beginning/end of line (home/end)
;; Use back-to-indentation instead of evil-beginning-of-line so that
;; cursor ends up at the first non-whitespace character of a line. 0
;; can be used to go to real beginning of line
(evil-map "_" 'back-to-indentation
          "-" 'evil-end-of-line)

;; Scrolling
(evil-map "\M-t" 'evil-scroll-page-up
          "\M-h" 'evil-scroll-page-down
          "T" 'evil-scroll-up
          "H" 'evil-scroll-down)

;; Execute command: map : to ;
(evil-map "s" 'evil-ex)

;;; Cut/copy/paste
(evil-map "k" 'evil-delete)

;;; Undo Tree
(use-package undo-tree
  :commands (undo-tree-undo undo-tree-redo)
  :init
  (evil-map "u" 'undo-tree-undo
            "U" 'undo-tree-redo))

;;; Helm
(use-package helm-config)

(use-package helm
  :config
  (define-key helm-map (kbd "C-h") nil)
  (define-key helm-map (kbd "C-h") 'helm-next-line)
  (define-key helm-map (kbd "C-t") 'helm-previous-line)
  (define-key helm-map (kbd "C-n") 'helm-execute-persistent-action))

(use-package helm-files
  :config
  (define-key helm-read-file-map (kbd "C-d") 'helm-find-files-up-one-level)
  (define-key helm-find-files-map (kbd "C-d") 'helm-find-files-up-one-level)
  (define-key helm-command-map "b" 'helm-buffers-list))

(helm-mode 1)
(diminish 'helm-mode)

;;; Ace Jump
(evil-map "M-f" 'evil-ace-jump-word-mode)
(evil-map "M-F" 'evil-ace-jump-line-mode)

;;; Search
(evil-map "/" 'isearch-forward
          "?" 'isearch-backward
          "l" 'isearch-repeat-forward
          "L" 'isearch-repeat-backward)

(global-set-key (kbd "C-c /") 'isearch-forward)
(global-set-key (kbd "C-c ?") 'isearch-backward)
(global-set-key (kbd "C-c l") 'isearch-repeat-forward)
(global-set-key (kbd "C-c L") 'isearch-repeat-backward)

(define-key ctl-x-map "w" 'evil-window-map)
(evil-leader/set-key "w" 'evil-window-map)
(define-key evil-window-map "d" 'evil-window-left)
(define-key evil-window-map "D" 'evil-window-move-far-left)
(define-key evil-window-map "h" 'evil-window-down)
(define-key evil-window-map "H" 'evil-window-move-very-bottom)
(define-key evil-window-map "t" 'evil-window-up)
(define-key evil-window-map "T" 'evil-window-move-very-top)
(define-key evil-window-map "n" 'evil-window-right)
(define-key evil-window-map "N" 'evil-window-move-far-right)
(define-key evil-window-map "w" 'evil-window-vnew)
(define-key evil-window-map "W" 'evil-window-new)
(define-key evil-window-map "k" 'evil-window-delete)

(evil-leader/set-key "C-." 'helm-M-x)
(global-set-key (kbd "C-.") 'helm-M-x)

(evil-leader/set-key "z" ctl-x-map)
(define-key ctl-x-map (kbd "C-z") 'helm-find-files)
(define-key ctl-x-map (kbd "C-h") help-map)

(evil-leader/set-key "h" helm-command-map)

(evil-map "C-," 'evil-emacs-state)
(global-set-key (kbd "C-,") 'evil-exit-emacs-state)

(evil-leader/set-key "TAB"
  (lambda ()
    (interactive)
    (switch-to-buffer (other-buffer (current-buffer) t))))

(evil-leader/set-key
  "bb" 'helm-buffers-list
  "bk" 'kill-buffer
  "bK" 'kill-other-buffers
  "bn" 'switch-to-next-buffer
  "bp" 'switch-to-prev-buffer
  "bR" (lambda () (interactive) (revert-buffer nil t))
  "br" 'rename-current-buffer-file
  "bs" 'switch-to-buffer)

(evil-leader/set-key "jk" 'evil-join)

(evil-leader/set-key
  "en" 'next-error
  "ep" 'previous-error)

;; Use my development helm version, if present
(when (file-exists-p "~/.emacs.d/helm")
  (add-to-list 'load-path "~/.emacs.d/helm"))

(use-package magit
  :commands (magit-status)
  :init (evil-leader/set-key "gs" 'magit-status))

(use-package git-timemachine
  :commands (git-timemachine)
  :init (evil-leader/set-key "gt" 'git-timemachine-toggle))

(use-package org
  :commands (org-agenda org-capture)
  :defines (org-capture-templates)
  :init
  (custom-theme-set-variables
   'user
   '(org-modules
     '(org-bbdb org-bibtex org-docview org-gnus org-info org-jsinfo org-habit
                org-irc org-mew org-mhe org-rmail org-vm org-wl org-w3m)))
  (evil-leader/set-key
    "oa" 'org-agenda
    "oc" 'org-capture)
  :config
  (custom-theme-set-variables
   'user
   '(org-capture-templates
     '(("n" "Notes" entry
        (file+datetree "~/org/notes.org")
        "* %?
  %U
  %i
  %a")
       ("o" "Open Source Software" entry
        (file+datetree "~/org/oss.org")
        "* %?
  %U
  %i
  %a")
       ("p" "Physics" entry
        (file+datetree "~/org/physics.org")
        "* %?
  %U
  %i
  %a")
       ("t" "Todo" entry
        (file "~/org/todo.org")
        "* TODO %?
  %i
  %a")))
   '(org-default-notes-file "~/org/notes.org")
   '(org-hide-leading-stars t)
   '(org-agenda-files '("~/org"))
   '(org-clock-persist 'history)
   '(org-log-done t))
  (custom-theme-set-faces 'user
   '(org-level-1 ((t (:inherit nil :foreground "#FD971F" :height 1.0))))
   '(org-level-2 ((t (:inherit nil :foreground "#A6E22E" :height 1.0))))
   '(org-level-3 ((t (:inherit nil :foreground "#66D9EF" :height 1.0))))
   '(org-level-4 ((t (:inherit nil :foreground "#E6DB74" :height 1.0))))
   '(org-level-5 ((t (:inherit nil :foreground "#A1EFE4"))))
   '(org-level-6 ((t (:inherit nil :foreground "#A6E22E"))))
   '(org-level-7 ((t (:inherit nil :foreground "#F92672"))))
   '(org-level-8 ((t (:inherit nil :foreground "#66D9EF")))))
  (org-clock-persistence-insinuate)
  (add-hook 'org-mode-hook 'auto-fill-mode)
  (global-set-key (kbd "C-c l") 'org-store-link)
  (global-set-key (kbd "C-c a") 'org-agenda)
  (evil-leader/set-key
    "os" 'org-save-all-org-buffers))

(defadvice newline (after indent-clean-after-newline activate)
  "Stop ill-behaved major-modes from leaving indentation on blank lines.
After a newline, remove whitespace from the previous line if that line is
only whitespace."
  (progn
    (forward-line -1)
    (beginning-of-line)
    (while
        (re-search-forward "^[[:space:]]+$" (line-end-position) t)
      (replace-match ""))
    (forward-line 1)
    (back-to-indentation)))

(use-package undo-tree
  :commands (global-undo-tree-mode)
  :diminish undo-tree-mode)
(global-undo-tree-mode 1)

(use-package company
  :commands (global-company-mode)
  :diminish company-mode
  :config
  (progn
    (define-key company-active-map (kbd "C-h") 'company-select-next)
    (define-key company-active-map (kbd "C-t") 'company-select-previous)))
(global-company-mode)

;;; rainbow-delimiters

(use-package rainbow-delimiters
  :commands (rainbow-delimiters-mode))

(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)

;;; AucTeX

(defvar ttuegel/LaTeX-no-autofill-environments
  '("align" "align*" "equation" "equation*")
  "A list of LaTeX environment names in which `auto-fill-mode' should be inhibited.")

(defun ttuegel/LaTeX-auto-fill-function ()
  "This function checks whether point is currently inside one of
the LaTeX environments listed in
`ttuegel/LaTeX-no-autofill-environments'. If so, it inhibits automatic
filling of the current paragraph."
  (let ((do-auto-fill t)
        (current-environment "")
        (level 0))
    (while (and do-auto-fill (not (string= current-environment "document")))
      (setq level (1+ level)
            current-environment (LaTeX-current-environment level)
            do-auto-fill (not (member current-environment ttuegel/LaTeX-no-autofill-environments))))
    (when do-auto-fill
      (do-auto-fill))))

(defun ttuegel/LaTeX-setup-auto-fill ()
  "This function turns on auto-fill-mode and sets the function
used to fill a paragraph to `ttuegel/LaTeX-auto-fill-function'."
  (auto-fill-mode)
  (setq auto-fill-function 'ttuegel/LaTeX-auto-fill-function))

(use-package tex-site ; auctex
  :mode ("\\.\\(tex\\|sty\\|cls\\)\\'" . latex-mode)
  :commands (latex-mode LaTeX-mode plain-tex-mode)
  :config
  (progn
    (custom-theme-set-variables
     'user
     '(font-latex-fontify-script nil)
     '(font-latex-fontify-sectioning 'color)
     '(font-latex-math-environments
       '("display" "displaymath" "equation" "eqnarray" "gather" "multline" "align"
         "alignat" "xalignat" "dmath" "math")))

    (add-hook 'LaTeX-mode-hook
              (lambda ()
                (ttuegel/LaTeX-setup-auto-fill)
                (flyspell-mode 1)))))

;;; flycheck

(use-package flycheck
  :commands (flycheck-mode)
  :config
  (setq flycheck-checkers (delq 'haskell-hlint flycheck-checkers)))

;;; ghc-mod

(use-package ghc
  :commands (ghc-init ghc-debug)
  :defines (ghc-sort-key)
  :config
  (progn
    (custom-theme-set-variables 'user '(ghc-sort-key nil))
    (add-to-list 'company-backends 'company-ghc)))

;;; haskell-mode

(use-package haskell-mode
  :commands (haskell-mode haskell-cabal-mode)
  :defines (haskell-indentation-cycle-warn haskell-indentation-starter-offset)
  :config
  (progn
    (custom-theme-set-variables
     'user
     '(haskell-literate-default 'tex)
     '(haskell-process-auto-import-loaded-modules nil)
     '(haskell-process-log t)
     '(haskell-process-suggest-remove-import-lines nil)
     '(haskell-process-type 'cabal-repl))
    (add-hook 'haskell-mode-hook (lambda () (linum-mode 1)))
    (add-hook 'haskell-mode-hook #'rainbow-delimiters-mode)
    (add-hook 'haskell-mode-hook
              (lambda ()
                (turn-on-haskell-indentation)
                (setq haskell-indentation-cycle-warn nil)
                (setq haskell-indentation-starter-offset 2)))
    (add-hook 'electric-indent-functions
              (lambda (c) (when (or (eq 'haskell-mode major-mode)
                                    (eq 'haskell-cabal-mode major-mode))
                            'no-indent)))))

;;; nix-mode

(use-package nix-mode
  :mode ("\\.nix\\'" . nix-mode)
  :commands (nix-mode))

;(require 'evil-surround)
;(global-evil-surround-mode 1)

;(require 'evil-indent-textobject)

(defun byte-compile-current-buffer ()
  "`byte-compile' current buffer if it's emacs-lisp-mode and compiled file exists."
  (interactive)
  (when (and (eq major-mode 'emacs-lisp-mode)
             (file-exists-p (byte-compile-dest-file buffer-file-name)))
    (byte-compile-file buffer-file-name)))

(add-hook 'after-save-hook 'byte-compile-current-buffer)

(use-package ledger-mode
  :mode ("\\.ledger\\'" . ledger-mode))

(use-package julia-mode
  :mode ("\\.jl\\'" . julia-mode))

(provide 'init)
;;; init.el ends here
