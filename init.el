;;; init.el -- ttuegel's configuration file
;;; Commentary:
;;; Code:

(eval-when-compile (require 'use-package))

(require 'diminish)
(require 'bind-key)

(add-to-list 'load-path "~/.emacs.d/lisp")

;;; Emacs settings

;; Use UTF-8 everywhere. It's 2016, how is this not default?
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Turn off that damn bell!
(customize-set-variable 'visible-bell t)

;; Don't piddle backup files everywhere like an un-housebroken puppy.
(customize-set-variable 'auto-save-default nil)
(customize-set-variable 'make-backup-files nil)

;; Blinking should be reserved for eyelids and indicators that require immediate attention.
(customize-set-variable 'blink-cursor-mode nil)

;; Make the cursor white by default.
(customize-set-variable 'default-frame-alist '((cursor-color . "white")))

;; What is this, Microsoft Word?
(menu-bar-mode -1)
(tool-bar-mode -1)

;; Thank you, but I know what program this is.
(customize-set-variable 'inhibit-startup-screen t)

;; Don't update the X selection from the kill-ring.
;; Like Vim, Evil keeps the X selection in the `"' register.
(customize-set-variable 'x-select-enable-clipboard nil)

;; Ask `y or n' rather than `yes or no'
(defalias 'yes-or-no-p 'y-or-n-p)

;; Set color scheme
(customize-set-variable 'custom-safe-themes t)
(use-package monokai-theme
  :config
  (load-theme 'monokai)
  :demand)

;; Ignore common extensions.
(customize-set-variable
 'completion-ignored-extensions
 '(".o" "~" ".bin" ".lbin" ".so" ".a" ".ln" ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".svn/" ".hg/" ".git/" ".bzr/" "CVS/" "_darcs/" "_MTN/" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".dfsl" ".pfsl" ".d64fsl" ".p64fsl" ".lx64fsl" ".lx32fsl" ".dx64fsl" ".dx32fsl" ".fx64fsl" ".fx32fsl" ".sx64fsl" ".sx32fsl" ".wx64fsl" ".wx32fsl" ".fasl" ".ufsl" ".fsl" ".dxl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo" ".hi" ".elc"))

;; Fonts
(custom-theme-set-faces
 'user

 ;; Use Source Code Pro font by default.
 '(default ((t (:family "Source Code Pro"))))

 ;; Don't use italics to indicate types.
 '(font-lock-type-face ((t :slant normal))))

;; Tabs
(customize-set-variable 'tab-always-indent t)
(customize-set-variable 'tab-stop-list (number-sequence 2 120 2))
(customize-set-variable 'tab-width 2)
(customize-set-variable 'indent-tabs-mode nil)

;; Fill column
(customize-set-variable 'fill-column 80)

;; Whitespace
(customize-set-variable 'whitespace-style '(face trailing tabs))
(global-whitespace-mode t)
(diminish 'global-whitespace-mode)

(defun ttuegel/indent-whitespace-hygiene ()
  "Remove whitespace from the current line if it is only whitespace."
  (save-excursion
    (beginning-of-line)
    (while
        (re-search-forward "^[[:space:]]+$" (line-end-position) t)
      (replace-match ""))))

(defadvice newline (after indent-whitespace-hygiene-after-newline activate)
  "Stop ill-behaved major-modes from leaving indentation on blank lines.
After a newline, remove whitespace from the previous line if that line is
only whitespace."
  (progn
    (forward-line -1)
    (ttuegel/indent-whitespace-hygiene)
    (forward-line 1)
    (back-to-indentation)))

;; Parentheses
(show-paren-mode t)

;; Automatic indentation
(electric-indent-mode t)

;; Mode line

(defun ttuegel/mode-line-buffer-modified ()
  "Mode line indicator that the buffer is modified"
  (if (buffer-modified-p) "[*]" "   "))

(defun ttuegel/mode-line-position ()
  "Mode line position indicator"
  (let ((unpadded (format " (%4s, %2s) "
                          (format-mode-line "%l")
                          (format-mode-line "%c"))))
    (format "%15s" unpadded)))

(customize-set-variable
 'mode-line-format
 '("%e"
   mode-line-front-space
   mode-line-remote
   mode-line-frame-identification
   mode-line-buffer-identification
   "   "
   (:eval (ttuegel/mode-line-buffer-modified))
   (:eval (ttuegel/mode-line-position))
   mode-line-modes
   (vc-mode vc-mode)
   "  "
   mode-line-misc-info
   mode-line-end-spaces))

;;; Required Packages

;; Make buffer names more unique
(use-package uniquify :demand
  :config
  (customize-set-variable 'uniquify-buffer-name-style 'forward))

;; Be evil
(use-package evil-leader :demand
  :config
  (evil-leader/set-leader "C-SPC"))
(global-evil-leader-mode)

(bind-key "C-SPC" ctl-x-map)

(use-package evil :demand
  :config

  ;; Whitespace
  (defadvice evil-normal-state
      (after indent-whitespace-hygiene-after-evil-normal-state activate)
    (ttuegel/indent-whitespace-hygiene))

  ;; Custom tags
  (defun ttuegel/evil-tag-props (props)
    "Text properties for my custom Evil tags"
    (setq props (plist-put props :weight 'bold))
    (setq props (plist-put props :foreground "#272822"))
    props)

  (defmacro ttuegel/set-evil-tag (var tag props)
    "Set a custom Evil tag with my own text properties."
    `(setq ,var (propertize ,tag 'face (quote ,(ttuegel/evil-tag-props props)))))

  (ttuegel/set-evil-tag evil-normal-state-tag " N " (:background "#A6E22E"))
  (ttuegel/set-evil-tag evil-emacs-state-tag " E " (:background "#FD971F"))
  (ttuegel/set-evil-tag evil-insert-state-tag " I " (:background "#F92672"))
  (ttuegel/set-evil-tag evil-motion-state-tag " M " (:background "#66D9EF"))
  (ttuegel/set-evil-tag evil-visual-state-tag " V " (:background "#6b6b6b"))
  (ttuegel/set-evil-tag evil-operator-state-tag " O " (:background "#AE81FF"))
  (ttuegel/set-evil-tag evil-replace-state-tag " R " (:background "#FD5FF0"))

  ;; Use C-<return> instead of ESC
  (define-key evil-insert-state-map (kbd "C-<return>") 'evil-normal-state)
  (define-key evil-emacs-state-map (kbd "C-<return>") 'evil-normal-state)
  (define-key evil-motion-state-map (kbd "C-<return>") 'evil-normal-state)
  (define-key evil-operator-state-map (kbd "C-<return>") 'evil-normal-state)
  (define-key evil-replace-state-map (kbd "C-<return>") 'evil-normal-state)
  (define-key evil-visual-state-map (kbd "C-<return>") 'evil-normal-state)

  (defun ttuegel/evil-map (key def &rest bindings)
    (evil-leader--def-keys evil-normal-state-map key def bindings)
    (evil-leader--def-keys evil-visual-state-map key def bindings)
    (evil-leader--def-keys evil-motion-state-map key def bindings)
    (evil-leader--def-keys evil-operator-state-map key def bindings))

  ;; Vim motion keys
  (ttuegel/evil-map "t" 'evil-previous-visual-line
                    "h" 'evil-next-visual-line
                    "d" 'evil-backward-char
                    "n" 'evil-forward-char)

  ;; Emacs-mode equivalent of Vim motion keys
  (global-set-key (kbd "C-t") 'previous-line)
  (global-set-key (kbd "C-h") 'next-line)
  (global-set-key (kbd "C-d") 'backward-char)
  (global-set-key (kbd "C-n") 'forward-char)

  ;; Beginning/end of line (home/end)
  ;; Use back-to-indentation instead of evil-beginning-of-line so that
  ;; cursor ends up at the first non-whitespace character of a line. 0
  ;; can be used to go to real beginning of line
  (ttuegel/evil-map "_" 'back-to-indentation
                    "-" 'evil-end-of-line)

  ;; Scrolling
  (ttuegel/evil-map "\M-t" 'evil-scroll-page-up
                    "\M-h" 'evil-scroll-page-down
                    "T" 'evil-scroll-up
                    "H" 'evil-scroll-down)

  (ttuegel/evil-map "k" 'evil-delete)

  ;; Search
  (ttuegel/evil-map "/" 'isearch-forward
                    "?" 'isearch-backward
                    "l" 'isearch-repeat-forward
                    "L" 'isearch-repeat-backward)

  (global-set-key (kbd "C-c /") 'isearch-forward)
  (global-set-key (kbd "C-c ?") 'isearch-backward)
  (global-set-key (kbd "C-c l") 'isearch-repeat-forward)
  (global-set-key (kbd "C-c L") 'isearch-repeat-backward)

  ;; Windows
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

  (evil-leader/set-key "TAB"
    (lambda ()
      (interactive)
      (switch-to-buffer (other-buffer (current-buffer) t))))

  ;; C-x map
  (evil-leader/set-key "C-SPC" ctl-x-map)

  ;; Emacs state
  (ttuegel/evil-map "C-," 'evil-emacs-state)
  (global-set-key (kbd "C-,") 'evil-exit-emacs-state)

  ;; Buffers
  (evil-leader/set-key
    "bK" 'kill-other-buffers
    "bn" 'switch-to-next-buffer
    "bp" 'switch-to-prev-buffer
    "bR" (lambda () (interactive) (revert-buffer nil t))
    "br" 'rename-current-buffer-file)

  ;; Errors
  (evil-leader/set-key
    "en" 'next-error
    "ep" 'previous-error)

  (customize-set-variable 'evil-mode-line-format '(before . mode-line-front-space))
  (customize-set-variable 'evil-shift-width tab-width))
(evil-mode t)

(use-package evil-surround)
(global-evil-surround-mode 1)

(use-package evil-indent-textobject :demand)

;; Undo Tree
(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode 1)
  (ttuegel/evil-map "u" 'undo-tree-undo
                    "U" 'undo-tree-redo)
  :demand)

;; Helm

;; Use my development helm version, if present
(when (file-exists-p "~/.emacs.d/helm")
  (add-to-list 'load-path "~/.emacs.d/helm"))

(use-package helm :demand
  :config
  (require 'helm-config)
  (customize-set-variable 'helm-ff-skip-boring-files t)

  (bind-key "M-SPC" 'helm-M-x)
  (evil-leader/set-key "h" helm-command-map)

  (bind-key "C-h" 'helm-next-line helm-map)
  (bind-key "C-t" 'helm-previous-line helm-map))
(helm-mode 1)
(diminish 'helm-mode)

(use-package helm-files :demand
  :config
  (bind-key "C-f" 'helm-find-files helm-command-map)
  (bind-key "C-d" 'helm-find-files-up-one-level helm-read-file-map)
  (bind-key "C-d" 'helm-find-files-up-one-level helm-find-files-map))

;; Avy
(use-package avy :demand
  :config
  (ttuegel/evil-map "f" 'avy-goto-char
                    "F" 'avy-goto-line))

;;; Optional Packages

;; Git

(use-package magit
  :config
  (diminish 'auto-revert-mode)
  (evil-leader/set-key "gs" 'magit-status))

(use-package git-timemachine
  :config
  (evil-leader/set-key "gt" 'git-timemachine-toggle))

;; Org

(use-package org
  :init
  (evil-leader/set-key
    "oa" 'org-agenda
    "oc" 'org-capture
    "os" 'org-save-all-org-buffers)
  :config
  (customize-set-variable
   'org-modules
   '(org-bbdb org-bibtex org-docview org-gnus org-info org-habit
              org-irc org-mew org-mhe org-rmail org-vm org-wl org-w3m))
  (customize-set-variable 'org-default-notes-file "~/org/notes.org")
  (customize-set-variable 'org-agenda-files '("~/org"))

  (custom-theme-set-faces
   'user
   '(org-level-1 ((t (:inherit nil :foreground "#FD971F" :height 1.0))))
   '(org-level-2 ((t (:inherit nil :foreground "#A6E22E" :height 1.0))))
   '(org-level-3 ((t (:inherit nil :foreground "#66D9EF" :height 1.0))))
   '(org-level-4 ((t (:inherit nil :foreground "#E6DB74" :height 1.0))))
   '(org-level-5 ((t (:inherit nil :foreground "#A1EFE4"))))
   '(org-level-6 ((t (:inherit nil :foreground "#A6E22E"))))
   '(org-level-7 ((t (:inherit nil :foreground "#F92672"))))
   '(org-level-8 ((t (:inherit nil :foreground "#66D9EF")))))

  ;; Cursor movement (agenda mode)
  (define-key org-agenda-mode-map (kbd "h") 'org-agenda-next-item)
  (define-key org-agenda-mode-map (kbd "t") 'org-agenda-previous-item)
  (define-key org-agenda-mode-map (kbd "C-h") 'org-agenda-next-date-line)
  (define-key org-agenda-mode-map (kbd "C-t") 'org-agenda-previous-date-line)
  (define-key org-agenda-mode-map (kbd "d") 'org-agenda-todo)

  (global-set-key (kbd "C-c l") 'org-store-link)

  (add-hook 'org-mode-hook 'auto-fill-mode))

;; Rainbow Delimiters

(use-package rainbow-delimiters)

;; TeX

(use-package tex-site
  :commands latex-mode
  :mode ("\\.\\(tex\\|sty\\|cls\\)\\'" . latex-mode)
  :config
  (customize-set-variable 'safe-local-variable-names '((TeX-command-extra-options . "-shell-escape")))
  (customize-set-variable 'font-latex-fontify-script nil)
  (customize-set-variable 'font-latex-fontify-sectioning 'color)
  (customize-set-variable
   'font-latex-math-environments
   '("display" "displaymath" "equation" "eqnarray" "gather" "multline" "align"
     "alignat" "xalignat" "dmath" "math"))
  (customize-set-variable 'TeX-auto-save t)
  (customize-set-variable 'TeX-parse-self t)
  (customize-set-variable 'TeX-save-query nil)
  (customize-set-variable 'TeX-source-correlate-mode t)
  (customize-set-variable
   'TeX-view-program-selection
   '(((output-dvi has-no-display-manager) "dvi2tty")
     (output-dvi "xdvi")
     (output-pdf "Okular")
     (output-html "xdg-open")))

   (add-hook 'LaTeX-mode-hook #'yas-minor-mode)
   (add-hook 'LaTeX-mode-hook (lambda () (flyspell-mode 1))))

;; Flycheck

(use-package flycheck
  :config
  (customize-set-variable 'flycheck-disabled-checkers '(haskell-hlint)))

;; Nix

(use-package nix-mode
  :mode ("\\.nix\\'" . nix-mode))

;; Ledger

(use-package ledger-mode
  :mode ("\\.ledger\\'" . ledger-mode))

;; Snippets

(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (customize-set-variable 'yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-reload-all))

;; Company

(use-package company :demand
  :diminish company-mode
  :bind (:map company-active-map
              ("C-h" . company-select-next)
              ("C-t" . company-select-previous)
              ("RET" . nil)))
(global-company-mode)

;; Emacs Lisp

(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)

(defun ttuegel/byte-compile-current-buffer ()
  "`byte-compile' current buffer if it's emacs-lisp-mode and compiled file exists."
  (interactive)
  (when (and (eq major-mode 'emacs-lisp-mode)
             (file-exists-p (byte-compile-dest-file buffer-file-name)))
    (byte-compile-file buffer-file-name)))

(add-hook 'after-save-hook 'ttuegel/byte-compile-current-buffer)

;; Haskell

(add-to-list 'load-path "~/.emacs.d/hs-indent")
(use-package hs-indent)

(use-package haskell-mode
  :config
  (customize-set-variable 'haskell-literate-default 'tex)
  (customize-set-variable 'haskell-auto-import-loaded-modules nil)
  (customize-set-variable 'haskell-process-log t)
  (customize-set-variable 'haskell-process-suggest-remove-import-lines nil)

  (add-hook 'haskell-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'haskell-mode-hook #'yas-minor-mode)
  (add-hook 'haskell-mode-hook #'turn-on-hs-indent))

(provide 'init)
;;; init.el ends here
