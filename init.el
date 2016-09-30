;;; init.el -- ttuegel's configuration file
;;; Commentary:
;;; Code:

(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("gnu" . "http://elpa.gnu.org/packages/")))
(setq package-archive-priorities
      '(("melpa-stable" . 1)))
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/use-package")

(eval-when-compile
  (require 'use-package))

(setq use-package-always-ensure t)

(use-package diminish :demand)
(use-package bind-key :demand)

(add-to-list 'load-path "~/.emacs.d/lisp")

;;; Emacs settings

;; Don't EVER touch my init.el!
(eval-after-load "cus-edit"
  '(defun customize-save-variable (variable value &optional comment) value))

(bind-key "C-z" ctl-x-map)

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

;; Make buffer names more unique
(customize-set-variable 'uniquify-buffer-name-style 'forward)

;; Set color scheme
(customize-set-variable 'custom-safe-themes t)
(use-package monokai-theme :demand
  :config
  (load-theme 'monokai))

;; Ignore common extensions.
(customize-set-variable
 'completion-ignored-extensions
 '(".o" "~" ".bin" ".lbin" ".so" ".a" ".ln" ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".svn/" ".hg/" ".git/" ".bzr/" "CVS/" "_darcs/" "_MTN/" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".dfsl" ".pfsl" ".d64fsl" ".p64fsl" ".lx64fsl" ".lx32fsl" ".dx64fsl" ".dx32fsl" ".fx64fsl" ".fx32fsl" ".sx64fsl" ".sx32fsl" ".wx64fsl" ".wx32fsl" ".fasl" ".ufsl" ".fsl" ".dxl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo" ".hi" ".elc"))

;;; Fonts

;; Use Source Code Pro font by default.

(custom-theme-set-faces
 'user
 '(default ((t (:family "Source Code Pro" :height 100)))))

;; Don't use italics to indicate types.
(custom-theme-set-faces 'user '(font-lock-type-face ((t :slant normal))))

;;; Tabs
(customize-set-variable 'tab-always-indent t)
(customize-set-variable 'tab-stop-list (number-sequence 2 120 2))
(customize-set-variable 'tab-width 2)
(customize-set-variable 'indent-tabs-mode nil)

;;; Fill column
(customize-set-variable 'fill-column 80)

;;; Whitespace
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

;;; Parentheses
(show-paren-mode t)
(use-package rainbow-delimiters)

;;; Automatic indentation
(electric-indent-mode t)

;;; Mode line

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

;;; Search

(defvar ttuegel/search-map (make-sparse-keymap))
(bind-key "C-f" ttuegel/search-map)
(bind-keys
 :map ttuegel/search-map
 ("f" . (lambda () (interactive) (isearch-forward t)))
 ("F" . (lambda () (interactive) (isearch-backward t))))

;;; Buffers

(defvar ttuegel/buffer-map (make-sparse-keymap))
(bind-key "b" ttuegel/buffer-map ctl-x-map)
(bind-keys
 :map ttuegel/buffer-map
 ("k" . kill-buffer)
 ("K" . kill-other-buffers)
 ("n" . switch-to-next-buffer)
 ("p" . switch-to-prev-buffer)
 ("R" . (lambda () (interactive) (revert-buffer nil t)))
 ("r" . rename-current-buffer-file))

;;; Errors

(defvar ttuegel/error-map (make-sparse-keymap))
(bind-key "e" ttuegel/error-map ctl-x-map)
(bind-keys
 :map ttuegel/error-map
 ("h" . next-error)
 ("t" . previous-error))

;;; Required Packages

(defun ttuegel/beginning-of-line ()
  "`beginning-of-line' if `back-to-indentation' does not move the cursor."
  (interactive)
  (let ((before (point)))
    (unless (eq before (line-beginning-position))
      (back-to-indentation)
      (let ((after (point)))
        (when (eq before after) (beginning-of-line))))))

;;; Be Evil
(use-package evil :demand
  :config

  ;; Emacs-mode equivalent of Vim motion keys
  (bind-keys
   ("C-t" . 'previous-line)
   ("C-h" . 'next-line)
   ("C-d" . 'backward-char)
   ("C-n" . 'forward-char)

   ("C-T" . evil-scroll-up)
   ("C-H" . evil-scroll-down)
   ("C-D" . beginning-of-visual-line)
   ("C-N" . end-of-visual-line)

   ("C-M-t" . evil-scroll-page-up)
   ("C-M-h" . evil-scroll-page-down))

  (customize-set-variable 'evil-toggle-key "C-,")

  ;; Windows
  (bind-key "w" evil-window-map ctl-x-map)
  (bind-keys
   :map evil-window-map
   ("d" . evil-window-left)
   ("D" . evil-window-move-far-left)
   ("h" . evil-window-down)
   ("H" . evil-window-move-very-bottom)
   ("t" . evil-window-up)
   ("T" . evil-window-move-very-top)
   ("n" . evil-window-right)
   ("N" . evil-window-move-far-right)
   ("w" . evil-window-new)
   ("W" . evil-window-vnew)
   ("k" . evil-window-delete))

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

  (customize-set-variable 'evil-mode-line-format '(before . mode-line-front-space))

  (bind-key "<C-return>" 'evil-normal-state)

  ;; Vim motion keys
  (bind-keys
   :map evil-motion-state-map
   ("t" . evil-previous-visual-line)
   ("h" . evil-next-visual-line)
   ("d" . evil-backward-char)
   ("n" . evil-forward-char)

   ("T" . evil-scroll-up)
   ("H" . evil-scroll-down)
   ("D" . ttuegel/beginning-of-line)
   ("N" . end-of-line)

   ("M-t" . evil-scroll-page-up)
   ("M-h" . evil-scroll-page-down))

  (unbind-key "j" evil-motion-state-map)
  (unbind-key "k" evil-motion-state-map)
  (unbind-key "l" evil-motion-state-map)

  (unbind-key "C-f" evil-motion-state-map)
  (unbind-key "C-b" evil-motion-state-map)
  (unbind-key "-" evil-motion-state-map)
  (unbind-key "+" evil-motion-state-map)

  (bind-keys
   :map evil-normal-state-map
   ("k" . evil-delete)
   ("K" . evil-delete-line))

  (unbind-key "d" evil-normal-state-map)
  (unbind-key "D" evil-normal-state-map)

  (customize-set-variable 'evil-shift-width tab-width))
(evil-mode t)

(use-package evil-surround :demand)
(global-evil-surround-mode 1)

(use-package evil-indent-textobject :demand)

;; Undo Tree
(use-package undo-tree :demand
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode 1)
  (bind-keys
   :map evil-normal-state-map
   ("u" . undo-tree-undo)
   ("U" . undo-tree-redo)))

;; Helm

(use-package helm-config :ensure helm :demand
  :config
  (require 'helm-files)
  (customize-set-variable 'helm-ff-skip-boring-files t)
  (customize-set-variable 'helm-split-window-in-side-p t)

  (bind-key "C-b" 'helm-buffers-list ctl-x-map)

  (bind-key "M-z" 'helm-M-x)
  (bind-key "C-h" helm-command-map ctl-x-map)
  (bind-key "C-f" 'helm-find-files ctl-x-map)

  (bind-key "C-h" 'helm-next-line helm-map)
  (bind-key "C-t" 'helm-previous-line helm-map)
  (bind-key "C-n" 'helm-execute-persistent-action helm-map)

  (bind-key "C-f" 'helm-find-files helm-command-map)
  (bind-key "C-d" 'helm-find-files-up-one-level helm-read-file-map)
  (bind-key "C-d" 'helm-find-files-up-one-level helm-find-files-map))
(helm-mode 1)
(diminish 'helm-mode)

;; Avy
(use-package avy :demand
  :init
  :bind (:map evil-motion-state-map
              ("f" . avy-goto-char)
              ("F" . avy-goto-line))
  :config
  (customize-set-variable 'avy-keys '(?a ?o ?e ?u ?h ?t ?n ?s)))

;;; Optional Packages

;; Git

(defvar ttuegel/vc-map (make-sparse-keymap))
(bind-key "g" ttuegel/vc-map ctl-x-map)

(use-package magit
  :config
  (diminish 'auto-revert-mode)
  (bind-key "s" 'magit-status ttuegel/vc-map))

(use-package git-timemachine
  :config
  (bind-key "t" 'git-timemachine-toggle ttuegel/vc-map))

;; TeX

(use-package tex-site :ensure auctex
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

;; Markdown

(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-mode))

;; Flycheck

(use-package flycheck
  :config
  (customize-set-variable 'flycheck-disabled-checkers '(haskell-hlint)))

;; Nix

(use-package nix-mode
  :mode ("\\.nix\\'" . nix-mode)
  :config
  (add-hook 'nix-mode-hook #'rainbow-delimiters-mode))

;; Ledger

(use-package ledger-mode
  :mode ("\\.ledger\\'" . ledger-mode))

;; YAML

(use-package yaml-mode
  :mode ("\\.\\(yml\\|yaml\\)\\'" . yaml-mode))

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

(use-package intero)

(use-package haskell-mode
  :config
  (customize-set-variable 'haskell-literate-default 'tex)
  (customize-set-variable 'haskell-auto-import-loaded-modules nil)
  (customize-set-variable 'haskell-process-log t)
  (customize-set-variable 'haskell-process-suggest-remove-import-lines nil)

  (add-hook 'haskell-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'haskell-mode-hook #'intero-mode))

;; XML
(add-to-list 'auto-mode-alist '("\\.rng\\'" . xml-mode))

;; Zotero integration

(use-package zotelo
  :config
  (add-hook 'TeX-mode-hook #'zotelo-minor-mode)
  (add-hook 'org-mode-hook #'zotelo-minor-mode))

;; org-mode
(use-package org
  :config
  (customize-set-variable 'org-catch-invisible-edits 'show))

(provide 'init)
;;; init.el ends here
