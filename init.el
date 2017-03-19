;;; init.el -- ttuegel's configuration file
;;; Commentary:
;;; Code:

(add-to-list 'exec-path "/nix/var/nix/profiles/per-user/ttuegel/profile/bin")

;; Load the .el if it's newer than the .elc
(setq load-prefer-newer t)

(package-initialize)

(eval-when-compile (require 'use-package))
(require 'diminish)
(require 'bind-key)

(setq use-package-always-demand t)

(use-package auto-compile
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

;;; Emacs settings

;; Don't EVER touch my init.el!
(eval-after-load "cus-edit"
  '(defun customize-save-variable
       (variable value &optional comment) value))

(bind-key "C-s" ctl-x-map)

;; Use UTF-8 everywhere. It's 2017, how is this not default?
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Turn off that damn bell!
(setq visible-bell t)

;; Don't piddle backup files everywhere like an un-housebroken puppy.
(setq auto-save-default nil)
(setq make-backup-files nil)

;; Blinking should be reserved for eyelids and indicators that require immediate attention.
(blink-cursor-mode -1)

;; What is this, Microsoft Word?
(menu-bar-mode -1)
(tool-bar-mode -1)
(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))
(add-to-list 'default-frame-alist '(horizontal-scroll-bars . nil))

;; Thank you, but I know what program this is.
(setq inhibit-startup-screen t)

;; Don't update the X selection from the kill-ring.
;; Like Vim, Evil keeps the X selection in the `"' register.
(setq select-enable-clipboard nil)

;; Ask `y or n' rather than `yes or no'
(defalias 'yes-or-no-p 'y-or-n-p)

;; It's 2017, every display is wide, and vertically-split windows
;; are unreadable.
(setq split-height-threshold nil)
(setq split-width-threshold 144)

;; Make buffer names more unique
(setq uniquify-buffer-name-style 'forward)

;; Set color scheme
(use-package solarized-theme
  :load-path "~/.emacs.d/solarized-emacs"
  :init
  (setq custom-safe-themes t)
  (setq solarized-use-variable-pitch nil)
  (setq solarized-high-contrast-mode-line t)
  (setq solarized-height-minus-1 1.0)
  (setq solarized-height-plus-1 1.0)
  (setq solarized-height-plus-2 1.0)
  (setq solarized-height-plus-3 1.0)
  (setq solarized-height-plus-4 1.0)
  :config
  (load-theme 'solarized-light))

;; Ignore common extensions.
(add-to-list 'completion-ignored-extensions ".elc")

;;; Fonts

;; Use Source Code Pro font by default.

(add-to-list 'default-frame-alist '(font . "Hack-11"))
(set-face-attribute 'default t :font "Hack-11")

;; Don't use italics to indicate types.
(custom-theme-set-faces 'user '(font-lock-type-face ((t :slant normal))))

;;; Tabs
(setq-default tab-always-indent t)
(setq-default tab-stop-list (number-sequence 2 120 2))
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

;;; Fill column
(setq-default fill-column 80)

;;; Whitespace
(setq whitespace-style '(face trailing tabs))
(global-whitespace-mode t)
(diminish 'global-whitespace-mode)

(defun ttuegel/indent-whitespace-hygiene ()
  "Remove whitespace from the current line if it is only whitespace."
  (save-excursion
    (beginning-of-line)
    (while
        (re-search-forward "^[[:space:]]+$" (line-end-position) t)
      (replace-match ""))))

(defadvice newline
    (after indent-whitespace-hygiene-after-newline activate)
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
(use-package rainbow-delimiters
  :config
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode))

;;; Automatic indentation
(electric-indent-mode t)

;;; Window numbers
(use-package winum
  :init
  (setq winum-keymap (make-sparse-keymap)) ; Please don't mess with my maps
  :config
  (winum-mode))

;;; Window layouts
(use-package eyebrowse
  :config
  (eyebrowse-mode t))

;;; Mode line
(use-package spaceline-config
  :config
  (setq spaceline-highlight-face-func
        #'spaceline-highlight-face-evil-state) ; Color modeline by Evil state
  (spaceline-helm-mode)
  (spaceline-emacs-theme))

;;; Search
(define-prefix-command 'ttuegel/search-map)
(bind-key "C-f" ttuegel/search-map global-map)
(bind-keys
 :map ttuegel/search-map
 ("f" . (lambda () (interactive) (isearch-forward t)))
 ("F" . (lambda () (interactive) (isearch-backward t))))

;;; Buffers
(define-prefix-command 'ttuegel/buffer-map)
(bind-key* "C-b" ttuegel/buffer-map)
(bind-keys
 :map ttuegel/buffer-map
 ("C-b" . helm-buffers-list)
 ("k" . kill-buffer)
 ("C-k" . kill-this-buffer)
 ("n" . switch-to-next-buffer)
 ("p" . switch-to-prev-buffer)
 ("R" . (lambda () (interactive) (revert-buffer nil t)))
 ("r" . rename-current-buffer-file))

;;; Errors

(define-prefix-command 'ttuegel/error-map)
(bind-key* "C-e" ttuegel/error-map)
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

(unbind-key "M-z" global-map)

;;; Be Evil
(use-package evil
  :init
  (setq evil-toggle-key "C-,")

  :config
  (bind-key* "C--" 'evil-normal-state)

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

  ;; Windows
  (bind-key* "C-w" evil-window-map)
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
   ("-" . evil-window-new)
   ("|" . evil-window-vnew)
   ("k" . evil-window-delete))

  ;; Whitespace hygiene
  (defadvice evil-normal-state
      (after indent-whitespace-hygiene-after-evil-normal-state activate)
    (ttuegel/indent-whitespace-hygiene))

  ;; Evil motion keys for Dvorak
  (let ((map evil-motion-state-map))

    (unbind-key "k" map) ; evil-previous-visual-line
    (unbind-key "j" map) ; evil-next-visual-line
    (unbind-key "h" map) ; evil-backward-char
    (unbind-key "l" map) ; evil-forward-char
    (unbind-key "C-d" map) ; evil-scroll-down
    (unbind-key "$" map) ; evil-end-of-line
    (unbind-key "C-f" map) ; evil-scroll-page-down
    (unbind-key "C-b" map) ; evil-scroll-page-up

    (bind-key "t" #'evil-previous-visual-line map)
    (bind-key "h" #'evil-next-visual-line map)
    (bind-key "d" #'evil-backward-char map)
    (bind-key "n" #'evil-forward-char map)
    (bind-key "H" #'evil-scroll-down map)
    (bind-key "T" #'evil-scroll-up map)
    (bind-key "D" #'ttuegel/beginning-of-line map)
    (bind-key "N" #'end-of-visual-line map)
    (bind-key "M-h" #'evil-scroll-page-down map)
    (bind-key "M-t" #'evil-scroll-page-up map))

  (let ((map evil-normal-state-map))
    (unbind-key "d" map) ; evil-delete
    (unbind-key "D" map) ; evil-delete-line

    (bind-keys :map map
               ("k" . evil-delete)
               ("K" . evil-delete-line)))

  (setq-default evil-shift-width tab-width)

  (evil-set-initial-state 'comint-mode 'emacs)

  (evil-mode t))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package evil-indent-textobject)

;;; Undo Tree
(use-package undo-tree
  :bind (:map evil-normal-state-map
              ("u" . undo-tree-undo)
              ("U" . undo-tree-redo))
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode 1))

;;; Helm
(use-package helm-config
  :diminish helm-mode
  :config
  (require 'helm-files)

  ;; Skip boring files in `helm-find-files'
  (setq helm-ff-skip-boring-files t)
  ;; Open Helm in the current window
  (setq helm-split-window-default-side 'same)

  (bind-key "M-h" helm-command-map)
  (bind-key "C-f" 'helm-find-files helm-command-map)

  (bind-key "M-s" 'helm-M-x)
  (bind-key "C-f" 'helm-find-files ctl-x-map)

  (bind-key "C-h" 'helm-next-line helm-map)
  (bind-key "C-t" 'helm-previous-line helm-map)
  (bind-key "C-n" 'helm-execute-persistent-action helm-map)

  ;; `C-d' goes up one level in `helm-find-files' and friends
  (bind-key "C-d" 'helm-find-files-up-one-level helm-read-file-map)
  (bind-key "C-d" 'helm-find-files-up-one-level helm-find-files-map)

  (helm-mode 1))

;;; Avy
(use-package avy
  :bind (:map evil-motion-state-map
              ("f" . avy-goto-char)
              ("F" . avy-goto-line))
  :config
  (setq avy-keys '(?a ?o ?e ?u ?h ?t ?n ?s)))

;;; Git
(define-prefix-command 'ttuegel/vc-map)
(bind-key "C-z" ttuegel/vc-map global-map)

(use-package magit
  :bind (:map ttuegel/vc-map
              ("C-z" . magit-status)))

(use-package autorevert
  :demand nil
  :config
  (diminish 'auto-revert-mode))

(use-package git-timemachine
  :bind (:map ttuegel/vc-map
              ("t" . git-timemachine-toggle)))

;;; TeX
(use-package tex
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq TeX-save-query nil)
  (setq TeX-source-correlate-mode t)

  (add-hook 'LaTeX-mode-hook (lambda () (TeX-PDF-mode t)))
  (add-hook 'LaTeX-mode-hook (lambda () (flyspell-mode 1)))

  ;; Use a proper URL with Okular
  (add-to-list
   'TeX-view-program-list
   '("Okular" ("okular --unique file:%o"
               (mode-io-correlate "#src:%n%a")) "okular"))

  ;; Use absolute filename for "%o" expansion
  (add-to-list
   'TeX-expand-list
   '("%o" (lambda nil
            (expand-file-name
             (funcall file (TeX-output-extension) t)))))

  (setq
   TeX-view-program-selection
   '(((output-dvi has-no-display-manager) "dvi2tty")
     (output-dvi "xdvi")
     (output-pdf "Okular")
     (output-html "xdg-open"))))

(use-package font-latex
  :config
  ;; Disable Unicode fontification
  (setq font-latex-fontify-script nil)
  (setq font-latex-fontify-sectioning 'color)
  (add-to-list 'font-latex-math-environments "dmath"))

(use-package reftex
  :after tex
  :config
  (setq reftex-plug-into-AUCTeX t)
  (setq reftex-default-bibliography "~/bib/default.bib")
  (add-hook 'TeX-mode-hook #'reftex-mode))

(use-package helm-bibtex
  :config
  (setq bibtex-completion-bibliography "~/bib/default.bib")
  (setq bibtex-completion-library-path "~/bib/files/")
  (setq bibtex-completion-pdf-open-function
        #'helm-open-file-with-default-tool))

(use-package bibtex-fetch
  :load-path "~/.emacs.d/bibtex-fetch")

;;; Markdown
(use-package markdown-mode)

;;; Flycheck
(use-package flycheck
  :config
  (add-to-list 'flycheck-disabled-checkers 'haskell-hlint))

;;; Nix
(use-package nix-mode
  :config
  (add-hook 'nix-mode-hook #'rainbow-delimiters-mode))

;;; Ledger
(use-package ledger-mode)

;;; YAML
(use-package yaml-mode)

;;; Company
(use-package company
  :diminish company-mode
  :bind (:map company-active-map
              ("C-h" . company-select-next)
              ("C-t" . company-select-previous)
              ("C-n" . company-complete-common)
              ("C-j" . company-complete-selection)
              ("C-g" . company-abort))
  :config
  (global-company-mode))

;;; Emacs Lisp

;;; Haskell
(use-package haskell-mode
  :config
  (setq haskell-literate-default 'tex)
  (setq haskell-auto-import-loaded-modules nil)
  (setq haskell-process-log t)
  (setq haskell-process-suggest-remove-import-lines nil)
  (eval-after-load 'rainbow-delimiters
    '(add-hook 'haskell-mode-hook #'rainbow-delimiters-mode)))

(use-package dante
  :after haskell-mode
  :config

  (let ((methods `((bare  . ,(lambda (_) '("cabal" "repl"))))))
    (setq dante-repl-command-line-methods-alist methods))

  (add-hook 'haskell-mode-hook #'dante-mode)
  (eval-after-load 'flycheck
    '(add-hook 'haskell-mode-hook #'flycheck-mode)))

;;; XML
(add-to-list 'auto-mode-alist '("\\.rng\\'" . xml-mode))

;;; Org
(use-package org
  :config
  (setq org-catch-invisible-edits 'show))

;;; rust
(use-package rust-mode)

(use-package cargo
  :after rust-mode)

(use-package flycheck-rust
  :after rust-mode flycheck
  :config
  (add-hook 'rust-mode-hook #'flycheck-mode))

;;; Mail

(defun ttuegel/set-mail-host ()
  (interactive)
  (save-excursion
    (save-restriction
      (message-narrow-to-headers-or-head)
      (let ((from (mail-extract-address-components
                   (message-fetch-field "From"))))
        (if from
            (let* ((addr (split-string (cadr from) "@"))
                   (host (cadr addr)))
              (if host
                  (setq-local mail-host-address host)
                (error "Could not get mail host from address %s" addr)))
          (error "Could not get sender address"))))))

(defadvice message-send (before ttuegel/set-mail-host-advice activate)
  (interactive)
  (ttuegel/set-mail-host))

(use-package message
  :config
  (setq mm-text-html-renderer 'shr)
  (setq message-sendmail-envelope-from 'header)
  (setq message-kill-buffer-on-exit t))

(use-package sendmail
  :config
  (setq send-mail-function #'sendmail-send-it)
  (setq sendmail-program "msmtp"))

;;; Notmuch
(use-package notmuch
  :config
  (setq notmuch-command "~/.local/bin/notmuch")

  (setq notmuch-search-oldest-first nil)
  (setq notmuch-fcc-dirs
        '(("ttuegel@mailbox.org" . "mailbox/INBOX +sent +mailbox")
          ("ttuegel@gmail.com" . "\"gmail/[Gmail].All Mail\" +sent +gmail")
          ("tuegel2@illinois.edu" . "illinois/INBOX +sent +illinois")))

  (setq notmuch-saved-searches
        '((:name "inbox" :query "tag:inbox and not tag:foss")
          (:name "todo" :query "tag:todo and not tag:foss")
          (:name "foss" :query "tag:inbox and tag:foss")
          (:name "foss todo" :query "tag:todo and tag:foss")))

  (let ((map notmuch-search-mode-map))
    (unbind-key "n" map) ; notmuch-search-next-thread
    (unbind-key "p" map) ; notmuch-search-previous-thread
    (unbind-key "l" map) ; notmuch-search-filter
    (bind-keys :map map
               ("h" . notmuch-search-next-thread)
               ("t" . notmuch-search-previous-thread)
               ("f" . notmuch-search-filter)))

  (defun ttuegel/notmuch-search-delete (&optional beg end)
    "Delete the selected thread or region.

This function advances to the next thread when finished."
    (interactive (notmuch-search-interactive-region))
    (notmuch-search-tag '("+deleted" "-inbox") beg end)
    (when (eq beg end)
      (notmuch-search-next-thread)))
  (bind-key "k" #'ttuegel/notmuch-search-delete notmuch-search-mode-map)

  (defun ttuegel/notmuch-show-delete ()
    "Delete the thread in the current buffer, then show the next thread from search."
    (interactive)
    (notmuch-show-tag-all '("+deleted" "-inbox"))
    (notmuch-show-next-thread t))
  (bind-key "k" #'ttuegel/notmuch-show-delete notmuch-show-mode-map)

  (defun ttuegel/notmuch-search-mute (&optional beg end)
    "Mute the selected thread or region.

This function advances to the next thread when finished."
    (interactive (notmuch-search-interactive-region))
    (notmuch-search-tag '("+muted" "-inbox") beg end)
    (when (eq beg end)
      (notmuch-search-next-thread)))
  (bind-key "u" #'ttuegel/notmuch-search-mute notmuch-search-mode-map)

  (defun ttuegel/notmuch-show-mute ()
    "Mute the thread in the current buffer, then show the next thread from search."
    (interactive)
    (notmuch-show-tag-all '("+muted" "-inbox"))
    (notmuch-show-next-thread t))
  (bind-key "u" #'ttuegel/notmuch-show-mute notmuch-show-mode-map))

;;; C
(add-hook 'c-mode-common-hook #'c-guess)

;;; Es
(use-package sh-script
  :config
  (let ((es-syntax-table
         (sh-mode-syntax-table sh-mode-syntax-table ?\` "'")))
    (add-to-list 'sh-mode-syntax-table-input `(es ,es-syntax-table))))

;;; Maxima
(use-package maxima
  :mode ("\\.mac\\'" . maxima-mode)
  :config
  (evil-set-initial-state 'inferior-maxima-mode 'emacs))

(use-package imaxima
  :config
  (setq imaxima-equation-color "#657b83"))

(provide 'init)
;;; init.el ends here
