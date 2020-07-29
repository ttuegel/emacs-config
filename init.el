;;; init.el -- ttuegel's configuration file
;;; Commentary:
;;; Code:

;;; use-package

(eval-when-compile (require 'use-package))
(setq use-package-always-defer t)

(require 'bind-key)
(require 'diminish)

(diminish 'eldoc-mode)

;;; auto-compile -- automatically compile .el files

(require 'auto-compile)
(auto-compile-on-load-mode)
(auto-compile-on-save-mode)

;; Load the .el if it's newer than the .elc
(setq load-prefer-newer t)

;;; which-key -- show available keys after incomplete commands

(require 'which-key)
(which-key-mode)
(diminish 'which-key-mode)


;;; Definitions

(defun ttuegel/call-process (outfile program &rest args)
  "Run PROGRAM with ARGS and save output in OUTFILE."
  (let ((out (if outfile `((:file ,outfile) nil) 0)))
    (apply #'call-process program nil out nil args)))

(defun ttuegel/cabal2nix ()
  "Regenerate Nix expression from a Cabal package in the current directory."
  (let ((cabal2nix (executable-find "cabal2nix"))
        (cabal-file-names (file-expand-wildcards "*.cabal")))
    (when (and cabal2nix cabal-file-names)
      (let* ((project-name (file-name-base (car cabal-file-names)))
             (nix-file-name (format "%s.nix" project-name)))
        (when (file-exists-p nix-file-name)
          (ttuegel/call-process nix-file-name cabal2nix "./."))))))

(defun ttuegel/hpack ()
  "Run hpack in the current directory."
  (let ((hpack (executable-find "hpack")))
    (when hpack (ttuegel/call-process nil hpack "--silent"))))

(defun ttuegel/beginning-of-line ()
  "`beginning-of-line' if `back-to-indentation' does not move the cursor."
  (interactive)
  (let ((before (point)))
    (unless (eq before (line-beginning-position))
      (back-to-indentation)
      (let ((after (point)))
        (when (eq before after) (beginning-of-line))))))

;;; Emacs

;; Don't EVER touch my init.el!
(eval-after-load "cus-edit"
  '(defun customize-save-variable
       (variable value &optional comment) value))

;; Use UTF-8 everywhere. It's 2018, how is this not default?
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

;; It's 2020, every display is wide, and vertically-split windows
;; are unreadable.
(setq split-height-threshold nil)
(setq split-width-threshold 144)

;; Make buffer names more unique
(setq uniquify-buffer-name-style 'forward)

;; Fill column
(setq-default fill-column 80)

;; Ignore common extensions.
(add-to-list 'completion-ignored-extensions ".elc")
(add-to-list 'completion-ignored-extensions ".hi")
(add-to-list 'completion-ignored-extensions ".o")
(add-to-list 'completion-ignored-extensions ".dyn_hi")
(add-to-list 'completion-ignored-extensions ".dyn_o")

;; Tab stops
(setq-default tab-always-indent t)
(setq-default tab-stop-list (number-sequence 2 120 2))
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

;; Hygiene

(defun ttuegel/indent-whitespace-hygiene ()
  "Remove whitespace from the current line if it is only whitespace."
  (save-excursion
    (beginning-of-line)
    (while
        (re-search-forward "^[[:space:]]+$" (line-end-position) t)
      (replace-match "\n"))))

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

;; Performance

(setq gc-cons-threshold (* 100 1024 1024))

;; Emacs 27:
(setq read-process-output-max (* 1024 1024))

;; Local variables

(put 'dante-repl-command-line 'safe-local-variable #'ttuegel/string-listp)
(put 'haskell-indentation-where-post-offset 'safe-local-variable #'numberp)
(put 'haskell-indentation-where-pre-offset 'safe-local-variable #'numberp)
(put 'haskell-stylish-on-save 'safe-local-variable #'booleanp)

;;; whitespace

(require 'whitespace)
(setq-default whitespace-style '(face trailing tabs))
(global-whitespace-mode t)
(diminish 'global-whitespace-mode)

;;; electric-indent

(electric-indent-mode -1)
(add-hook 'emacs-lisp-mode-hook #'electric-indent-local-mode)

;;; show-paren

(show-paren-mode t)

;;; browse-url

(use-package browse-url
  :config
  (setq browse-url-browser-function #'browse-url-firefox)
  (setq browse-url-new-window-flag t)
  )

;;; spaceline

(require 'spaceline-config)

;; Evil state colors
(setq spaceline-highlight-face-func #'spaceline-highlight-face-evil-state)

(spaceline-emacs-theme)
(spaceline-toggle-buffer-size-off)
(spaceline-toggle-buffer-encoding-abbrev-off)
(spaceline-toggle-version-control-off)

;;; evil

(defvar evil-toggle-key "C-,")
(use-package evil :demand)

;; Allow the cursor to move one character beyond the end of the line,
;; unlike Vim but as in Emacs. Prevents the cursor from creeping backwards
;; when pasting under evil-execute-in-normal-state.
(setq evil-move-beyond-eol t)

(setq-default evil-shift-width tab-width)

(evil-set-initial-state 'comint-mode 'emacs)
(evil-set-initial-state 'text-mode 'normal)
(evil-set-initial-state 'git-commit-mode 'normal)

(evil-mode t)

;; C-x

(bind-key "C-r" ctl-x-map)
(unbind-key "C-r" evil-normal-state-map)
(unbind-key "C-x")

(bind-key "M-r" #'execute-extended-command)

(bind-key "C-v" #'quoted-insert)

;; Evil normal state

(bind-key "C-b" #'evil-normal-state evil-insert-state-map)
(bind-key "C-b" #'evil-normal-state evil-replace-state-map)
(bind-key "C-b" #'evil-normal-state evil-visual-state-map)

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
 ("-" . evil-window-new)
 ("|" . evil-window-vnew)
 ("k" . evil-window-delete))

;; Motion

(let ((map evil-motion-state-map))
  (unbind-key "k" map) ; evil-previous-visual-line
  (unbind-key "j" map) ; evil-next-visual-line
  (unbind-key "h" map) ; evil-backward-char
  (unbind-key "l" map) ; evil-forward-char
  (unbind-key "C-d" map) ; evil-scroll-down
  (unbind-key "$" map) ; evil-end-of-line
  (unbind-key "C-f" map) ; evil-scroll-page-down
  (unbind-key "C-b" map) ; evil-scroll-page-up

  (bind-keys
   :map map
   ("t" . evil-previous-visual-line)
   ("h" . evil-next-visual-line)
   ("d" . evil-backward-char)
   ("n" . evil-forward-char)
   ("H" . evil-scroll-down)
   ("T" . evil-scroll-up)
   ("D" . beginning-of-visual-line)
   ("N" . evil-end-of-line)
   ("M-h" . evil-scroll-page-down)
   ("M-t" . evil-scroll-page-up)))

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


(let ((map evil-normal-state-map))
  (unbind-key "d" map) ; evil-delete
  (unbind-key "D" map) ; evil-delete-line

  (bind-keys
   :map map
   ("k" . evil-delete)
   ("K" . evil-delete-line)))

;; Buffers

(defvar buffer-map)
(define-prefix-command 'buffer-map)
(bind-key "b" #'buffer-map ctl-x-map)
(bind-keys
 :map buffer-map
 ("b" . switch-to-buffer)
 ("C-b" . buffer-menu)
 ("k" . kill-buffer)
 ("C-k" . kill-this-buffer)
 ("n" . switch-to-next-buffer)
 ("p" . switch-to-prev-buffer)
 ("R" . (lambda () (interactive) (revert-buffer nil t)))
 ("r" . rename-current-buffer-file))

;;; xref

;; evil-mode clobbers the default xref-find-apropos binding
(unbind-key "M-." evil-normal-state-map)

;;; undo-tree

(require 'undo-tree)

(let ((map undo-tree-map))
  (unbind-key "C-_" map)
  (unbind-key "M-_" map))

(bind-keys
    :map evil-normal-state-map
    ("u" . undo-tree-undo)
    ("U" . undo-tree-redo))

(global-undo-tree-mode 1)
(diminish 'undo-tree-mode)

;;; evil-surround

(require 'evil-surround)
(global-evil-surround-mode 1)

;;; evil-indent-textobject

(require 'evil-indent-textobject)

;;; faces

;; Use system default monospace font in 12 pt height.

(defun ttuegel/set-font (frame)
  "Configure font when FRAME is created."
  (select-frame frame)
  (when (display-graphic-p)
    (set-frame-font "Monospace-12")))

;; Set font in all extant frames.
(mapc #'ttuegel/set-font (frame-list))

;; Set font in all future frames.
(add-hook 'after-make-frame-functions #'ttuegel/set-font)

;; Don't use italics to indicate types.

(custom-theme-set-faces 'user '(font-lock-type-face ((t :slant normal))))

;; Colors

(setq custom-safe-themes t)

(use-package solarized-theme
  :demand
  :config
  (setq solarized-use-variable-pitch nil)
  (setq solarized-high-contrast-mode-line t)
  (setq solarized-height-minus-1 1.0)
  (setq solarized-height-plus-1 1.0)
  (setq solarized-height-plus-2 1.0)
  (setq solarized-height-plus-3 1.0)
  (setq solarized-height-plus-4 1.0)
  (load-theme 'solarized-light)
  )

;; Disable overline: it changes the mode-line's height
;; Draw a dark box, even on inactive windows.
(let
    ((spec (list :box '(:line-width 1 :color "#687b83" :style unspecified)
                 :overline nil)))
  (custom-theme-set-faces 'user
    `(mode-line          ((t . ,spec)) t)
    `(mode-line-inactive ((t . ,spec)) t)
    )
  )

;;; rainbow-delimiters

(require 'rainbow-delimiters)
(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook #'display-line-numbers-mode)

;;; Ivy

(use-package ivy
  :commands ivy-mode
  :init
  (ivy-mode)
  :config
  (diminish 'ivy-mode)
  (bind-keys
      :map ivy-minibuffer-map
      ("C-h" . next-line)
      ("C-t" . previous-line)
      ("C-d" . backward-char)
      ("C-n" . forward-char)
      ("C-f" . ivy-partial-or-done)
      )
  )

(use-package counsel
  :commands counsel-mode
  :init
  (counsel-mode)
  :config
  (diminish 'counsel-mode)
  )

(use-package swiper
  :commands swiper
  :init
  (bind-key "C-s" #'swiper)
  )


;;; Projectile

(use-package projectile
  :diminish projectile-mode
  :commands projectile-mode
  :init
  (run-with-idle-timer 0.5 nil (lambda () (projectile-mode)))
  :config
  (setq projectile-completion-system 'ivy)
  (bind-key "C-c p" #'projectile-command-map projectile-mode-map)
  )

(use-package counsel-projectile
  :init
  (add-hook 'projectile-mode #'counsel-projectile-mode)
  )

;;; Visual-Fill-Column

(use-package visual-fill-column
  :commands (turn-on-visual-fill-column-mode)
  )


;;; Avy

(use-package avy
  :requires (evil)
  :init
  (bind-keys
   :map evil-motion-state-map
   ("f" . avy-goto-char)
   ("F" . avy-goto-line)
   :map search-map
   ("f" . avy-goto-char)
   ("F" . avy-goto-line))
  :config
  (setq avy-keys '(?a ?o ?e ?u ?h ?t ?n ?s)))


;;; Company

(use-package company
  :diminish company-mode
  :commands company-mode
  :init
  (add-hook 'emacs-lisp-mode-hook #'company-mode)
  :config
  (bind-keys :map company-mode-map ("C-f" . company-complete))
  (setq company-minimum-prefix-length 1)
  ;; Disable idle completion. The idle timer is rather slow.
  (setq company-idle-delay nil)
  (setf company-active-map (make-sparse-keymap))
  (bind-keys
   :map company-active-map
   ("C-h" . company-select-next)
   ("C-t" . company-select-previous)
   ("C-f" . company-complete-selection)
   ("M-f" . company-complete-common)
   ("C-g" . company-abort))
  )


;;; flycheck

(use-package flycheck
  :commands flycheck-mode global-flycheck-mode
  :config
  (setq flycheck-clang-language-standard "c++17"))

(add-hook 'emacs-lisp-mode-hook #'flycheck-mode)


;;; flymake

(use-package flymake
  :defines flymake-run-in-place
  :config
  (setq flymake-run-in-place nil)
  )


;;; yasnippet

(use-package yasnippet
  :config
  (diminish 'yas-minor-mode))


;;; Git

(use-package magit
  :init
  (bind-key "g" #'magit-status vc-prefix-map))

(use-package autorevert
  :diminish auto-revert-mode)

(use-package git-timemachine
  :commands git-timemachine-toggle
  :init
  (bind-key "t" #'git-timemachine-toggle vc-prefix-map))

(use-package git-auto-commit-mode
  ;; Use personal fork for `call-process' instead of `shell-command'.
  :load-path "./git-auto-commit-mode")


;;; TeX

(use-package tex
  :commands TeX-PDF-mode
  :defines
  TeX-save-query
  LaTeX-item-indent
  LaTeX-indent-level
  LaTeX-label-alist
  :config
  ;; Recognize style files multi-file documents
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)

  ;; Save without asking
  (setq TeX-save-query nil)

  ;; Allow viewer to move cursor
  (setq TeX-source-correlate-mode t)

  ;; Disable automatic indentation
  (setq LaTeX-item-indent 0)
  (setq LaTeX-indent-level 0)

  ;; No automatic equation labels
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (add-to-list 'LaTeX-label-alist '("equation" . nil))
              (add-to-list 'LaTeX-label-alist '("eqnarray" . nil))
              (add-to-list 'LaTeX-label-alist '("dmath" . nil))
              (add-to-list 'LaTeX-label-alist '("dgroup" . nil))
              (add-to-list 'LaTeX-label-alist '("dseries" . nil))))

  ;; Build PDFs by default
  (add-hook 'LaTeX-mode-hook (lambda () (TeX-PDF-mode t)))

  ;; Automatic spell checking
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
     (output-html "xdg-open")))
  )

(use-package font-latex
  :commands font-latex-setup
  :init
  (add-hook 'TeX-mode-hook #'font-latex-setup)
  :config
  ;; Disable Unicode fontification
  (setq font-latex-fontify-script nil)
  (setq font-latex-fontify-sectioning 'color)
  (add-to-list 'font-latex-math-environments "dmath"))

(use-package reftex
  :commands reftex-mode
  :init
  (add-hook 'TeX-mode-hook #'reftex-mode)
  :config
  (setq reftex-plug-into-AUCTeX t)
  (setq reftex-default-bibliography "~/bib/default.bib"))

;; Completion

(use-package company-math
  :init
  (with-eval-after-load "company"
    (push 'company-math-symbols-latex company-backends)
    (push 'company-latex-commands company-backends)))


;;; Markdown

(use-package markdown-mode)


;;; Nix

(use-package nix-mode
  :commands nix-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode))
  (add-to-list 'auto-mode-alist '("\\.nix.in\\'" . nix-mode))
  :config
  (add-hook 'nix-mode-hook #'rainbow-delimiters-mode))


;;; Ledger

(use-package ledger-mode)


;;; YAML

(defun ttuegel/after-save-hpack ()
  "Run `hpack' after saving `package.yaml'."
  (when (equal (file-name-nondirectory buffer-file-name) "package.yaml")
    (ttuegel/hpack)
    (ttuegel/cabal2nix)))

(use-package yaml-mode
  :config
  (add-hook
   'yaml-mode-hook
   (lambda ()
     (add-hook 'after-save-hook #'ttuegel/after-save-hpack))
   )
  )


;;; Language Server

(use-package lsp-mode
  :commands lsp
  :hook
  ((lsp-mode . lsp-enable-which-key-integration)
   (haskell-mode . lsp)
   )
  :config
  (diminish 'lsp-mode)
  (setq lsp-auto-configure t)
  (setq lsp-diagnostic-package :flycheck)
  (setq lsp-prefer-capf t)

  ;; Disable file watchers
  (setq lsp-enable-file-watchers nil)

  ;; Disable hover info in eldoc because it is slow.
  ;; TODO: Get hover info asynchronously?
  (setq lsp-eldoc-enable-hover nil)
  )

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-peek-enable nil)
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-ui-imenu-enable t)
  )

(use-package lsp-java :hook (java-mode . lsp))

(use-package lsp-haskell
  :config
  (setq lsp-haskell-process-path-hie "ghcide")
  (setq lsp-haskell-process-args-hie '())
  )

(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list
  )

(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol
  )

;;; eglot

(use-package eglot
  :defines eglot-server-programs
  :config
  (add-to-list 'eglot-server-programs '(haskell-mode . ("ghcide" "--lsp")))
  ;; Disable eldoc-mode in eglot.
  ;; eldoc-mode generates hover events after every cursor movement, which is
  ;; quite slow.
  (add-hook 'eglot-managed-mode-hook #'turn-off-eldoc-mode)
  )

;;; Haskell

;; (use-package intero
;;   :commands intero-global-mode
;;   :config
;;   (setq intero-extra-ghc-options '("-Wall"))
;;   (setq intero-blacklist '("~/ttuegel.github.io")))

;; (intero-global-mode 1)

(defun turn-off-eldoc-mode ()
  "Disable function `eldoc-mode' in the current buffer."
  (eldoc-mode -1))

;; (use-package hhp
;;   :load-path "~/hhp/elisp"
;;   :commands hhp-init hhp-debug)

(defun ttuegel/string-listp (object)
  "Return t if OBJECT is a list of strings.
Otherwise return nil."
  (and (listp object)
       (seq-reduce (lambda (accum item) (and accum (stringp item))) object t)
       )
  )

;; (use-package dante
;;   :after haskell-mode
;;   :commands 'dante-mode
;;   :init
;;   (add-hook 'haskell-mode-hook #'dante-mode)
;;   (setq flycheck-check-syntax-automatically '(save mode-enabled))
;;   )

(use-package haskell-mode
  :config
  (require 'lsp-haskell)

  (setq haskell-literate-default 'tex)
  (setq haskell-process-log t)

  (add-hook 'haskell-mode-hook #'company-mode)
  (add-hook 'haskell-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'haskell-mode-hook #'display-line-numbers-mode)
  (add-hook 'haskell-mode-hook
            (lambda ()
              (setq-local whitespace-style '(face lines trailing tabs))))
  )


;;; Dhall

(use-package dhall-mode
  :mode ("\\.dhall\\'" . dhall-mode)
  :config
  (setq dhall-format-at-save nil))


;;; XML

(use-package nxml-mode
  :mode ("\\.rng\\'" . xml-mode))


;;; Org

(defvar org-prefix-map)
(define-prefix-command 'org-prefix-map)
(bind-key "o" #'org-prefix-map ctl-x-map)

(use-package org
  :defines
  org-clock-clocktable-default-properties
  org-clock-persist
  :config
  (require 'org-agenda)
  (require 'org-capture)

  (setq org-directory "~/org")

  (setq org-todo-keywords
        (quote ((type "TODO(t)" "STARTED(s@)" "WAITING(w@/!)" "MAYBE(m)"
                      "|" "DONE(d!)" "CANCELLED(c@)"))))

  ;; Children block parent TODO items
  (setq org-enforce-todo-dependencies t)

  ;; Log entries into LOGBOOK drawer
  (setq org-log-into-drawer t)

  (setq org-default-notes-file "~/org/notes.org"
        org-reverse-note-order t)

  (setq org-catch-invisible-edits 'show)
  (setq org-blank-before-new-entry '((heading . t) (plain-list-item t)))
  (setq org-file-apps
        '((auto-mode . emacs)
          (system . "xdg-open %s")
          ("\\.x?html?\\'" . system)
          ("\\.pdf\\'" . system)))

  (setq org-clock-clocktable-default-properties
        '(:maxlevel 2 :scope file :block today))

  (add-hook 'org-mode-hook #'turn-on-visual-line-mode)
  (add-hook 'org-mode-hook #'turn-on-visual-fill-column-mode)

  ;; Save clock history across Emacs sessions
  (setq org-clock-persist 'history)
  (org-clock-persistence-insinuate)
  )


(use-package org-agenda
  :defines
  org-agenda-ndays
  :init
  (bind-key "a" #'org-agenda org-prefix-map)
  :config
  (setq org-agenda-window-setup 'current-window)

  (setq org-agenda-files (list org-directory)
        org-agenda-ndays 7
        org-deadline-warning-days 0
        org-agenda-show-all-dates t
        org-agenda-skip-deadline-if-done t
        org-agenda-skip-scheduled-if-done t
        org-agenda-start-on-weekday nil)

  (setq org-agenda-custom-commands
        (quote
         (("u" "Unscheduled tasks" tags "-SCHEDULED={.+}/!+TODO"))))

  (unbind-key "H" org-agenda-mode-map)
  (unbind-key "h" org-agenda-mode-map)

  (bind-key "C-d" #'org-agenda-backward-block org-agenda-mode-map)
  (bind-key "C-n" #'org-agenda-forward-block org-agenda-mode-map)
  (bind-key "C-h" #'org-agenda-next-line org-agenda-mode-map)
  (bind-key "C-t" #'org-agenda-previous-line org-agenda-mode-map)

  (bind-key "M-h" #'org-agenda-drag-line-forward org-agenda-mode-map)
  (bind-key "M-t" #'org-agenda-drag-line-backward org-agenda-mode-map)
  )


(use-package org-capture
  :commands org-capture
  :init
  (bind-key "c" #'org-capture org-prefix-map)
  :config
  (setq org-capture-templates
        (quote (("t" "todo" entry (file+headline "~/org/todo.org" "Tasks")
                 "* TODO %?")
                ("j" "journal" entry (file+olp+datetree "~/org/journal.org")
                 "* %?\n\n%U")
                ("n" "note" entry (file "~/org/notes.org")
                 "* %?\n\n%U")
                ("b" "bibliography" entry (file "~/org/bib.org")
                 "* UNREAD %:title%?")))))


;;; rust

(use-package rust-mode
  :config
  (add-to-list 'rust-mode-hook #'flycheck-mode))

(use-package cargo)

(use-package flycheck-rust
  :commands flycheck-rust-setup
  :init
  (with-eval-after-load "flycheck"
    (add-to-list 'flycheck-mode-hook #'flycheck-rust-setup)))


;;; C

(add-hook 'c-mode-common-hook #'flycheck-mode)
(add-hook
 'c-mode-common-hook
 (lambda ()
   (setq-local whitespace-style '(lines face trailing))))


;; Makefile

(add-hook
 'makefile-mode-hook
 (lambda ()
   (setq-local whitespace-style '(lines face trailing))))


;;; unfill-region

(defun unfill-region (beg end)
  "Join text paragraphs between BEG and END into a single logical line.
This is useful, e.g., for use with function `visual-line-mode'."
  (interactive "*r")
  (let ((fill-column (point-max)))
    (fill-region beg end)))

;;; pdftotext

(use-package pdftotext
  :load-path "./lisp"
  :demand)


;;; secret

(use-package secret
  :load-path "~/el/secret-el"
  :demand)


;;; Scheme

(use-package scheme
  :config
  (add-hook 'scheme-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'scheme-mode-hook #'company-mode)
  (put 'unless 'scheme-indent-function 1)
  (put 'match 'scheme-indent-function 1)
  (put 'with-directory 'scheme-indent-function 1)
  (put 'when 'scheme-indent-function 1))


;;; Idris

(use-package idris-mode)


;;; Fish

(use-package fish-mode)


;;; EditorConfig

(use-package editorconfig
  :demand
  :diminish
  :config
  (editorconfig-mode 1))


;;; Ledger

(use-package ledger-mode
  :mode ("\\.journal\\'" . ledger-mode))

(use-package evil-ledger
  :after ledger-mode
  :diminish
  :init
  (add-hook 'ledger-mode-hook #'evil-ledger-mode)
  :config
  (dolist (state '(normal motion visual))
    (evil-define-key* state evil-ledger-mode-map
      (kbd "gh") 'evil-ledger-forward-xact
      (kbd "gt") 'evil-ledger-backward-xact)))


;;; Groovy (Jenkinsfile)

(use-package groovy-mode
  :mode ("Jenkinsfile" . groovy-mode))


;;; Scala

(use-package scala-mode)


;;; Direnv

(use-package direnv)


;;; imenu

(bind-key "C-c i" #'imenu)


;;; spinner

;; Disable the spinner.
(with-eval-after-load "spinner"
  (defun spinner-start (&optional type-or-object fps delay) nil)
  (defun spinner-stop (&optional spinner) nil)
  )


(provide 'init)
;;; init.el ends here
