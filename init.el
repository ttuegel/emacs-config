;;; init.el -- ttuegel's configuration file -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile (require 'use-package))

(defun ttuegel/emacs-init-time-message ()
  "Display the Emacs startup time in *Messages*."
  (message "Emacs started in %s" (emacs-init-time)))

(add-hook 'emacs-startup-hook #'ttuegel/emacs-init-time-message)

;;; diminish -- hide select minor modes on the modeline
(use-package diminish)

(diminish 'eldoc-mode)


;;; auto-compile -- automatically compile .el files
(use-package auto-compile
  :init
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode)
  )


;;; which-key -- show available keys after incomplete commands
(use-package which-key
  :diminish which-key-mode
  :custom (which-key-idle-delay 0.2)
  :custom (which-key-popup-type 'minibuffer)
  :init
  (which-key-mode)
  )

;;; Definitions
(defun pulse-line (&rest _)
  "Pulse the current line."
  (pulse-momentary-highlight-one-line (point)))

;;; Emacs
(use-package emacs
  :custom (kill-do-not-save-duplicates t)
  ;; Customize `custom.el' instead of `init.el'
  :custom (custom-file (expand-file-name "custom.el" user-emacs-directory))
  ;; Turn off the damn bell! The screen will flash instead.
  :custom (visible-bell t)
  ;; Twenty-seven eight-by-ten color glossy photographs, with circles and arrows
  ;; on the back of each one explaining what each one was, to be used as
  ;; evidence against us.
  :custom ((auto-save-default nil)
           (make-backup-files nil))
  ;; Don't update the X selection from the kill-ring.
  ;; Like Vim, Evil keeps the X selection in the `=' register.
  :custom (select-enable-clipboard nil)
  ;; Make buffer names more unique
  :custom (uniquify-buffer-name-style 'forward)
  ;; Do not save duplicates in the kill ring
  :custom (kill-do-not-save-duplicates t)
  ;; Fill column
  :custom (fill-column 80)
  ;; Tab stops
  :custom ((tab-always-indent t)
           (tab-stop-list nil)
           (tab-width 2)
           (indent-tabs-mode nil))
  :custom (max-mini-window-height 0.125)
  ;; Make scripts executable on save
  :hook (after-save . executable-make-buffer-file-executable-if-script-p)
  :hook (emacs-lisp-mode . display-line-numbers-mode)
  :config
  ;; Blinking should be reserved for eyelids and indicators that require immediate attention.
  (blink-cursor-mode -1)

  ;; Ignore common extensions.
  (add-to-list 'completion-ignored-extensions ".elc")
  (add-to-list 'completion-ignored-extensions ".hi")
  (add-to-list 'completion-ignored-extensions ".o")
  (add-to-list 'completion-ignored-extensions ".dyn_hi")
  (add-to-list 'completion-ignored-extensions ".dyn_o")

  ;; Emacs 27:
  (setq read-process-output-max (* 1024 1024))

  ;; Pulse the current line after scrolling.
  (dolist (command '(scroll-up-command scroll-down-command recenter-top-bottom other-window))
    (advice-add command :after #'pulse-line))

  (column-number-mode t)
  )

(load-theme 'modus-operandi t)

;;; electric-indent
;; Enable `electric-indent-mode' only in select major modes.
(electric-indent-mode -1)
(add-hook 'emacs-lisp-mode-hook #'electric-indent-local-mode)


;;; smartparens
(use-package smartparens
  :diminish
  ;; :hook (prog-mode . smartparens-mode)
  )

;;; puni

(use-package puni)


;;; paren
;; Highlight matching parentheses.
(show-paren-mode t)


;;; doom-modeline
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :custom ((doom-modeline-height (max (ceiling (* (frame-char-height) all-the-icons-scale-factor))
                                      30
                                      )
                                 )
           (doom-modeline-bar-width 8)
           )
  :custom (doom-modeline-minor-modes t)
  :custom (doom-modeline-buffer-file-name-style 'truncate-except-project)
  :config
  (doom-modeline-def-modeline 'ttuegel/main
    '(bar workspace-name window-number modals matches follow buffer-info remote-host buffer-position word-count parrot selection-info)
    '(objed-state misc-info persp-name debug repl lsp minor-modes input-method indent-info major-mode process checker)
    )
  (defun ttuegel/set-default-modeline ()
    (doom-modeline-set-modeline 'ttuegel/main 'default)
    )
  (add-hook 'doom-modeline-mode-hook #'ttuegel/set-default-modeline)

  )


;;; helpful
(use-package helpful
  :bind (:map helpful-mode-map ([remap revert-buffer] . helpful-update))
  :bind (([remap describe-command] . helpful-command)
         ([remap describe-function] . helpful-callable)
         ([remap describe-key] . helpful-key)
         ([remap describe-symbol] . helpful-symbol)
         ([remap describe-variable] . helpful-variable)
         ("C-h F" . helpful-function)
         )
  )

(bind-key "C-h K" #'describe-keymap)


;;; boon
(use-package boon
  :init
  (boon-mode)
  :config
  (require 'boon-dvorak)

  (bind-key [remap self-insert-command] 'undefined boon-command-map)
  (bind-key "SPC" #'consult-line boon-forward-search-map)
  (bind-key "C-k" #'boon-set-command-state boon-insert-map)
  (unbind-key "j" boon-command-map)

  (bind-key "n" #'indent-rigidly-right indent-rigidly-map)
  (bind-key "t" #'indent-rigidly-left indent-rigidly-map)
  )


;;; Vertico
(use-package vertico
  :bind (:map vertico-map
              ("C-f" . vertico-insert)
              ("C-n" . vertico-next)
              ("C-t" . vertico-previous)
              ("M-t" . vertico-directory-up)
              )
  :custom (vertico-cycle t)
  :config
  (require 'vertico-directory)
  )

(vertico-mode 1)

;; Completion with Vertico
(bind-key "C-f" #'completion-at-point)


;;; Marginalia
(use-package marginalia
  :custom (marginalia-annotators (marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode 1)
  )


;;; Consult
(use-package consult
  :bind ("C-s" . consult-line)
  :bind (:map minibuffer-local-map ("C-r" . consult-history))
  :init
  (defun ttuegel/completion-with-vertico (&rest args)
    "Use `consult-completion-in-region' if Vertico is enabled."
    (if vertico-mode
        (apply #'consult-completion-in-region args)
      (apply #'completion--in-region args)
      )
    )
  (setq completion-in-region-function #'ttuegel/completion-with-vertico)

  (bind-key [remap isearch-edit-string] #'consult-isearch-history isearch-mode-map)
  )


;;; Orderless

(use-package orderless
  :custom (completion-styles '(orderless partial-completion basic))
  )


;;; Embark

(use-package embark
  :bind ([remap describe-bindings] . embark-bindings)
  :bind ("C-." . embark-act)
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  )

(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode)
  )


;;; deadgrep -- fast, friendly searching with ripgrep
(use-package deadgrep
  :defer
  )


;;; imenu-anywhere
(use-package imenu-anywhere
  :bind (:map boon-goto-map ("i" . imenu-anywhere))
  )


;;; Buffers
(defvar buffer-map)
(define-prefix-command 'buffer-map)
(bind-key "b" #'buffer-map ctl-x-map)
(bind-keys
 :map buffer-map
 ("b" . consult-buffer)
 ("C-b" . buffer-menu)
 ("C-k" . kill-buffer)
 ("k" . kill-current-buffer)
 ("n" . switch-to-next-buffer)
 ("p" . switch-to-prev-buffer)
 ("R" . revert-buffer-quick)
 )


;;; Windows
(defvar window-map)
(define-prefix-command 'window-map)
(bind-key "w" #'window-map ctl-x-map)
(bind-keys
 :map window-map
 ("c" . windmove-up)
 ("r" . windmove-down)
 ("t" . windmove-left)
 ("n" . windmove-right)
 ("_" . split-window-below)
 ("|" . split-window-right)
 ("e" . delete-window)
 ("E" . delete-other-windows)
 )


;;; rainbow-delimiters

(use-package rainbow-delimiters
  :hook (emacs-lisp-mode . rainbow-delimiters-mode)
  )

;;; Avy

(use-package avy
  :config
  (setq avy-keys '(?a ?o ?e ?u ?h ?t ?n ?s))
  )


;;; flycheck

(use-package flycheck
  :hook (emacs-lisp-mode . flycheck-mode))


;;; Git

(use-package magit
  :bind (:map vc-prefix-map ("g" . magit-status))
  :diminish auto-revert-mode
  )

(use-package git-timemachine
  :commands git-timemachine-toggle
  :bind (:map vc-prefix-map ("t" . git-timemachine-toggle))
  )


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
  :after tex
  :commands font-latex-setup
  :hook (TeX-mode . font-latex-setup)
  :config
  ;; Disable Unicode fontification
  (setq font-latex-fontify-script nil)
  (setq font-latex-fontify-sectioning 'color)
  (add-to-list 'font-latex-math-environments "dmath")
  )

(use-package reftex
  :after tex
  :commands reftex-mode
  :hook (TeX-mode . reftex-mode)
  :config
  (setq reftex-plug-into-AUCTeX t)
  (setq reftex-default-bibliography "~/bib/default.bib")
  )


;;; Markdown
(use-package markdown-mode
  :mode "\\.md\\'"
  )


;;; Nix
(use-package nix-mode
  :commands nix-mode
  :hook (nix-mode . rainbow-delimiters-mode)
  :mode "\\.nix\\'"
  :mode "\\.nix.in\\'"
  )


;;; YAML

(use-package yaml-mode
  :defer
  )


;;; yasnippet
(use-package yasnippet
  :hook (lsp-mode . yas-minor-mode)
  :config
  (yas-reload-all)
  )


;;; Language Server

(use-package lsp-mode
  :commands lsp
  :hook (lsp-mode . lsp-enable-which-key-integration)
  :diminish lsp-mode
  :custom (lsp-completion-provider :none "Using orderless.")
  :init
  (defun ttuegel/lsp-completion-mode-setup ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))
  :hook (lsp-completion-mode . ttuegel/lsp-completion-mode-setup)
  :custom (lsp-auto-configure t)
  :custom (lsp-diagnostic-package :flycheck)
  :custom (lsp-eldoc-enable-hover nil "Disable hover info in eldoc (slow).")
  :custom (lsp-headerline-breadcrumb-enable nil "Do not clutter the headerline with breadcrumbs.")
  :custom (lsp-modeline-diagnostics-enable nil "Do not display diagnostics on the modeline (duplicates Flycheck).")
  :custom (lsp-modeline-code-actions-enable nil "Do not clutter the modeline with code actions.")
  :custom (lsp-progress-via-spinner nil "Do not clutter the modeline with the slow spinner.")
  :custom (lsp-lens-enable nil "Do not clutter the buffer with code lens.")
  )

(use-package lsp-ui
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :custom (lsp-ui-doc-enable nil)
  :custom (lsp-ui-peek-enable nil)
  :custom (lsp-ui-sideline-enable nil)
  :custom (lsp-ui-imenu-enable t)
  )

(use-package lsp-java :hook (java-mode . lsp))

(use-package lsp-haskell
  :defer ; Loaded automatically by lsp-mode.
  )

;;; Eglot

(use-package eglot)


;;; Haskell
(defun ttuegel/set-haskell-mode-whitespace-style ()
  "Set up `whitespace-style' for `haskell-mode'."
  (setq-local whitespace-style '(face lines trailing tabs)))

(use-package haskell-mode
  :hook (haskell-mode . rainbow-delimiters-mode)
  :hook (haskell-mode . display-line-numbers-mode)
  :hook (haskell-mode . ttuegel/set-haskell-mode-whitespace-style)
  :config
  (setq haskell-literate-default 'tex)
  (setq haskell-process-log t)
  )


;;; Dhall
(use-package dhall-mode
  :mode "\\.dhall\\'"
  :custom (dhall-format-at-save nil))


;;; XML
(use-package nxml-mode
  :mode "\\.rng\\'"
  )


;;; Org
(defvar org-prefix-map)
(define-prefix-command 'org-prefix-map)
(bind-key "o" #'org-prefix-map ctl-x-map)

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :defines
  org-clock-clocktable-default-properties
  org-clock-persist
  :hook (org-mode . turn-on-visual-line-mode)
  :hook (org-mode . turn-on-visual-fill-column-mode)
  :config
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

  ;; Save clock history across Emacs sessions
  (setq org-clock-persist 'history)
  (org-clock-persistence-insinuate)
  )

(use-package org-agenda
  :defines
  org-agenda-ndays
  :bind (:map org-prefix-map ("a" . org-agenda))
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
  :mode "\\.rs\\'"
  :hook (rust-mode . flycheck-mode)
  )

(use-package cargo
  :hook (rust-mode . cargo-minor-mode)
  )

(use-package flycheck-rust
  :after flycheck
  :commands flycheck-rust-setup
  :hook (flycheck-mode . flycheck-rust-setup)
  )


;;; C
(add-hook 'c-mode-common-hook #'flycheck-mode)


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
  :defines pdftotext-insert-text)


;;; Scheme

(use-package scheme
  :hook (scheme-mode . rainbow-delimiters-mode)
  :config
  (put 'unless 'scheme-indent-function 1)
  (put 'match 'scheme-indent-function 1)
  (put 'with-directory 'scheme-indent-function 1)
  (put 'when 'scheme-indent-function 1))


;;; Idris
(use-package idris-mode
  :mode "\\.idr\\'"
  )


;;; Fish
(use-package fish-mode)


;;; EditorConfig
(use-package editorconfig
  :diminish
  :init
  (editorconfig-mode 1))


;;; Groovy (Jenkinsfile)
(use-package groovy-mode
  :mode "Jenkinsfile"
  )


;;; Scala
(use-package scala-mode
  :mode "\\.scala\\'"
  )


;;; Direnv
(use-package direnv)


;;; Kotlin
(use-package kotlin-mode
  :defer
  )


;;; Swift
(use-package swift-mode
  :defer
  )


;;; Typescript
(use-package typescript-mode
  :mode "\\.tsx\\'"
  )


;;; spinner
;; Disable the spinner.
(with-eval-after-load "spinner"
  (defun spinner-start (&optional type-or-object fps delay) nil)
  (defun spinner-stop (&optional spinner) nil)
  )


;;; Garbage collection
;; Make GC pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 4 1024 1024))

(provide 'init)
;;; init.el ends here
