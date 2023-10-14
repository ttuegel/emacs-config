;;; init.el -- ttuegel's configuration file -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; straight.el -- bootstrap
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage)
  )
(setq straight-use-package-by-default t)
(straight-use-package 'use-package)

(defun ttuegel/emacs-init-time-message ()
  "Display the Emacs startup time in *Messages*."
  (message "Emacs started in %s" (emacs-init-time))
  )
(add-hook 'emacs-startup-hook #'ttuegel/emacs-init-time-message)

(use-package diminish)

(use-package eldoc
  :diminish
  :custom (eldoc-documentation-stategy 'eldoc-documentation-compose-eagerly)
  :custom (eldoc-display-functions '(eldoc-display-in-buffer))
  )

(use-package auto-compile
  :init
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode)
  )

(use-package which-key
  :diminish which-key-mode
  :custom (which-key-idle-delay 0.2)
  :custom (which-key-popup-type 'minibuffer)
  :init (which-key-mode)
  )

;; Pulse the current line after scrolling.
(defun pulse-line (&rest _)
  "Pulse the current line."
  (pulse-momentary-highlight-one-line (point))
  )
(dolist (command '(scroll-up-command scroll-down-command recenter-top-bottom other-window))
  (advice-add command :after #'pulse-line)
  )

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
  ;; Follow the output in compilation-mode.
  :custom (compilation-scroll-output t)
  ;; Stop `display-buffer' from resizing windows:
  :custom (even-window-sizes nil)
  :custom (sentence-end-double-space nil "One space between sentences")
  :custom (frame-inhibit-implied-resize t "Accept frame size set by window manager")
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

  ;; Show column numbers in the mode line.
  (column-number-mode t)
  )

(load-theme 'modus-operandi t)

(use-package eri-mode
  :straight nil
  :load-path "~/ttuegel/eri-mode"
  :config
  (add-to-list 'indent-line-ignored-functions #'eri-indent)
  )

(defun ttuegel/clipboard-get-contents ()
  "Return the contents of the system clipboard as a string."
  (condition-case nil
      (cond
       ((and (fboundp 'window-system) (window-system)
        (or
          (and (fboundp 'ns-get-pasteboard)
            (ns-get-pasteboard))
          (and (fboundp 'w32-get-clipboard-data)
               (w32-get-clipboard-data))
          (and (and (featurep 'mac)
                 (fboundp 'gui-get-selection))
            (gui-get-selection 'CLIPBOARD 'NSStringPboardType))
          (and (and (featurep 'mac)
                 (fboundp 'x-get-selection))
            (x-get-selection 'CLIPBOARD 'NSStringPboardType))
          ;; todo, this should try more than one request type, as in gui--selection-value-internal
          (and (fboundp 'gui-get-selection)
            (gui-get-selection 'CLIPBOARD (car x-select-request-type)))
          ;; todo, this should try more than one request type, as in gui--selection-value-internal
          (and (fboundp 'x-get-selection)
            (x-get-selection 'CLIPBOARD (car x-select-request-type))))))
       (t
        (error "Clipboard support not available")))
    (error
     (condition-case nil
         (cond
          ((eq system-type 'darwin)
           (with-output-to-string
             (with-current-buffer standard-output
               (call-process "/usr/bin/pbpaste" nil t nil "-Prefer" "txt"))))
          ((eq system-type 'cygwin)
           (with-output-to-string
             (with-current-buffer standard-output
               (call-process "getclip" nil t nil))))
          ((memq system-type '(gnu gnu/linux gnu/kfreebsd))
           (with-output-to-string
             (with-current-buffer standard-output
               (call-process "xsel" nil t nil "--clipboard" "--output"))))
          (t
           (error "Clipboard support not available")))
       (error
        (error "Clipboard support not available")))))
  )
(defun ttuegel/clipboard-set-contents (str-val)
  "Set the contents of the system clipboard to STR-VAL."
  (cl-callf or str-val "")
  (cl-assert (stringp str-val) nil "STR-VAL must be a string or nil")
  (condition-case nil
      (cond
        ((and (fboundp 'window-system) (window-system)
         (or
           (and (fboundp 'ns-set-pasteboard)
             (ns-set-pasteboard str-val))
           (and (fboundp 'w32-set-clipboard-data)
             (w32-set-clipboard-data str-val))
           (and (fboundp 'gui-set-selection)
             (gui-set-selection 'CLIPBOARD str-val))
           (and (fboundp 'x-set-selection)
             (x-set-selection 'CLIPBOARD str-val)))))
        (t
         (error "Clipboard support not available")))
    (error
     (condition-case nil
         (cond
           ((eq system-type 'darwin)
            (with-temp-buffer
              (insert str-val)
              (call-process-region (point-min) (point-max) "/usr/bin/pbcopy")))
           ((eq system-type 'cygwin)
            (with-temp-buffer
              (insert str-val)
              (call-process-region (point-min) (point-max) "putclip")))
           ((memq system-type '(gnu gnu/linux gnu/kfreebsd))
            (with-temp-buffer
              (insert str-val)
              (call-process-region (point-min) (point-max) "xsel" nil nil nil "--clipboard" "--input")))
           (t
            (error "Clipboard support not available")))
       (error
        (error "Clipboard support not available")))))
  )

;; Enable `electric-indent-mode' only in select major modes.
(electric-indent-mode -1)
(add-hook 'emacs-lisp-mode-hook #'electric-indent-local-mode)

(use-package puni
  :bind (("C-c (" . puni-wrap-round)
         ("C-c [" . puni-wrap-square)
         ("C-c {" . puni-wrap-curly)
         ("C-c <" . puni-wrap-angle))
  )

;; Highlight matching parentheses.
(show-paren-mode t)

(use-package expand-region
  :bind ("C-/" . er/expand-region)
  )

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :custom ((doom-modeline-bar-width 8)
           (doom-modeline-icon nil)
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

(use-package helpful
  :bind (([remap describe-command] . helpful-command)
         ([remap describe-function] . helpful-callable)
         ([remap describe-key] . helpful-key)
         ([remap describe-symbol] . helpful-symbol)
         ([remap describe-variable] . helpful-variable)
         ("C-h F" . helpful-function)
         )
  :config
  (bind-keys :map helpful-mode-map ([remap revert-buffer] . helpful-update))
  )
(bind-key "C-h K" #'describe-keymap)

(use-package boon
  :diminish boon-local-mode
  :preface
  (defun boon-enter-dwim ()
    "Enter `boon-insert-state'. Kill the region if it is active."
    (interactive)
    (when (use-region-p)
      (kill-region (region-beginning) (region-end)))
    (boon-set-insert-like-state)
    )
  (defun boon-special-quit-window ()
    "Call `quit-window', but only in `boon-special-state'."
    (interactive)
    (when boon-special-state
      (quit-window))
    )
  :init (boon-mode)
  :config
  (require 'boon-dvorak)

  (bind-key [remap self-insert-command] 'ignore boon-command-map)
  (bind-key "RET" 'ignore boon-command-map)

  (bind-key "q" #'boon-x-map boon-special-map)
  (unbind-key "x" boon-special-map)
  (bind-key "q" #'boon-special-quit-window ctl-x-map)

  (bind-key "j" 'ignore boon-command-map)

  (bind-key "u" #'undo boon-command-map)
  (bind-key "U" #'undo-redo boon-command-map)
  (bind-key "-" 'ignore boon-command-map)
  (unbind-key "C-M-_")

  (bind-key "p" #'boon-splice boon-command-map)
  (bind-key "P" #'yank-pop boon-command-map)

  (bind-key "k" #'boon-take-region boon-command-map)
  (bind-key "K" #'boon-treasure-region boon-command-map)
  (unbind-key "C-k")
  (bind-key "e" 'ignore boon-command-map)
  (bind-key "E" 'ignore boon-command-map)

  (bind-key "C-e" #'boon-set-command-state boon-insert-map)
  (bind-key "e" #'boon-enter-dwim boon-command-map)
  (bind-key "o" 'ignore boon-command-map)
  (bind-key "O" 'ignore boon-command-map)

  (bind-keys :map boon-goto-map
             ("g" . consult-goto-line)
             ("i" . consult-imenu)
             ("I" . consult-imenu-multi)
             )

  (bind-key "," #'goto-last-change boon-backward-search-map)
  (bind-key "." #'goto-last-change-reverse boon-forward-search-map)
  )

(straight-use-package
 '(vertico
   :files (:defaults "extensions/*")
   :includes (vertico-buffer
              vertico-directory
              vertico-flat
              vertico-indexed
              vertico-mouse
              vertico-quick
              vertico-repeat
              vertico-reverse)
   )
 )

(use-package vertico
  :custom (vertico-cycle t)
  :init (vertico-mode 1)
  :config
  (require 'vertico-directory)
  (bind-keps :map vertico-map
             ("C-f" . vertico-insert)
             ("C-n" . vertico-next)
             ("C-t" . vertico-previous)
             ("M-t" . vertico-directory-up)
             )
  )

(use-package vertico-directory
  :after vertico
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  )

(bind-key "C-f" #'completion-at-point)

(use-package marginalia
  :custom (marginalia-annotators (marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode 1)
  )

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

(use-package consult-lsp
  :after (lsp)
  :bind (:map lsp-mode-map ([remap xref-find-apropos] . consult-lsp-symbols))
  )

(use-package orderless
  :custom (completion-styles '(orderless partial-completion basic))
  )

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

(use-package deadgrep
  :defer
  )

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

(defvar window-map)
(define-prefix-command 'window-map)
(bind-key "w" #'window-map ctl-x-map)
(bind-keys
 :map window-map
 ("c" . windmove-up)
 ("r" . windmove-down)
 ("t" . windmove-left)
 ("n" . windmove-right)
 ("C" . windmove-swap-states-up)
 ("R" . windmove-swap-states-down)
 ("T" . windmove-swap-states-left)
 ("N" . windmove-swap-states-right)
 ("_" . split-window-below)
 ("|" . split-window-right)
 ("k" . delete-window)
 ("K" . delete-other-windows)
 ("w" . other-window)
 )

(use-package rainbow-delimiters
  :hook (emacs-lisp-mode . rainbow-delimiters-mode)
  )

(use-package avy
  :config
  (setq avy-keys '(?a ?o ?e ?u ?h ?t ?n ?s))
  )

(use-package flycheck
  :diminish
  :preface
  ;; https://www.masteringemacs.org/article/seamlessly-merge-multiple-documentation-sources-eldoc
  (defun mp-flycheck-eldoc (callback &rest _ignored)
    "Print flycheck messages at point by calling CALLBACK."
    (when-let ((flycheck-errors (and flycheck-mode (flycheck-overlay-errors-at (point)))))
      (mapc
       (lambda (err)
         (funcall callback
           (format "%s: %s"
                   (let ((level (flycheck-error-level err)))
                     (pcase level
                       ('info (propertize "I" 'face 'flycheck-error-list-info))
                       ('error (propertize "E" 'face 'flycheck-error-list-error))
                       ('warning (propertize "W" 'face 'flycheck-error-list-warning))
                       (_ level)))
                   (flycheck-error-message err))
           :thing (or (flycheck-error-id err)
                      (flycheck-error-group err))
           :face 'font-lock-doc-face))
       flycheck-errors))
    )
  (defun mp-flycheck-prefer-eldoc ()
    (add-hook 'eldoc-documentation-functions #'mp-flycheck-eldoc nil t)
    (setq eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
    (setq flycheck-display-errors-function nil)
    (setq flycheck-help-echo-function nil))
  :hook (flycheck-mode . mp-flycheck-prefer-eldoc)
  :hook (emacs-lisp-mode . flycheck-mode)
  )

(use-package magit
  :diminish auto-revert-mode
  :bind (:map ctl-x-map ("g" . magit-status))
  )

(use-package git-timemachine
  :commands git-timemachine-toggle
  )

(use-package tex
  :straight auctex
  :commands TeX-PDF-mode
  :commands font-latex-setup
  :defines TeX-save-query
  :defines LaTeX-item-indent
  :defines LaTeX-indent-level
  :defines LaTeX-label-alist
  :hook (TeX-mode . font-latex-setup)
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
  (setq
   TeX-view-program-selection
   '(((output-dvi has-no-display-manager) "dvi2tty")
     (output-dvi "xdvi")
     (output-pdf "Okular")
     (output-html "xdg-open")))
  ;; Use absolute filename for "%o" expansion
  (add-to-list
   'TeX-expand-list
   '("%o" (lambda nil
            (expand-file-name
             (funcall file (TeX-output-extension) t)))))
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

(use-package nix-mode
  :commands nix-mode
  :hook (nix-mode . rainbow-delimiters-mode)
  :mode "\\.nix\\'"
  :mode "\\.nix.in\\'"
  )

(use-package yaml-mode
  :defer
  )

(use-package yasnippet
  :hook (lsp-mode . yas-minor-mode)
  :config (yas-reload-all)
  )

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
  :custom (lsp-enable-file-watchers nil "Do not watch excess files (slow).")
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
  :custom (lsp-haskell-check-project nil)
  )

(use-package eglot
  :preface
  (defun mp-eglot-eldoc ()
    (setq eldoc-documentation-strategy
            'eldoc-documentation-compose-eagerly))
  :hook (eglot-managed-mode . mp-eglot-eldoc)
  :config (add-to-list 'eglot-server-programs '(haskell-mode . ("static-ls")))
  )

(use-package haskell-mode
  :hook (haskell-mode . rainbow-delimiters-mode)
  :hook (haskell-mode . display-line-numbers-mode)
  :hook (haskell-mode . (lambda nil
                          ;; This is sensitive to order: We have to turn off
                          ;; `haskell-indentation-mode' before we turn on
                          ;; `eri-mode' so that the former doesn't reverse
                          ;; changes made by the latter.
                          (haskell-indentation-mode -1)
                          (eri-mode)))
  :hook (haskell-mode . (lambda nil (setq-local create-lockfiles nil)))
  :config
  (setq haskell-literate-default 'tex)
  (setq haskell-process-log t)
  )

;; Use `haskell-compilation-mode' in `ghcid.txt' buffers.
(autoload #'haskell-compilation-mode "haskell-compile" nil t)
(define-derived-mode haskell-ghcid-mode haskell-compilation-mode "haskell-ghcid"
  "Major mode for navigating messages in a \"ghcid.txt\" file."
  (auto-revert-mode)
  (add-hook 'after-revert-hook #'haskell-ghcid-mode nil t)
  (goto-char (point-min))
  (when (looking-at-p "[^[:space:]]")
    (let ((inhibit-read-only t)) (insert "\n"))
    (set-buffer-modified-p nil)
    )
  )
(add-to-list 'auto-mode-alist '("ghcid\\.txt\\'" . haskell-ghcid-mode))

(use-package yesod-mode
  :straight (yesod-mode :type git :host github :repo "lfborjas/yesod-mode")
  )

(use-package dhall-mode
  :mode "\\.dhall\\'"
  :custom (dhall-format-at-save nil)
  )

(use-package nxml-mode
  :straight nil ; ships with Emacs
  :mode "\\.rng\\'"
  )

(defvar org-prefix-map)
(define-prefix-command 'org-prefix-map)
(bind-key "o" #'org-prefix-map mode-specific-map)

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :defines org-clock-clocktable-default-properties
  :defines org-clock-persist
  :hook (org-mode . turn-on-visual-line-mode)
  :config
  (setq org-directory "~/org")
  (setq org-todo-keywords
        (quote ((type "TODO(t)" "STARTED(s@)" "WAITING(w@/!)" "MAYBE(m)"
                      "|" "DONE(d!)" "CANCELLED(c@)")))
        )
  ;; Children block parent TODO items
  (setq org-enforce-todo-dependencies t)
  ;; Log entries into LOGBOOK drawer
  (setq org-log-into-drawer t)
  ;; Notes
  (setq org-default-notes-file "~/org/notes.org"
        org-reverse-note-order t)
  (setq org-catch-invisible-edits 'show)
  (setq org-blank-before-new-entry '((heading . t) (plain-list-item t)))
  (setq org-file-apps
        '((auto-mode . emacs)
          (system . "xdg-open %s")
          ("\\.x?html?\\'" . system)
          ("\\.pdf\\'" . system))
        )
  (setq org-clock-clocktable-default-properties
        '(:maxlevel 2 :scope file :block today)
        )
  ;; Save clock history across Emacs sessions
  (setq org-clock-persist 'history)
  (org-clock-persistence-insinuate)
  )

(use-package org-agenda
  :after org
  :straight nil ; ships with org
  :defines org-agenda-ndays
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
         (("u" "Unscheduled tasks" tags "-SCHEDULED={.+}/!+TODO")))
        )
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
  :after org
  :straight nil ; ships with org
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
                 "* UNREAD %:title%?"))))
  )

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

(add-hook 'c-mode-common-hook #'flycheck-mode)

(defun unfill-region (beg end)
  "Join text paragraphs between BEG and END into a single logical line.
This is useful, e.g., for use with function `visual-line-mode'."
  (interactive "*r")
  (let ((fill-column (point-max)))
    (fill-region beg end))
  )

(use-package pdftotext
  :straight nil
  :load-path "./lisp"
  :defines pdftotext-insert-text
  )

(use-package scheme
  :hook (scheme-mode . rainbow-delimiters-mode)
  :config
  (put 'unless 'scheme-indent-function 1)
  (put 'match 'scheme-indent-function 1)
  (put 'with-directory 'scheme-indent-function 1)
  (put 'when 'scheme-indent-function 1))

(use-package idris-mode
  :mode "\\.idr\\'"
  )

(use-package fish-mode)

(use-package editorconfig
  :diminish
  :init (editorconfig-mode 1)
  )

(use-package groovy-mode
  :mode "Jenkinsfile"
  )

(use-package scala-mode
  :mode "\\.scala\\'"
  )

(use-package kotlin-mode
  :defer
  )

(use-package swift-mode
  :defer
  )

(use-package typescript-mode
  :mode "\\.tsx\\'"
  )

;; Disable the spinner.
(with-eval-after-load "spinner"
  (defun spinner-start (&optional type-or-object fps delay) nil)
  (defun spinner-stop (&optional spinner) nil)
  )

(use-package xterm-color
  :init
  (defun ttuegel/advice-compilation-filter (f proc string)
    (funcall f proc (xterm-color-filter string))
    )
  (with-eval-after-load "compile"
    (add-to-list 'compilation-environment "TERM=xterm")
    (advice-add 'compilation-filter :around #'ttuegel/advice-compilation-filter)
    )
  )


(use-package adaptive-wrap
  :hook (visual-line-mode . adaptive-wrap-prefix-mode)
  )

(use-package envrc
  :init (envrc-global-mode)
  :diminish)


(use-package goto-chg)

(use-package f
  :init
  (defun touch ()
    "Change the timestamp of the file visited by the current buffer."
    (interactive)
    (let ((file-name (buffer-file-name)))
      (when file-name
        (f-touch file-name)
        )
      )
    )
  )


;;; Garbage collection
;; Make GC pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 4 1024 1024))

;;; Customize
(load (expand-file-name "custom.el" user-emacs-directory))

(provide 'init)
;;; init.el ends here
