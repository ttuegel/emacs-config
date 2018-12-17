;;; init.el -- ttuegel's configuration file
;;; Commentary:
;;; Code:

;;; File and package loading

;(package-initialize)

;; Load the .el if it's newer than the .elc
(setq load-prefer-newer t)

(eval-when-compile (require 'use-package))
(setq use-package-always-defer t)

(require 'bind-key)
(require 'diminish)

(diminish 'eldoc-mode)

;; Auto-compile .el files
(require 'auto-compile)
(auto-compile-on-load-mode)
(auto-compile-on-save-mode)

;; Show available keys after incomplete commands
(require 'which-key)
(which-key-mode)
(diminish 'which-key-mode)


;;; Definitions

(defun relative (file-name)
  (if load-file-name
      (expand-file-name file-name (file-name-directory load-file-name))
    (expand-file-name file-name)))

(defun ttuegel/call-process (outfile program &rest args)
  (let ((out (if outfile `(:file ,outfile) 0)))
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


(add-to-list 'load-path (relative "./init"))
(require 'init-emacs)
(require 'init-modeline)
(require 'init-keymaps)
(require 'init-evil)
(require 'init-faces)


(add-to-list 'load-path (relative "./config"))


(require 'rainbow-delimiters)
(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)


;;; Ivy

(require 'ivy)
(bind-keys
    :map ivy-minibuffer-map
    ("C-h" . next-line)
    ("C-t" . previous-line)
    ("C-d" . backward-char)
    ("C-n" . forward-char)
    ("C-f" . ivy-partial-or-done))
(ivy-mode)
(diminish 'ivy-mode)

(require 'counsel)
(diminish 'counsel-mode)

(use-package swiper :commands swiper)
(bind-key "C-s" #'swiper)


;;; Projectile

(use-package projectile
  :diminish projectile-mode
  :commands projectile-mode
  :config
  (bind-key "C-c p" #'projectile-command-map projectile-mode-map))

(run-with-idle-timer 0.5 nil (lambda () (projectile-mode)))


;;; Visual-Fill-Column

(use-package visual-fill-column
  :commands (turn-on-visual-fill-column-mode))


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
  (add-to-list 'company-backends #'company-capf)
  (add-to-list 'company-backends #'company-dabbrev-code)
  (setf company-active-map (make-sparse-keymap))
  (bind-keys
   :map company-active-map
   ("C-h" . company-select-next)
   ("C-t" . company-select-previous)
   ("C-f" . company-complete-selection)
   ("M-f" . company-complete-common)
   ("C-g" . company-abort)))



;;; Flycheck

(use-package flycheck
  :commands flycheck-mode
  :config
  (setq flycheck-clang-language-standard "c++17"))


;;; Git

(use-package magit
  :init
  (setq vc-handled-backends (delq 'Git vc-handled-backends))
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
     (output-html "xdg-open"))))

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
  (add-hook 'yaml-mode-hook
            (lambda ()
              (add-hook 'after-save-hook #'ttuegel/after-save-hpack))))


;;; Language Server

(use-package lsp-mode
  :config
  (setq lsp-enable-eldoc nil))

(use-package lsp-ui
  :init
  (add-hook 'lsp-mode-hook #'lsp-ui-mode)
  :config
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-ui-doc-enable nil))

(use-package lsp-haskell
  :commands lsp-haskell-enable
  :init
  ;; (add-hook 'haskell-mode-hook #'lsp-haskell-enable))
  )

(use-package company-lsp
  :after company
  :demand
  :config
  (push #'company-lsp company-backends))

(use-package lsp-java
  :commands lsp-java-enable
  :init
  (add-hook 'java-mode-hook #'lsp-java-enable))

;;; Haskell

(defun ttuegel/after-save-cabal2nix ()
  (when (string-match-p "\\.cabal" buffer-file-name)
    (ttuegel/cabal2nix)))

(defun ttuegel/haskell-cabal-mode-hook ()
  (add-hook 'after-save-hook #'ttuegel/after-save-cabal2nix))

;; (use-package intero
;;   :commands intero-global-mode
;;   :config
;;   (setq intero-extra-ghc-options '("-Wall"))
;;   (setq intero-blacklist '("~/ttuegel.github.io")))

;; (intero-global-mode 1)

(defun turn-off-eldoc-mode ()
  "Disable `eldoc-mode' in the current buffer."
  (eldoc-mode -1))

(use-package haskell-mode
  :config
  (setq haskell-literate-default 'tex)
  (setq haskell-process-log t)

  (add-hook 'haskell-mode-hook #'company-mode)
  (add-hook 'haskell-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'haskell-mode-hook #'turn-off-eldoc-mode)
  ;; (add-hook 'haskell-mode-hook #'flycheck-mode)
  (add-hook 'haskell-interactive-mode-hook #'company-mode)
  (add-hook 'haskell-cabal-mode-hook #'ttuegel/haskell-cabal-mode-hook))


;; Completion
(use-package company-ghci
  :commands company-ghci
  :init
  (with-eval-after-load "company"
    (add-to-list 'company-backends #'company-ghci)))


;;; Dhall

(use-package dhall-mode
  :mode ("\\.dhall\\'" . dhall-mode)
  :config
  (setq dhall-format-at-save nil))


;;; XML

(use-package nxml-mode
  :mode ("\\.rng\\'" . xml-mode))


;;; Org

(define-prefix-command 'org-prefix-map)
(bind-key "o" #'org-prefix-map ctl-x-map)

(use-package org
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

  (add-hook 'org-mode-hook #'turn-on-visual-line-mode)
  (add-hook 'org-mode-hook #'turn-on-visual-fill-column-mode)

  ;; Save clock history across Emacs sessions
  (setq org-clock-persist 'history)
  (org-clock-persistence-insinuate))


(use-package org-agenda
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
  (bind-key "M-t" #'org-agenda-drag-line-backward org-agenda-mode-map))


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


;;; Mail

(use-package message
  :commands (message-narrow-to-headers-or-head message-fetch-field)
  :config
  (add-to-list 'messages-buffer-mode-hook #'company-mode)

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

  (setq mm-discouraged-alternatives '("image/.*"))
  (setq mm-text-html-renderer 'w3m)
  (setq message-sendmail-envelope-from 'header)
  (setq message-kill-buffer-on-exit t))

(use-package mailcap
  :config
  (push '(".nb" . "application/vnd.wolfram.nb") mailcap-mime-extensions))

(use-package sendmail
  :commands sendmail-send-it
  :config
  (setq send-mail-function #'sendmail-send-it)
  (setq sendmail-program "msmtp"))

(use-package messages-are-flowing
  :commands messages-are-flowing-use-and-mark-hard-newlines
  :config
  (add-hook 'message-mode-hook
            #'messages-are-flowing-use-and-mark-hard-newlines))


;;; Notmuch

(use-package notmuch
  :config
  (require 'config-notmuch)

  (setq notmuch-search-oldest-first nil)
  (setq notmuch-fcc-dirs
        '(("ttuegel@mailbox.org" . "mailbox/INBOX +sent +mailbox")
          ("ttuegel@gmail.com" . "gmail/all_mail +sent +gmail")
          ("tuegel2@illinois.edu" . "illinois/INBOX +sent +illinois")))
  (setq notmuch-saved-searches
        '((:name "inbox" :query "tag:inbox and not tag:foss and not tag:ci")
          (:name "foss" :query "tag:inbox and tag:foss and not tag:ci")
          (:name "ci" :query "tag:inbox and tag:ci")
          (:name "trash" :query "(tag:notice or tag:ad) and not (tag:inbox or tag:sent or tag:replied)")))

  (bind-key "`" #'notmuch-show-apply-tag-macro notmuch-show-mode-map)
  (bind-key "`" #'notmuch-search-apply-tag-macro notmuch-search-mode-map)

  (let ((map notmuch-search-mode-map))
    (unbind-key "n" map) ; notmuch-search-next-thread
    (unbind-key "p" map) ; notmuch-search-previous-thread
    (unbind-key "l" map) ; notmuch-search-filter

    (bind-key "h" #'notmuch-search-next-thread map)
    (bind-key "t" #'notmuch-search-previous-thread map)
    (bind-key "f" #'notmuch-search-filter map)))


;;; C

(add-hook 'c-mode-common-hook #'c-guess)
(add-hook 'c-mode-common-hook #'flycheck-mode)
(add-hook
 'c-mode-common-hook
 (lambda ()
   (setq-local whitespace-style '(line face trailing))
   (setq-local indent-tabs-mode t)))


;; Makefile

(add-hook
 'makefile-mode-hook
 (lambda ()
   (setq-local whitespace-style '(line face trailing))))


;;; Maxima

(use-package maxima
  :mode ("\\.mac\\'" . maxima-mode)
  :config
  (evil-set-initial-state 'inferior-maxima-mode 'emacs))


(use-package imaxima
  :config
  (setq imaxima-equation-color "#657b83"))


;;; unfill-region

(defun unfill-region (beg end)
  "Unfill the region, joining text paragraphs into a single
logical line.  This is useful, e.g., for use with
`visual-line-mode'."
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


;;; Direnv
(use-package direnv)


(provide 'init)
;;; init.el ends here
