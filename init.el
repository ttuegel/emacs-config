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


;;; Emacs settings

;; Don't EVER touch my init.el!
(eval-after-load "cus-edit"
  '(defun customize-save-variable
       (variable value &optional comment) value))

;; C-x remap
(bind-key "C-q" ctl-x-map)
(bind-key "C-v" #'quoted-insert)
(unbind-key "C-s" ctl-x-map)
(bind-key "s" #'save-buffer ctl-x-map)

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

;; It's 2018, every display is wide, and vertically-split windows
;; are unreadable.
(setq split-height-threshold nil)
(setq split-width-threshold 144)

;; Make buffer names more unique
(setq uniquify-buffer-name-style 'forward)

;; Fill column
(setq-default fill-column 80)

;; Whitespace
(setq whitespace-style '(face trailing tabs))
(global-whitespace-mode t)
(diminish 'global-whitespace-mode)

;; Ignore common extensions.
(add-to-list 'completion-ignored-extensions ".elc")
(add-to-list 'completion-ignored-extensions ".hi")
(add-to-list 'completion-ignored-extensions ".o")
(add-to-list 'completion-ignored-extensions ".dyn_hi")
(add-to-list 'completion-ignored-extensions ".dyn_o")


;;; Fonts

;; Use Iosevka font by default.
(set-frame-font (font-spec :family "Iosevka Type" :size 12.0 :weight 'normal))
(set-face-attribute 'default nil :family "Iosevka Type" :height 120 :weight 'normal)

;; Don't use italics to indicate types.
(custom-theme-set-faces 'user '(font-lock-type-face ((t :slant normal))))


;;; Colors

;; Be easy on the eyes

(setq solarized-use-variable-pitch nil)
(setq solarized-high-contrast-mode-line t)
(setq solarized-height-minus-1 1.0)
(setq solarized-height-plus-1 1.0)
(setq solarized-height-plus-2 1.0)
(setq solarized-height-plus-3 1.0)
(setq solarized-height-plus-4 1.0)
(setq custom-safe-themes t)

(add-to-list 'load-path (relative "./solarized-emacs"))
(require 'solarized-theme)
(load-theme 'solarized-light)



;;; Indentation

;; Electric indent
(electric-indent-mode -1)
(add-hook 'emacs-lisp-mode-hook #'electric-indent-local-mode)

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

(use-package semantic-indent
  :load-path "~/semantic-indent")


;;; Delimiters

(show-paren-mode t)
(require 'rainbow-delimiters)
(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)


;;; Helm

(use-package helm
  :demand
  :config
  (require 'helm-config)
  (require 'helm-files)

  ;; Open Helm in the current window
  (setq helm-split-window-default-side 'same)

  ;; Use `rg' in place of `ag'
  (setq helm-grep-ag-command
        "rg --color=always --smart-case --no-heading --line-number %s %s %s")

  ;; Skip boring files in `helm-find-files'
  (setq helm-ff-skip-boring-files t)

  ;; `C-d' goes up one level in `helm-find-files' and friends
  (bind-key "C-d" 'helm-find-files-up-one-level helm-read-file-map)
  (bind-key "C-d" 'helm-find-files-up-one-level helm-find-files-map)

  ;; Movement keys for `helm-mode'
  (bind-key "C-h" 'helm-next-line helm-map)
  (bind-key "C-t" 'helm-previous-line helm-map)
  (bind-key "C-f" 'helm-execute-persistent-action helm-map)
  (unbind-key "C-n" helm-map))

(helm-mode 1)
(diminish 'helm-mode)

;; Rebind `M-x'
(bind-key "M-<SPC>" #'helm-M-x)

(let ((map ctl-x-map))
  (unbind-key "C-f" map)

  (bind-key "f" #'helm-find-files ctl-x-map)
  (bind-key "M-f" #'helm-multi-files ctl-x-map)

  (bind-key "g" #'helm-do-grep-ag ctl-x-map))

;; Describe key bindings
(use-package helm-descbinds
  :demand :after (helm)
  :commands helm-descbinds-mode)
(helm-descbinds-mode)


;;; Project management

(use-package projectile
  :diminish projectile-mode
  :commands projectile-mode)

(use-package helm-projectile
  :after (helm projectile))

(run-with-idle-timer 0.5 nil (lambda ()
                               (projectile-mode)
                               (helm-projectile-on)))


;;; Window layouts

(use-package eyebrowse :demand :commands eyebrowse-mode)
(eyebrowse-mode t)

(use-package visual-fill-column
  :commands (turn-on-visual-fill-column-mode))


;;; Mode line

(use-package spaceline-config
  :demand :after (helm)
  :commands (spaceline-helm-mode spaceline-emacs-theme)
  :config
  ;; Color modeline by Evil state
  (setq spaceline-highlight-face-func #'spaceline-highlight-face-evil-state)
  (spaceline-helm-mode)
  (spaceline-emacs-theme)
  (spaceline-toggle-buffer-size-off)
  (spaceline-toggle-buffer-encoding-abbrev-off))


;;; Search

(bind-key "C-f" search-map global-map)
(bind-keys
 :map search-map
 ("s" . isearch-forward-regexp)
 ("C-s" . isearch-repeat-forward)
 ("M-s" . isearch-repeat-backward))


;;; Buffers

(define-prefix-command 'buffer-map)
(bind-key "b" buffer-map ctl-x-map)
(bind-keys
 :map buffer-map
 ("b" . helm-buffers-list)
 ("k" . kill-buffer)
 ("C-k" . kill-this-buffer)
 ("n" . switch-to-next-buffer)
 ("p" . switch-to-prev-buffer)
 ("R" . (lambda () (interactive) (revert-buffer nil t)))
 ("r" . rename-current-buffer-file))

(defun ttuegel/beginning-of-line ()
  "`beginning-of-line' if `back-to-indentation' does not move the cursor."
  (interactive)
  (let ((before (point)))
    (unless (eq before (line-beginning-position))
      (back-to-indentation)
      (let ((after (point)))
        (when (eq before after) (beginning-of-line))))))


;;; Be Evil

(setq evil-toggle-key "C-,")

(use-package evil
  :demand
  :config
  (setq-default evil-shift-width tab-width)

  (evil-set-initial-state 'comint-mode 'emacs)

  ;; Restore `esc-map'
  (bind-key "<ESC>" esc-map)

  (bind-key "C-[" #'evil-normal-state evil-visual-state-map)
  (bind-key "C-[" #'evil-normal-state evil-insert-state-map)
  (bind-key "C-[" #'evil-normal-state evil-replace-state-map)

  ;; `C-x' remaps
  (bind-key "<SPC>" ctl-x-map evil-normal-state-map)
  (bind-key "<SPC>" ctl-x-map evil-visual-state-map)

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
    )

  (bind-keys
   :map evil-motion-state-map
   ("t" . evil-previous-visual-line)
   ("h" . evil-next-visual-line)
   ("d" . evil-backward-char)
   ("n" . evil-forward-char)
   ("H" . evil-scroll-down)
   ("T" . evil-scroll-up)
   ("D" . ttuegel/beginning-of-line)
   ("N" . evil-end-of-line)
   ("M-h" . evil-scroll-page-down)
   ("M-t" . evil-scroll-page-up))

  (let ((map evil-normal-state-map))
    (unbind-key "d" map) ; evil-delete
    (unbind-key "D" map) ; evil-delete-line
    )

  (bind-keys
   :map evil-normal-state-map
   ("k" . evil-delete)
   ("K" . evil-delete-line)))

(evil-mode t)

;; Evil addons

(use-package evil-surround
  :demand :after (evil)
  :commands (global-evil-surround-mode))
(global-evil-surround-mode 1)

(use-package evil-indent-textobject
  :demand :after (evil))

;;; Undo Tree

(use-package undo-tree
  :demand :after (evil)
  :commands global-undo-tree-mode
  :config
  (let ((map undo-tree-map))
    (unbind-key "C-_" map)
    (unbind-key "M-_" map))
  (bind-keys
   :map evil-normal-state-map
   ("u" . undo-tree-undo)
   ("U" . undo-tree-redo)))

(global-undo-tree-mode 1)
(diminish 'undo-tree-mode)


;;; Avy

(use-package avy
  :after (evil)
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


;;; Git

(bind-key "z" vc-prefix-map ctl-x-map)

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


;;; TeX

(use-package tex
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
  :after (tex)
  :config
  ;; Disable Unicode fontification
  (setq font-latex-fontify-script nil)
  (setq font-latex-fontify-sectioning 'color)
  (add-to-list 'font-latex-math-environments "dmath"))

(use-package reftex
  :after (tex)
  :init
  (push #'reftex-mode TeX-mode-hook)
  :config
  (setq reftex-plug-into-AUCTeX t)
  (setq reftex-default-bibliography "~/bib/default.bib"))

(use-package helm-bibtex
  :after (helm tex)
  :config
  (setq bibtex-completion-bibliography "~/bib/default.bib")
  (setq bibtex-completion-library-path "~/bib/doc/")
  (setq bibtex-completion-pdf-open-function
        #'helm-open-file-with-default-tool))

(use-package bibtex-fetch
  :load-path "./bibtex-fetch"
  :after (tex))

;; Completion
(use-package company-math
  :after (tex company)
  :init
  (push 'company-math-symbols-latex company-backends)
  (push 'company-latex-commands company-backends))


;;; Markdown

(use-package markdown-mode)


;;; Flycheck

(use-package flycheck)

(use-package flycheck-haskell
  :after (flycheck haskell-mode)
  :commands flycheck-haskell-setup
  :init
  (add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))


;;; Nix

(use-package nix-mode
  :commands nix-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode))
  (add-to-list 'auto-mode-alist '("\\.nix.in\\'" . nix-mode))
  :config
  (add-hook 'nix-mode-hook #'rainbow-delimiters-mode))

(use-package nix-buffer
  :load-path "~/nix-buffer"
  :after (nix-mode))


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


;;; Company

(use-package company
  :diminish company-mode
  :commands global-company-mode
  :config
  (setf company-active-map (make-sparse-keymap))
  (bind-keys
   :map company-active-map
   ("C-h" . company-select-next)
   ("C-t" . company-select-previous)
   ("C-f" . company-complete-selection)
   ("M-f" . company-complete-common)
   ("C-g" . company-abort)))

(run-with-idle-timer 0.5 nil (lambda () (global-company-mode)))


;;; Haskell

(defun ttuegel/after-save-cabal2nix ()
  (when (string-match-p "\\.cabal" buffer-file-name)
    (ttuegel/cabal2nix)))

(use-package haskell-mode
  :after (flycheck)
  :config
  (setq haskell-literate-default 'tex)
  (setq haskell-process-log t)

  (add-to-list 'flycheck-disabled-checkers 'haskell-hlint)

  (add-hook 'haskell-mode-hook
            (lambda ()
              (haskell-indent-mode -1)
              (semantic-indent-mode t)
              (flycheck-mode)
              (rainbow-delimiters-mode)))
  (add-hook 'haskell-cabal-mode-hook
            (lambda ()
              (add-hook 'after-save-hook #'ttuegel/after-save-cabal2nix))))


;; Completion
(use-package company-ghci
  :after (company haskell-mode)
  :init
  (add-to-list 'company-backends #'company-ghci)

  (add-hook 'haskell-mode-hook #'company-mode)
  (add-hook 'haskell-interactive-mode-hook #'company-mode))


;;; Dhall

(use-package dhall-mode
  :mode ("\\.dhall\\'" . dhall-mode)
  :config
  (setq dhall-format-at-save nil))


;;; XML

(use-package nxml-mode
  :mode ("\\.rng\\'" . xml-mode))


;;; Org

(use-package org
  :config
  (bind-key "C-c b" #'bibtex-fetch/org-insert-entry-from-clipboard org-mode-map)
  (setq org-catch-invisible-edits 'show)
  (setq org-blank-before-new-entry '((heading . t) (plain-list-item t)))
  (setq org-file-apps
        '((auto-mode . emacs)
          (system . "xdg-open %s")
          ("\\.x?html?\\'" . system)
          ("\\.pdf\\'" . system)))
  (add-hook 'org-mode-hook #'turn-on-visual-line-mode)
  (add-hook 'org-mode-hook #'turn-on-visual-fill-column-mode))


;;; rust

(use-package rust-mode)

(use-package cargo :requires (rust-mode))

(use-package flycheck-rust
  :after (rust-mode)
  :init
  (push #'flycheck-rust-setup flycheck-mode-hook)
  (push #'flycheck-mode rust-mode-hook))


;;; Mail

(use-package message
  :config
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

  (setq mm-discouraged-alternatives '("text/html" "text/richtext" "image/.*"))
  (setq mm-text-html-renderer 'w3m-standalone)
  (setq message-sendmail-envelope-from 'header)
  (setq message-kill-buffer-on-exit t)
  (push '(".nb" . "application/vnd.wolfram.nb") mailcap-mime-extensions))

(use-package sendmail
  :config
  (setq send-mail-function #'sendmail-send-it)
  (setq sendmail-program "msmtp"))

(use-package messages-are-flowing
  :config
  (add-hook 'message-mode-hook
            #'messages-are-flowing-use-and-mark-hard-newlines))


;;; Notmuch

(use-package notmuch
  :config
  (defun ttuegel/notmuch-search-delete (&optional beg end)
    "Delete the selected thread or region.

This function advances to the next thread when finished."
    (interactive (notmuch-search-interactive-region))
    (notmuch-search-tag '("+deleted" "-inbox") beg end)
    (when (eq beg end)
      (notmuch-search-next-thread)))

  (defun ttuegel/notmuch-show-delete ()
    "Delete the thread in the current buffer, then show the next thread from search."
    (interactive)
    (notmuch-show-tag-all '("+deleted" "-inbox"))
    (notmuch-show-next-thread t))

  (defun ttuegel/notmuch-search-mute (&optional beg end)
    "Mute the selected thread or region.

This function advances to the next thread when finished."
    (interactive (notmuch-search-interactive-region))
    (notmuch-search-tag '("+mute-thread" "-inbox") beg end)
    (when (eq beg end)
      (notmuch-search-next-thread)))

  (defun ttuegel/notmuch-show-mute ()
    "Mute the thread in the current buffer, then show the next thread from search."
    (interactive)
    (notmuch-show-tag-message '("+mute" "-inbox"))
    (notmuch-show-next-thread t))

  (defun ttuegel/notmuch-show-mute-thread ()
    "Mute the thread in the current buffer, then show the next thread from search."
    (interactive)
    (notmuch-show-tag-all '("+mute-thread" "-inbox"))
    (notmuch-show-next-thread t))

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

  (let ((map notmuch-search-mode-map))
    (unbind-key "n" map) ; notmuch-search-next-thread
    (unbind-key "p" map) ; notmuch-search-previous-thread
    (unbind-key "l" map) ; notmuch-search-filter

    (bind-key "h" #'notmuch-search-next-thread map)
    (bind-key "t" #'notmuch-search-previous-thread map)
    (bind-key "f" #'notmuch-search-filter map)

    (bind-key "k" #'ttuegel/notmuch-search-delete notmuch-search-mode-map)
    (bind-key "u" #'ttuegel/notmuch-search-mute notmuch-search-mode-map))

  (let ((map notmuch-show-mode-map))
    (bind-key "k" #'ttuegel/notmuch-show-delete map)
    (bind-key "u" #'ttuegel/notmuch-show-mute-thread map)
    (bind-key "U" #'ttuegel/notmuch-show-mute-thread map)))


;;; C

(add-hook 'c-mode-common-hook #'c-guess)


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
  (put 'unless 'scheme-indent-function 1)
  (put 'match 'scheme-indent-function 1)
  (put 'with-directory 'scheme-indent-function 1)
  (put 'when 'scheme-indent-function 1))


;;; Idris

(use-package idris-mode)
(use-package helm-idris :after (idris-mode))


(provide 'init)
;;; init.el ends here
