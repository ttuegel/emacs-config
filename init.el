;;; init.el -- ttuegel's configuration file
;;; Commentary:
;;; Code:

;;; File and package loading

;; Load the .el if it's newer than the .elc
(setq load-prefer-newer t)

(package-initialize)

;; Auto-compile .el files
(require 'auto-compile)
(auto-compile-on-load-mode)
(auto-compile-on-save-mode)

(require 'diminish)
(require 'bind-key)

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

;; It's 2017, every display is wide, and vertically-split windows
;; are unreadable.
(setq split-height-threshold nil)
(setq split-width-threshold 144)

;; Make buffer names more unique
(setq uniquify-buffer-name-style 'forward)

;; Ignore common extensions.
(add-to-list 'completion-ignored-extensions ".elc")

;;; Fonts

;; Use Iosevka font by default.
(set-frame-font (font-spec :family "Iosevka Type" :size 12.0 :weight 'normal))
(set-face-attribute 'default nil :family "Iosevka Type" :height 120 :weight 'normal)

;; Don't use italics to indicate types.
(custom-set-faces '(font-lock-type-face ((t :slant normal))))

;;; Fill column
(setq-default fill-column 80)


;;; Whitespace

(setq whitespace-style '(face trailing tabs))
(global-whitespace-mode t)
(diminish 'global-whitespace-mode)


;;; Indentation

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


(add-to-list 'load-path "~/semantic-indent")
(require 'semantic-indent)


;;; Delimiters

(show-paren-mode t)
(require 'rainbow-delimiters)
(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)


;;; Helm

(require 'helm-config)
(require 'helm-files)

;; Skip boring files in `helm-find-files'

(setq helm-ff-skip-boring-files t)

;; Open Helm in the current window
(setq helm-split-window-default-side 'same)

;; Use `rg' in place of `ag'
(setq helm-grep-ag-command
      "rg --color=always --smart-case --no-heading --line-number %s %s %s")

;; Rebind `M-x'
(bind-key "M-<SPC>" 'helm-M-x)

(let ((map ctl-x-map))
  (unbind-key "C-f" map)

  (bind-key "f" #'helm-find-files ctl-x-map)
  (bind-key "M-f" #'helm-multi-files ctl-x-map)

  (bind-key "g" #'helm-do-grep-ag ctl-x-map))

;; Movement keys for `helm-mode'
(bind-key "C-h" 'helm-next-line helm-map)
(bind-key "C-t" 'helm-previous-line helm-map)
(bind-key "C-f" 'helm-execute-persistent-action helm-map)
(unbind-key "C-n" helm-map)

;; `C-d' goes up one level in `helm-find-files' and friends
(bind-key "C-d" 'helm-find-files-up-one-level helm-read-file-map)
(bind-key "C-d" 'helm-find-files-up-one-level helm-find-files-map)

(helm-mode 1)
(diminish 'helm-mode)

;; Describe key bindings
(require 'helm-descbinds)
(helm-descbinds-mode)


;;; Project management

(require 'projectile)
(require 'helm-projectile)
(projectile-mode)
(diminish 'projectile-mode)


;;; Window layouts

(require 'eyebrowse)
(eyebrowse-mode t)

(require 'visual-fill-column)


;;; Mode line

(require 'spaceline-config)

;; Color modeline by Evil state
(setq spaceline-highlight-face-func #'spaceline-highlight-face-evil-state)

(spaceline-helm-mode)
(spaceline-emacs-theme)

(spaceline-toggle-buffer-size-off)
(spaceline-toggle-buffer-encoding-abbrev-off)


;;; Search

(bind-key "C-f" search-map global-map)
(bind-key "s" #'isearch-forward-regexp search-map)
(bind-key "C-s" #'isearch-repeat-forward search-map)
(bind-key "M-s" #'isearch-repeat-backward search-map)
(bind-key "f" #'avy-goto-char search-map)
(bind-key "F" #'avy-goto-line search-map)


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

(add-to-list 'load-path "~/evil")
(require 'evil)

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

  (bind-key "t" #'evil-previous-visual-line map)
  (bind-key "h" #'evil-next-visual-line map)
  (bind-key "d" #'evil-backward-char map)
  (bind-key "n" #'evil-forward-char map)
  (bind-key "H" #'evil-scroll-down map)
  (bind-key "T" #'evil-scroll-up map)
  (bind-key "D" #'ttuegel/beginning-of-line map)
  (bind-key "N" #'evil-end-of-line map)
  (bind-key "M-h" #'evil-scroll-page-down map)
  (bind-key "M-t" #'evil-scroll-page-up map))

(let ((map evil-normal-state-map))
  (unbind-key "d" map) ; evil-delete
  (unbind-key "D" map) ; evil-delete-line

  (bind-key "k" #'evil-delete map)
  (bind-key "K" #'evil-delete-line map))

(setq-default evil-shift-width tab-width)

(evil-set-initial-state 'comint-mode 'emacs)

(evil-mode t)

;; Evil addons

(require 'evil-surround)
(global-evil-surround-mode 1)

(require 'evil-indent-textobject)


;;; Colors

(setq custom-safe-themes t)

;; Be easy on the eyes

(push (relative "./base16-emacs") load-path)
(push (relative "./base16-emacs/build") custom-theme-load-path)

(require 'base16-theme)
(load-theme 'base16-chalk t)

;;; Undo Tree

(require 'undo-tree)
(diminish 'undo-tree-mode)
(global-undo-tree-mode 1)

(let ((map evil-normal-state-map))
  (bind-key "u" #'undo-tree-undo map)
  (bind-key "U" #'undo-tree-redo map))

(let ((map undo-tree-map))
  (unbind-key "C-_" map)
  (unbind-key "M-_" map))


;;; Avy

(require 'avy)

(bind-keys
 :map evil-motion-state-map
 ("f" . avy-goto-char)
 ("F" . avy-goto-line))

(setq avy-keys '(?a ?o ?e ?u ?h ?t ?n ?s))


;;; Git

(bind-key "z" vc-prefix-map ctl-x-map)

(require 'magit)
(bind-key "g" #'magit-status vc-prefix-map)

(require 'autorevert)
(diminish 'auto-revert-mode)

(require 'git-timemachine)
(bind-key "t" #'git-timemachine-toggle vc-prefix-map)


;;; TeX

(require 'tex)

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

;; Don't offer to label environments
;; (I give semantic labels to important environments only.)
(add-hook 'LaTeX-mode-hook
          (lambda () (setq LaTeX-label-alist nil)))

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

(require 'font-latex)

;; Disable Unicode fontification
(setq font-latex-fontify-script nil)
(setq font-latex-fontify-sectioning 'color)
(add-to-list 'font-latex-math-environments "dmath")

(require 'reftex)
(setq reftex-plug-into-AUCTeX t)
(setq reftex-default-bibliography "~/bib/default.bib")
(add-hook 'TeX-mode-hook #'reftex-mode)

(require 'helm-bibtex)
(setq bibtex-completion-bibliography "~/bib/default.bib")
(setq bibtex-completion-library-path "~/bib/doc/")
(setq bibtex-completion-pdf-open-function
      #'helm-open-file-with-default-tool)

(add-to-list 'load-path (relative "./bibtex-fetch"))
(require 'bibtex-fetch)
(put 'bibtex-fetch/document-path 'safe-local-variable #'stringp)

;; Completion
(require 'company-math)
(push 'company-math-symbols-latex company-backends)
(push 'company-latex-commands company-backends)


;;; Markdown

(require 'markdown-mode)


;;; Flycheck

(require 'flycheck)
  :config
  (add-to-list 'flycheck-disabled-checkers 'haskell-hlint)

(require 'flycheck-haskell)
(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup)


;;; Nix

(defun turn-off-electric-indent-local-mode ()
  "Turn off `electric-indent-mode' in this buffer only."
  (electric-indent-local-mode -1))

(add-to-list 'load-path (relative "./nix-mode"))
(require 'nix-mode)
(add-hook 'nix-mode-hook #'turn-off-electric-indent-local-mode)
(add-hook 'nix-mode-hook #'rainbow-delimiters-mode)

(add-to-list 'load-path "~/nix-buffer")
(require 'nix-buffer)


;;; Ledger

(require 'ledger-mode)


;;; YAML

(require 'yaml-mode)

(defun ttuegel/after-save-hpack ()
  "Run `hpack' after saving `package.yaml'."
  (when (equal (file-name-nondirectory buffer-file-name) "package.yaml")
    (ttuegel/hpack)
    (ttuegel/cabal2nix)))

(defun ttuegel/yaml-mode-hook ()
  (push #'ttuegel/after-save-hpack after-save-hook))

(push #'ttuegel/yaml-mode-hook yaml-mode-hook)


;;; Company

(require 'company)
(diminish 'company-mode)

(setf company-active-map (make-sparse-keymap))
(bind-keys
 :map company-active-map
 ("C-h" . company-select-next)
 ("C-t" . company-select-previous)
 ("C-f" . company-complete-selection)
 ("M-f" . company-complete-common)
 ("C-g" . company-abort))

(global-company-mode)


;;; Haskell

(require 'haskell-mode)

(setq haskell-literate-default 'tex)
(setq haskell-auto-import-loaded-modules nil)
(setq haskell-process-log t)
(setq haskell-process-suggest-remove-import-lines nil)

(push "\\.dyn_hi$" helm-boring-file-regexp-list)
(push "\\.dyn_o$" helm-boring-file-regexp-list)

(defun ttuegel/haskell-mode-hook ()
  (turn-off-electric-indent-local-mode)
  (haskell-indent-mode -1)
  (semantic-indent-mode t)
  (flycheck-mode)
  (rainbow-delimiters-mode))

(push #'ttuegel/haskell-mode-hook haskell-mode-hook)

(defun ttuegel/after-save-cabal2nix ()
  (when (string-match-p "\\.cabal" buffer-file-name)
    (ttuegel/cabal2nix)))

(defun ttuegel/haskell-cabal-mode-hook ()
  (push #'ttuegel/after-save-cabal2nix after-save-hook))

(push #'ttuegel/haskell-cabal-mode-hook haskell-cabal-mode-hook)

;; Completion
(require 'company-ghci)
(push 'company-ghci company-backends)
(add-hook 'haskell-mode-hook #'company-mode)
(add-hook 'haskell-interactive-mode-hook #'company-mode)

;;; XML

(add-to-list 'auto-mode-alist '("\\.rng\\'" . xml-mode))


;;; Org

(require 'org)

(bind-key "C-c b" #'bibtex-fetch/org-insert-entry-from-clipboard org-mode-map)

(setq org-catch-invisible-edits 'show)

(setq org-blank-before-new-entry
        '((heading . t) (plain-list-item t)))

(setq org-file-apps
      '((auto-mode . emacs)
        (system . "xdg-open %s")
        ("\\.x?html?\\'" . system)
        ("\\.pdf\\'" . system)))

(add-hook 'org-mode-hook #'turn-off-electric-indent-local-mode)
(add-hook 'org-mode-hook #'turn-on-visual-line-mode)
(add-hook 'org-mode-hook #'turn-on-visual-fill-column-mode)


;;; rust

(require 'rust-mode)

(require 'cargo)

(require 'flycheck-rust)
(add-hook 'rust-mode-hook #'flycheck-mode)


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

(require' message)

(setq mm-discouraged-alternatives '("text/html" "text/richtext" "image/.*"))
(setq mm-text-html-renderer 'w3m-standalone)
(setq message-sendmail-envelope-from 'header)
(setq message-kill-buffer-on-exit t)
(push '(".nb" . "application/vnd.wolfram.nb") mailcap-mime-extensions)

(require 'sendmail)

(setq send-mail-function #'sendmail-send-it)
(setq sendmail-program "msmtp")

(require 'messages-are-flowing)

(add-hook 'message-mode-hook #'messages-are-flowing-use-and-mark-hard-newlines)


;;; Notmuch

(require 'notmuch)

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
  (bind-key "f" #'notmuch-search-filter map))

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
  (notmuch-search-tag '("+mute-thread" "-inbox") beg end)
  (when (eq beg end)
    (notmuch-search-next-thread)))
(bind-key "u" #'ttuegel/notmuch-search-mute notmuch-search-mode-map)

(defun ttuegel/notmuch-show-mute ()
  "Mute the thread in the current buffer, then show the next thread from search."
  (interactive)
  (notmuch-show-tag-message '("+mute" "-inbox"))
  (notmuch-show-next-thread t))
(bind-key "u" #'ttuegel/notmuch-show-mute-thread notmuch-show-mode-map)

(defun ttuegel/notmuch-show-mute-thread ()
  "Mute the thread in the current buffer, then show the next thread from search."
  (interactive)
  (notmuch-show-tag-all '("+mute-thread" "-inbox"))
  (notmuch-show-next-thread t))
(bind-key "U" #'ttuegel/notmuch-show-mute-thread notmuch-show-mode-map)


;;; C

(add-hook 'c-mode-common-hook #'c-guess)


;;; Es

(require 'sh-script)

(let ((es-syntax-table
       (sh-mode-syntax-table sh-mode-syntax-table ?\` "'")))
  (add-to-list 'sh-mode-syntax-table-input `(es ,es-syntax-table)))


;;; Maxima

(require 'maxima)
(add-to-list 'auto-mode-alist '("\\.mac\\'" . maxima-mode))

(evil-set-initial-state 'inferior-maxima-mode 'emacs)

(require 'imaxima)
(setq imaxima-equation-color "#657b83")


;;; unfill-region

(defun unfill-region (beg end)
  "Unfill the region, joining text paragraphs into a single
logical line.  This is useful, e.g., for use with
`visual-line-mode'."
  (interactive "*r")
  (let ((fill-column (point-max)))
    (fill-region beg end)))


;;; pdftotext

(add-to-list 'load-path (relative "./lisp"))
(require 'pdftotext)


;;; secret

(add-to-list 'load-path "~/el/secret-el")
(require 'secret)


;;; Scheme

(require 'scheme)

(add-hook 'scheme-mode-hook #'rainbow-delimiters-mode)

(put 'unless 'scheme-indent-function 1)
(put 'match 'scheme-indent-function 1)
(put 'with-directory 'scheme-indent-function 1)
(put 'when 'scheme-indent-function 1)


;;; Idris

(require 'idris-mode)
(require 'helm-idris)


(provide 'init)
;;; init.el ends here
