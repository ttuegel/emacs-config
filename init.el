;;; init.el -- ttuegel's configuration file -*- no-byte-compile: t -*-
;;; Commentary:
;;; Code:

(setq load-prefer-newer t)

(package-initialize)

(require 'auto-compile)
(auto-compile-on-load-mode)
(auto-compile-on-save-mode)

(add-to-list 'load-path "~/.emacs.d/lisp")

(require 'diminish)
(require 'bind-key)

;;; Emacs settings

;; Don't EVER touch my init.el!
(eval-after-load "cus-edit"
  '(defun customize-save-variable
       (variable value &optional comment) value))

(bind-key "C-z" ctl-x-map)

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

;; Make the cursor white by default.
(add-to-list 'default-frame-alist '(cursor-color . "white"))

;; What is this, Microsoft Word?
(menu-bar-mode -1)
(tool-bar-mode -1)
(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))
(add-to-list 'default-frame-alist '(horizontal-scroll-bars . nil))

;; Thank you, but I know what program this is.
(setq inhibit-startup-screen t)

;; Don't update the X selection from the kill-ring.
;; Like Vim, Evil keeps the X selection in the `"' register.
(setq x-select-enable-clipboard nil)

;; Ask `y or n' rather than `yes or no'
(defalias 'yes-or-no-p 'y-or-n-p)

;; It's 2017, everything display is wide, and vertically-split windows
;; are unreadable.
(setq split-height-threshold nil)
(setq split-width-threshold 144)

;; Make buffer names more unique
(setq uniquify-buffer-name-style 'forward)

;; Set color scheme
(setq custom-safe-themes t)
(require 'monokai-theme)
(load-theme 'monokai)

;; Ignore common extensions.
(add-to-list 'completion-ignored-extensions ".elc")

;;; Fonts

;; Use Source Code Pro font by default.

(add-to-list 'default-frame-alist '(font . "Source Code Pro-11"))
(set-face-attribute 'default t :font "Source Code Pro-11")

;; Don't use italics to indicate types.
(custom-theme-set-faces 'user '(font-lock-type-face ((t :slant normal))))

;;; Tabs
(setq-default tab-always-indent t)
(setq-default tab-stop-list (number-sequence 2 120 2))
(setq-default tab-width 2)
(setq indent-tabs-mode nil)

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
(require 'rainbow-delimiters)

;;; Automatic indentation
(electric-indent-mode t)

;;; Window numbers
(setq winum-keymap (make-sparse-keymap)) ; Please don't mess with my maps
(require 'winum)
(winum-mode)

;;; Window layouts
(require 'eyebrowse)
(eyebrowse-mode t)

;;; Mode line
(require 'spaceline-config)
(setq spaceline-highlight-face-func
      #'spaceline-highlight-face-evil-state) ; Color modeline by Evil state
(spaceline-helm-mode)
(spaceline-emacs-theme)

;;; Search
(defvar ttuegel/search-map (make-sparse-keymap))
(bind-key* "C-f" ttuegel/search-map)
(bind-keys
 :map ttuegel/search-map
 ("f" . (lambda () (interactive) (isearch-forward t)))
 ("F" . (lambda () (interactive) (isearch-backward t))))

;;; Buffers
(defvar ttuegel/buffer-map (make-sparse-keymap))
(bind-key* "C-b" ttuegel/buffer-map)
(bind-keys
 :map ttuegel/buffer-map
 ("C-b" . helm-buffers-list)
 ("k" . kill-buffer)
 ("K" . kill-other-buffers)
 ("n" . switch-to-next-buffer)
 ("p" . switch-to-prev-buffer)
 ("R" . (lambda () (interactive) (revert-buffer nil t)))
 ("r" . rename-current-buffer-file))

;;; Errors

(defvar ttuegel/error-map (make-sparse-keymap))
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

;;; Be Evil
(setq evil-toggle-key "C-,")
(require 'evil)

(bind-key* "C-/" 'evil-normal-state)

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
	(bind-keys :map map
						 ("t" . evil-previous-visual-line)
						 ("h" . evil-next-visual-line)
						 ("d" . evil-backward-char)
						 ("n" . evil-forward-char)

						 ("T" . evil-scroll-up)
						 ("H" . evil-scroll-down)
						 ("D" . ttuegel/beginning-of-line)
						 ("N" . end-of-line))

	(unbind-key "C-f" map)
	(bind-key "M-h" #'evil-scroll-page-down map)

	(unbind-key "C-b" map)
	(bind-key "M-t" #'evil-scroll-page-up map))

(let ((map evil-normal-state-map))
	(unbind-key "d" map)
	(bind-key "k" #'evil-delete map)

	(unbind-key "D" map)
	(bind-key "D" #'evil-delete-line))

(setq-default evil-shift-width tab-width)

(evil-mode t)

(require 'evil-surround)
(global-evil-surround-mode 1)

(require 'evil-indent-textobject) ; TODO: bind to something

;;; Undo Tree
(require 'undo-tree)
(bind-keys :map evil-normal-state-map
					 ("u" . undo-tree-undo)
					 ("U" . undo-tree-redo))
(diminish 'undo-tree-mode)
(global-undo-tree-mode 1)

;;; Helm
(require 'helm-config)
(require 'helm-files)

;; Skip boring files in `helm-find-files'
(setq helm-ff-skip-boring-files t)
;; Open Helm in the current window
(setq helm-split-window-default-side 'same)

(bind-key "M-h" helm-command-map)
(bind-key "C-f" 'helm-find-files helm-command-map)

(bind-key "M-z" 'helm-M-x)
(bind-key "C-f" 'helm-find-files ctl-x-map)

(bind-key "C-h" 'helm-next-line helm-map)
(bind-key "C-t" 'helm-previous-line helm-map)
(bind-key "C-n" 'helm-execute-persistent-action helm-map)

;; `C-d' goes up one level in `helm-find-files' and friends
(bind-key "C-d" 'helm-find-files-up-one-level helm-read-file-map)
(bind-key "C-d" 'helm-find-files-up-one-level helm-find-files-map)

(helm-mode 1)
(diminish 'helm-mode)

;;; Avy
(require 'avy)
(setq avy-keys '(?a ?o ?e ?u ?h ?t ?n ?s))
(bind-keys
 :map evil-motion-state-map
 ("f" . avy-goto-char)
 ("F" . avy-goto-line))

;;; Git
(require 'magit)
(diminish 'auto-revert-mode)

(defvar ttuegel/vc-map (make-sparse-keymap))
(bind-key "M-g" ttuegel/vc-map)

(bind-key "s" 'magit-status ttuegel/vc-map)

(require 'git-timemachine)
(bind-key "t" 'git-timemachine-toggle ttuegel/vc-map)

;;; TeX
(require 'tex-site)

(add-hook
 'TeX-mode-hook
 '(progn
		;; Disable Unicode fontification
		(setq font-latex-fontify-script nil)
		(setq font-latex-fontify-sectioning 'color)
		;;(add-to-list 'font-latex-math-environments "dmath")

		(setq TeX-auto-save t)
		(setq TeX-parse-self t)
		(setq TeX-save-query nil)
		(setq TeX-source-correlate-mode t)

		;; Use a proper URL with Okular
		;;(add-to-list 'TeX-view-program-list
		;;							'("Okular" ("okular --unique file:%o"
		;;													(mode-io-correlate "#src:%n%a")) "okular"))

		;; Use absolute filename for "%o" expansion
		;;(add-to-list 'TeX-expand-list
		;;						'("%o" (lambda nil
		;;										 (expand-file-name
		;;											(funcall file (TeX-output-extension) t)))))

		(setq
		 TeX-view-program-selection
		 '(((output-dvi has-no-display-manager) "dvi2tty")
			 (output-dvi "xdvi")
			 (output-pdf "Okular")
			 (output-html "xdg-open")))))

(add-hook 'LaTeX-mode-hook #'yas-minor-mode)
(add-hook 'LaTeX-mode-hook (lambda () (TeX-PDF-mode t)))
(add-hook 'LaTeX-mode-hook (lambda () (flyspell-mode 1)))

(require 'reftex)
(setq reftex-plug-into-AUCTeX t)
(setq reftex-default-bibliography "~/bib/default.bib")
(add-hook 'TeX-mode-hook #'reftex-mode)

(setq bibtex-completion-bibliography "~/bib/default.bib")
(setq bibtex-completion-library-path "~/bib/files/")
(setq bibtex-completion-pdf-open-function
			#'helm-open-file-with-default-tool)

(require 'helm-bibtex)

(add-to-list 'load-path "~/el/bibtex-fetch")
(require 'bibtex-normalize)
(require 'bibtex-fetch)

;;; Markdown
(require 'markdown-mode)

;;; Flycheck
(require 'flycheck)
(add-to-list 'flycheck-disabled-checkers 'haskell-hlint)

;;; Nix
(require 'nix-mode)
(add-hook 'nix-mode-hook #'rainbow-delimiters-mode)

;;; Ledger
(require 'ledger-mode)

;;; YAML
(require 'yaml-mode)

;;; Company
(require 'company)

(bind-keys
 :map company-active-map
 ("C-h" . company-select-next)
 ("C-t" . company-select-previous)
 ("C-n" . company-complete-common)
 ("C-j" . company-complete-selection)
 ("C-g" . company-abort))

(diminish 'company-mode)
(global-company-mode)

;;; Emacs Lisp
(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)

;;; Haskell
(require 'haskell-mode)
(setq haskell-literate-default 'tex)
(setq haskell-auto-import-loaded-modules nil)
(setq haskell-process-log t)
(setq haskell-process-suggest-remove-import-lines nil)
(add-hook 'haskell-mode-hook #'rainbow-delimiters-mode)

(require 'dante)
(add-hook 'haskell-mode-hook #'dante-mode)
(add-hook 'haskell-mode-hook #'flycheck-mode)

;;; XML
(add-to-list 'auto-mode-alist '("\\.rng\\'" . xml-mode))

;;; Org
(require 'org)
(setq org-catch-invisible-edits 'show)

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

(setq mm-text-html-renderer 'w3m)
(setq message-sendmail-envelope-from 'header)
(setq message-kill-buffer-on-exit t)
(setq send-mail-function #'sendmail-send-it)
(setq sendmail-program "msmtp")

;;; Notmuch
(require 'notmuch)
(setq notmuch-search-oldest-first nil)
(setq notmuch-fcc-dirs
			'(("ttuegel@mailbox.org" . "mailbox/INBOX +sent")
				("ttuegel@gmail.com" . "\"gmail/[Gmail].All Mail\" +sent")
				("tuegel2@illinois.edu" . "illinois/INBOX +sent")))

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
(bind-key "u" #'ttuegel/notmuch-show-mute notmuch-show-mode-map)

(require 'w3m)

;;; C
(add-hook 'c-mode-common-hook #'c-guess)

;;; Es
(eval-after-load 'sh-script
  '(let ((es-syntax-table (sh-mode-syntax-table sh-mode-syntax-table ?\` "'")))
		 (add-to-list 'sh-mode-syntax-table-input `(es ,es-syntax-table))))

(provide 'init)
;;; init.el ends here
