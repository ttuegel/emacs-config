;;; config.el -- Rational Emacs user configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

;; early-config.el unsets `package-enable-at-startup' so that Rational Emacs
;; won't update the package archives. We still want to initialize `package.el',
;; so we will do so now.
(package-initialize)

(customize-set-variable 'rational-startup-inhibit-splash t)

(require 'modus-themes)
(disable-theme 'deeper-blue)
(modus-themes-load-themes)
(modus-themes-load-operandi)

;; Don't piddle backup files everywhere like an un-housebroken puppy.
(setq auto-save-default nil)
(setq make-backup-files nil)

;; Blinking should be reserved for eyelids and indicators that require immediate attention.
(blink-cursor-mode -1)

;; Don't update the X selection from the kill-ring.
;; Like Vim, Evil keeps the X selection in the `=' register.
(setq select-enable-clipboard nil)

;; Make buffer names more unique
(setq uniquify-buffer-name-style 'forward)

;; Display column numbers in the mode line.
(setq column-number-mode t)

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
(setq-default tab-stop-list nil)
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

;; Disable automatic package installation.
;; Get all dependencies from the system (provided by Nix, in this case).
;; TODO: Install required dependencies automatically with Nix.
(defmacro rational-package-install-package (package)
  "Throw an error if PACKAGE is missing."
  `(unless (package-installed-p ,package)
     (error "Package '%s' is not installed!" ,package)
     )
  )

(require 'rational-defaults)

(require 'rational-ui)

(require 'rational-completion)

(unbind-key "C-j" vertico-map)
(unbind-key "C-k" vertico-map)
(unbind-key "M-h" vertico-map)
(unbind-key "TAB" vertico-map)

(bind-key "C-h" #'vertico-next vertico-map)
(bind-key "C-t" #'vertico-previous vertico-map)
(bind-key "M-h" #'vertico-directory-up vertico-map)
(bind-key "C-f" #'vertico-insert vertico-map)
(bind-key "TAB" #'self-insert-command vertico-map)

(customize-set-variable 'corfu-auto nil)
(unbind-key "C-M-i")
(bind-key "C-f" #'completion-at-point)

(require 'rational-windows)

(require 'rational-editing)

(diminish 'whitespace-mode)

(require 'rational-project)

(require 'rational-compile)

(require 'rational-evil)

;; Allow the cursor to move one character beyond the end of the line, unlike Vim
;; but as in Emacs. Prevents the cursor from creeping backwards when pasting
;; under evil-execute-in-normal-state.
(setq evil-move-beyond-eol t)

(setq-default evil-shift-width tab-width)

;; Revert a questionable binding from rational-evil.
(unbind-key "C-M-u")
(bind-key "C-u" #'universal-argument)

;; Rebind C-x to C-r
(bind-key "C-r" ctl-x-map)
(unbind-key "C-r" evil-normal-state-map)
(unbind-key "C-x")

;; Rebind M-x to M-r
(bind-key "M-r" #'execute-extended-command)
(unbind-key "M-x")

;;; Motion

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
   ("D" . evil-beginning-of-line)
   ("N" . evil-end-of-line)
   ("M-h" . evil-scroll-page-down)
   ("M-t" . evil-scroll-page-up)
   )
  )

(unbind-key "C-p") ; previous-line
(unbind-key "C-n") ; next-line
(unbind-key "C-b") ; backward-char
(unbind-key "C-f") ; forward-char

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
 ("C-M-h" . evil-scroll-page-down)
 )

(unbind-key "C-h" evil-insert-state-map)

;;; Windows

;; Use the same commands in Emacs mode.

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
 ("-" . evil-window-split)
 ("|" . evil-window-vsplit)
 ("k" . evil-window-delete)
 )

;;; Editing

(let ((map evil-normal-state-map))
  (unbind-key "d" map) ; evil-delete
  (unbind-key "D" map) ; evil-delete-line

  (bind-keys
   :map map
   ("k" . evil-delete)
   ("K" . evil-delete-line)
   )
  )

;;; Buffers

(defvar buffer-map)
(define-prefix-command 'buffer-map)

(bind-key "b" #'buffer-map ctl-x-map)

(bind-keys
 :map buffer-map
 ("b" . consult-buffer)
 ("C-b" . buffer-menu)
 ("k" . kill-buffer)
 ("C-k" . kill-current-buffer)
 ("n" . switch-to-next-buffer)
 ("p" . switch-to-prev-buffer)
 ("R" . (lambda () (interactive) (revert-buffer nil t)))
 ("r" . rename-current-buffer-file)
 )

;; evil-mode clobbers embark bindings
(unbind-key "M-." evil-normal-state-map)
(unbind-key "C-." evil-normal-state-map)
(bind-key "M-." #'embark-dwim)

;;; which-key -- show available keys after incomplete commands

(require 'which-key)
(which-key-mode)
(diminish 'which-key-mode)

;;; consult

(require 'consult)
(bind-keys
 ("M-g g" . consult-goto-line)
 ("C-s" . consult-isearch-history)
 )

(bind-keys
 :map isearch-mode-map
 ("M-e" . consult-isearch-history)
 ("M-s e" . consult-isearch-history)
 ("M-s l" . consult-line)
 ("M-s L" . consult-line-multi)
 )

;;; Magit

(bind-key "g" #'magit-status vc-prefix-map)

;;; config.el ends here
