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

;; It's 2019, every display is wide, and vertically-split windows
;; are unreadable.
(setq split-height-threshold nil)
(setq split-width-threshold 144)

;; Make buffer names more unique
(setq uniquify-buffer-name-style 'forward)

;; Fill column
(setq-default fill-column 80)

;; Whitespace
(require 'whitespace)
(setq-default whitespace-style '(face trailing tabs))
(global-whitespace-mode t)
(diminish 'global-whitespace-mode)

;; Ignore common extensions.
(add-to-list 'completion-ignored-extensions ".elc")
(add-to-list 'completion-ignored-extensions ".hi")
(add-to-list 'completion-ignored-extensions ".o")
(add-to-list 'completion-ignored-extensions ".dyn_hi")
(add-to-list 'completion-ignored-extensions ".dyn_o")

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

;; Delimiters

(show-paren-mode t)

;; browse-url
(setq browse-url-browser-function #'browse-url-firefox)
(setq browse-url-new-window-flag t)


;; Local variables
(setq safe-local-variable-values
      '((haskell-stylish-on-save . t)
        (haskell-indentation-where-pre-offset . 2)
        (haskell-indentation-where-post-offset . 2)
        (intero-targets "kore:lib"
                        "kore:exe:kore-exec"
                        "kore:exe:kore-format"
                        "kore:exe:kore-parser"
                        "kore:exe:prover"
                        "kore:test:kore-test"
                        "kore:bench:kore-parser-benchmark")))



(provide 'init-emacs)
