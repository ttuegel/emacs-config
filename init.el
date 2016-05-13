;;; init.el -- ttuegel's configuration file
;;; Commentary:
;;; Code:

(require 'package)

(package-initialize)

(require 'diminish)

(add-to-list 'load-path "~/.emacs.d/lisp")

;; Use UTF-8 everywhere
(mapc (lambda (fn) (funcall fn 'utf-8))
      '(set-terminal-coding-system
        set-keyboard-coding-system
        prefer-coding-system))

;; Turn off that damn bell!
(setq visible-bell t)

;; Don't litter annoying backups everywhere
(setq auto-save-default nil)
(setq make-backup-files nil)

;; End GUI silliness
(blink-cursor-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(custom-theme-set-variables
 'user
 '(inhibit-startup-screen t)
 '(x-select-enable-clipboard nil)
 '(default-frame-alist '((cursor-color . "white"))))

;; Set color scheme
(custom-theme-set-variables 'user '(custom-safe-themes t))
(require 'monokai-theme)
(load-theme 'monokai)

(defalias 'yes-or-no-p 'y-or-n-p)

;;; Config

;; Because emacs stops processing when it encounters an error, the sections of
;; this config are ordered to leave me with the most functional environment
;; possible if any errors occur.

;; Customization
(custom-theme-set-variables
 'user
 '(completion-ignored-extensions
   (quote
    (".o" "~" ".bin" ".lbin" ".so" ".a" ".ln" ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".svn/" ".hg/" ".git/" ".bzr/" "CVS/" "_darcs/" "_MTN/" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".dfsl" ".pfsl" ".d64fsl" ".p64fsl" ".lx64fsl" ".lx32fsl" ".dx64fsl" ".dx32fsl" ".fx64fsl" ".fx32fsl" ".sx64fsl" ".sx32fsl" ".wx64fsl" ".wx32fsl" ".fasl" ".ufsl" ".fsl" ".dxl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo" ".hi" ".elc"))))

(custom-theme-set-faces
 'user
 '(default ((t (:family "Source Code Pro"))))
 '(font-lock-type-face ((t :slant normal))))

;; tab stop settings
(custom-theme-set-variables
 'user
 '(tab-always-indent t)
 '(tab-stop-list (number-sequence 4 120 4))
 '(tab-width 4)
 '(indent-tabs-mode nil))

;; fill-column
(custom-theme-set-variables 'user '(fill-column 80))

;; whitespace-mode
(custom-theme-set-variables 'user '(whitespace-style '(face trailing tabs)))
(global-whitespace-mode t)
(diminish 'global-whitespace-mode)

(show-paren-mode t)
(electric-indent-mode t)

;; Make buffer names more unique
(require 'uniquify)
(custom-theme-set-variables 'user '(uniquify-buffer-name-style 'forward))

;; Be evil
(require 'evil-leader)
(evil-leader/set-leader "C-SPC")
(global-evil-leader-mode)

(require 'evil)
(custom-theme-set-variables 'user '(evil-shift-width 2))
(evil-mode t)

(defun ttuegel/evil-tag-props (props)
  (plist-put
   (plist-put props :weight 'bold)
   :foreground "#272822"))

(defmacro ttuegel/set-evil-tag (var tag props)
  (list 'setq var
        (list 'propertize tag
              ''face
              (list 'quote (ttuegel/evil-tag-props props)))))

(ttuegel/set-evil-tag evil-normal-state-tag " N " (:background "#A6E22E"))
(ttuegel/set-evil-tag evil-emacs-state-tag " E " (:background "#FD971F"))
(ttuegel/set-evil-tag evil-insert-state-tag " I " (:background "#F92672"))
(ttuegel/set-evil-tag evil-motion-state-tag " M " (:background "#66D9EF"))
(ttuegel/set-evil-tag evil-visual-state-tag " V " (:background "#6b6b6b"))
(ttuegel/set-evil-tag evil-operator-state-tag " O " (:background "#AE81FF"))
(ttuegel/set-evil-tag evil-replace-state-tag " R " (:background "#FD5FF0"))

(evil-define-command ttuegel/evil-shift-line (count &optional left)
  "Shift the current line right COUNT times (left if LEFT is non-nil).
The line is shifted to the nearest tab stop. Unlike
`evil-shift-right-line', the value of `evil-shift-width' is
ignored for better emacs interoperability. Works even when the
line is blank."
  (interactive "<c>")
  (let* ((initial-column (current-column))
         (initial-indent (current-indentation))
         (final-indent (indent-next-tab-stop initial-indent left))
         (delta-indent (- final-indent initial-indent))
         (final-column (+ initial-column delta-indent)))
    (progn
      (indent-line-to final-indent)
      (forward-char (- final-column (current-column))))))

(evil-define-command evil-shift-right-line (count)
  (interactive "<c>")
  (ttuegel/evil-shift-line count))

(evil-define-command evil-shift-left-line (count)
  (interactive "<c>")
  (ttuegel/evil-shift-line count 1))

;;; Use C-<return> instead of ESC
(define-key evil-insert-state-map (kbd "C-<return>") 'evil-normal-state)
(define-key evil-emacs-state-map (kbd "C-<return>") 'evil-normal-state)
(define-key evil-motion-state-map (kbd "C-<return>") 'evil-normal-state)
(define-key evil-operator-state-map (kbd "C-<return>") 'evil-normal-state)
(define-key evil-replace-state-map (kbd "C-<return>") 'evil-normal-state)
(define-key evil-visual-state-map (kbd "C-<return>") 'evil-normal-state)

(defun ttuegel/evil-map (key def &rest bindings)
  (evil-leader--def-keys evil-normal-state-map key def bindings)
  (evil-leader--def-keys evil-visual-state-map key def bindings)
  (evil-leader--def-keys evil-motion-state-map key def bindings)
  (evil-leader--def-keys evil-operator-state-map key def bindings))

;;; Up/down/left/right
(ttuegel/evil-map "t" 'evil-previous-visual-line
                  "h" 'evil-next-visual-line
                  "d" 'evil-backward-char
                  "n" 'evil-forward-char)
(global-set-key (kbd "C-t") 'previous-line)
(global-set-key (kbd "C-h") 'next-line)
(global-set-key (kbd "C-d") 'backward-char)
(global-set-key (kbd "C-n") 'forward-char)

;;; Beginning/end of line (home/end)
;; Use back-to-indentation instead of evil-beginning-of-line so that
;; cursor ends up at the first non-whitespace character of a line. 0
;; can be used to go to real beginning of line
(ttuegel/evil-map "_" 'back-to-indentation
                  "-" 'evil-end-of-line)

;; Scrolling
(ttuegel/evil-map "\M-t" 'evil-scroll-page-up
                  "\M-h" 'evil-scroll-page-down
                  "T" 'evil-scroll-up
                  "H" 'evil-scroll-down)

;;; Cut/copy/paste
(ttuegel/evil-map "k" 'evil-delete)

;; mode-line-format

(defun ttuegel/mode-line-buffer-modified ()
  "Mode line indicator that the buffer is modified"
  (if (buffer-modified-p) "[*]" "   "))

(defun ttuegel/mode-line-position ()
  "Mode line position indicator"
  (let ((unpadded (format " (%4s, %2s) "
                          (format-mode-line "%l")
                          (format-mode-line "%c"))))
    (format "%15s" unpadded)))

(setq-default mode-line-format
              '("%e"
                mode-line-front-space
                mode-line-remote
                mode-line-frame-identification
                mode-line-buffer-identification
                "   "
                (:eval (ttuegel/mode-line-buffer-modified))
                (:eval (ttuegel/mode-line-position))
                mode-line-modes
                (vc-mode vc-mode)
                "  "
                mode-line-misc-info
                mode-line-end-spaces))

(setq-default evil-mode-line-format '(before . mode-line-front-space))

;; Undo Tree
(require 'undo-tree)
(diminish 'undo-tree-mode)
(global-undo-tree-mode 1)
(ttuegel/evil-map "u" 'undo-tree-undo
                  "U" 'undo-tree-redo)

;; Helm

;;; Use my development helm version, if present
(when (file-exists-p "~/.emacs.d/helm")
  (add-to-list 'load-path "~/.emacs.d/helm"))

(require 'helm-config)
(require 'helm)
(define-key helm-map (kbd "C-h") nil)
(define-key helm-map (kbd "C-h") 'helm-next-line)
(define-key helm-map (kbd "C-t") 'helm-previous-line)
(define-key helm-map (kbd "C-n") 'helm-execute-persistent-action)

(require 'helm-files)
(define-key helm-read-file-map (kbd "C-d") 'helm-find-files-up-one-level)
(define-key helm-find-files-map (kbd "C-d") 'helm-find-files-up-one-level)
(define-key helm-command-map "b" 'helm-buffers-list)

(evil-leader/set-key "M-SPC" 'helm-M-x)
(global-set-key (kbd "M-SPC") 'helm-M-x)

(define-key ctl-x-map (kbd "C-z") 'helm-find-files)
(evil-leader/set-key "h" helm-command-map)

(custom-theme-set-variables 'user '(helm-ff-skip-boring-files t))

(helm-mode 1)
(diminish 'helm-mode)

;; Avy
(require 'avy)
(ttuegel/evil-map "f" 'avy-goto-char
                  "F" 'avy-goto-line)

;; Search
(ttuegel/evil-map "/" 'isearch-forward
                  "?" 'isearch-backward
                  "l" 'isearch-repeat-forward
                  "L" 'isearch-repeat-backward)

(global-set-key (kbd "C-c /") 'isearch-forward)
(global-set-key (kbd "C-c ?") 'isearch-backward)
(global-set-key (kbd "C-c l") 'isearch-repeat-forward)
(global-set-key (kbd "C-c L") 'isearch-repeat-backward)

;; Windows

(define-key ctl-x-map "w" 'evil-window-map)
(evil-leader/set-key "w" 'evil-window-map)
(define-key evil-window-map "d" 'evil-window-left)
(define-key evil-window-map "D" 'evil-window-move-far-left)
(define-key evil-window-map "h" 'evil-window-down)
(define-key evil-window-map "H" 'evil-window-move-very-bottom)
(define-key evil-window-map "t" 'evil-window-up)
(define-key evil-window-map "T" 'evil-window-move-very-top)
(define-key evil-window-map "n" 'evil-window-right)
(define-key evil-window-map "N" 'evil-window-move-far-right)
(define-key evil-window-map "w" 'evil-window-vnew)
(define-key evil-window-map "W" 'evil-window-new)
(define-key evil-window-map "k" 'evil-window-delete)

(evil-leader/set-key "TAB"
  (lambda ()
    (interactive)
    (switch-to-buffer (other-buffer (current-buffer) t))))

(evil-leader/set-key "C-SPC" ctl-x-map)
(global-set-key (kbd "C-SPC") ctl-x-map)
(define-key ctl-x-map (kbd "C-h") help-map)

(ttuegel/evil-map "C-," 'evil-emacs-state)
(global-set-key (kbd "C-,") 'evil-exit-emacs-state)

(evil-leader/set-key
  "bb" 'helm-buffers-list
  "bk" 'kill-buffer
  "bK" 'kill-other-buffers
  "bn" 'switch-to-next-buffer
  "bp" 'switch-to-prev-buffer
  "bR" (lambda () (interactive) (revert-buffer nil t))
  "br" 'rename-current-buffer-file
  "bs" 'switch-to-buffer)

(evil-leader/set-key "jk" 'evil-join)

(evil-leader/set-key
  "en" 'next-error
  "ep" 'previous-error)

(require 'magit)
(evil-leader/set-key "gs" 'magit-status)

(require 'git-timemachine)
(evil-leader/set-key "gt" 'git-timemachine-toggle)

(require 'org)
(custom-theme-set-variables
  'user
  '(org-modules
    '(org-bbdb org-bibtex org-docview org-gnus org-info org-jsinfo org-habit
              org-irc org-mew org-mhe org-rmail org-vm org-wl org-w3m)))
(evil-leader/set-key
  "oa" 'org-agenda
  "oc" 'org-capture)

(custom-theme-set-variables
  'user
  '(org-default-notes-file "~/org/notes.org")
  '(org-hide-leading-stars t)
  '(org-agenda-files '("~/org"))
  '(org-clock-persist 'history)
  '(org-log-done t))
(custom-theme-set-faces 'user
  '(org-level-1 ((t (:inherit nil :foreground "#FD971F" :height 1.0))))
  '(org-level-2 ((t (:inherit nil :foreground "#A6E22E" :height 1.0))))
  '(org-level-3 ((t (:inherit nil :foreground "#66D9EF" :height 1.0))))
  '(org-level-4 ((t (:inherit nil :foreground "#E6DB74" :height 1.0))))
  '(org-level-5 ((t (:inherit nil :foreground "#A1EFE4"))))
  '(org-level-6 ((t (:inherit nil :foreground "#A6E22E"))))
  '(org-level-7 ((t (:inherit nil :foreground "#F92672"))))
  '(org-level-8 ((t (:inherit nil :foreground "#66D9EF")))))
(org-clock-persistence-insinuate)
(add-hook 'org-mode-hook 'auto-fill-mode)

;;; Custom org-agenda keymap
(define-key org-agenda-mode-map (kbd "h") 'org-agenda-next-item)
(define-key org-agenda-mode-map (kbd "t") 'org-agenda-previous-item)
(define-key org-agenda-mode-map (kbd "C-h") 'org-agenda-next-date-line)
(define-key org-agenda-mode-map (kbd "C-t") 'org-agenda-previous-date-line)
(define-key org-agenda-mode-map (kbd "d") 'org-agenda-todo)

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(evil-leader/set-key
  "os" 'org-save-all-org-buffers)

;; Fix trailing whitespace.

(defun ttuegel/indent-whitespace-hygiene ()
  "Remove whitespace from the current line if it is only whitespace."
  (save-excursion
    (beginning-of-line)
    (while
        (re-search-forward "^[[:space:]]+$" (line-end-position) t)
      (replace-match ""))))

(defadvice newline (after indent-whitespace-hygiene-after-newline activate)
  "Stop ill-behaved major-modes from leaving indentation on blank lines.
After a newline, remove whitespace from the previous line if that line is
only whitespace."
  (progn
    (forward-line -1)
    (ttuegel/indent-whitespace-hygiene)
    (forward-line 1)
    (back-to-indentation)))

(defadvice evil-normal-state
    (after indent-whitespace-hygiene-after-evil-normal-state activate)
  (ttuegel/indent-whitespace-hygiene))

;; rainbow-delimiters

(require 'rainbow-delimiters)

;; AucTeX

(defvar ttuegel/LaTeX-no-autofill-environments
  '("align" "align*" "equation" "equation*")
  "A list of LaTeX environment names in which `auto-fill-mode' should be inhibited.")

(defun ttuegel/LaTeX-auto-fill-function ()
  "This function checks whether point is currently inside one of
the LaTeX environments listed in
`ttuegel/LaTeX-no-autofill-environments'. If so, it inhibits automatic
filling of the current paragraph."
  (let ((do-auto-fill t)
        (current-environment "")
        (level 0))
    (while (and do-auto-fill (not (string= current-environment "document")))
      (setq level (1+ level)
            current-environment (LaTeX-current-environment level)
            do-auto-fill (not (member current-environment ttuegel/LaTeX-no-autofill-environments))))
    (when do-auto-fill
      (do-auto-fill))))

(defun ttuegel/LaTeX-setup-auto-fill ()
  "This function turns on auto-fill-mode and sets the function
used to fill a paragraph to `ttuegel/LaTeX-auto-fill-function'."
  (auto-fill-mode)
  (setq auto-fill-function 'ttuegel/LaTeX-auto-fill-function))

(require 'tex-site)

(add-to-list 'auto-mode-alist '("\\.\\(tex\\|sty\\|cls\\)\\'" . latex-mode))

(custom-theme-set-variables
  'user
  '(safe-local-variable-values (quote ((TeX-command-extra-options . "-shell-escape"))))
  '(font-latex-fontify-script nil)
  '(font-latex-fontify-sectioning 'color)
  '(font-latex-math-environments
    '("display" "displaymath" "equation" "eqnarray" "gather" "multline" "align"
      "alignat" "xalignat" "dmath" "math"))
  '(TeX-auto-save t)
  '(TeX-parse-self t)
  '(TeX-save-query nil)
  '(TeX-source-correlate-mode t)
  '(TeX-view-program-selection
    '(((output-dvi has-no-display-manager) "dvi2tty")
      (output-dvi "xdvi")
      (output-pdf "Okular")
      (output-html "xdg-open"))))

(add-hook 'LaTeX-mode-hook #'yas-minor-mode)
(add-hook 'LaTeX-mode-hook (lambda () (ttuegel/LaTeX-setup-auto-fill)))
(add-hook 'LaTeX-mode-hook (lambda () (flyspell-mode 1)))

;; flycheck

(require 'flycheck)
(setq flycheck-checkers (delq 'haskell-hlint flycheck-checkers))

(require 'ttuegel-haskell-config)

;; nix-mode

(require 'nix-mode)
(add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode))
(add-hook 'nix-mode-hook
          (lambda ()
            ; Use the default Nixpkgs indentation style in nix-mode.
            (setq-local tab-stop-list (number-sequence 2 120 2))
            (setq-local tab-width 2)))

(require 'evil-surround)
(global-evil-surround-mode 1)

(require 'evil-indent-textobject)

(require 'ledger-mode)
(add-to-list 'auto-mode-alist '("\\.ledger\\'" . ledger-mode))

(require 'yasnippet)
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(yas-reload-all)
(diminish 'yas-minor-mode)

;; company-mode

(require 'company)
(diminish 'company-mode)
(define-key company-active-map (kbd "C-h") 'company-select-next)
(define-key company-active-map (kbd "C-t") 'company-select-previous)
(global-company-mode)

(require 'company-ghc)
;(add-to-list 'company-backends 'company-ghc)

;; emacs-lisp-mode

(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)

(defun byte-compile-current-buffer ()
  "`byte-compile' current buffer if it's emacs-lisp-mode and compiled file exists."
  (interactive)
  (when (and (eq major-mode 'emacs-lisp-mode)
             (file-exists-p (byte-compile-dest-file buffer-file-name)))
    (byte-compile-file buffer-file-name)))

(add-hook 'after-save-hook 'byte-compile-current-buffer)

(require 'ats2-mode)

(provide 'init)
;;; init.el ends here
