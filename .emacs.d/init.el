;;; init.el -- ttuegel's configuration file
;;; Commentary:
;;; Code:

;; Use UTF-8 everywhere
(mapc (lambda (fn) (funcall fn 'utf-8))
      '(set-terminal-coding-system
        set-keyboard-coding-system
        prefer-coding-system))

;; Turn off that damn bell!
(setq visible-bell t)

(defalias 'yes-or-no-p 'y-or-n-p)

;;; Config

;; Because emacs stops processing when it encounters an error, the sections of
;; this config are ordered to leave me with the most functional environment
;; possible if any errors occur.

;; Customization
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-default nil)
 '(blink-cursor-mode nil)
 '(completion-ignored-extensions
   (quote
    (".o" "~" ".bin" ".lbin" ".so" ".a" ".ln" ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".svn/" ".hg/" ".git/" ".bzr/" "CVS/" "_darcs/" "_MTN/" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".dfsl" ".pfsl" ".d64fsl" ".p64fsl" ".lx64fsl" ".lx32fsl" ".dx64fsl" ".dx32fsl" ".fx64fsl" ".fx32fsl" ".sx64fsl" ".sx32fsl" ".wx64fsl" ".wx32fsl" ".fasl" ".ufsl" ".fsl" ".dxl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo" ".hi" ".elc")))
 '(custom-safe-themes t)
 '(default-frame-alist (quote ((cursor-color . "white"))))
 '(el-get-user-package-directory "~/.emacs.d/el-get-user/init")
 '(evil-shift-width 2)
 '(font-latex-math-environments
   (quote
    ("display" "displaymath" "equation" "eqnarray" "gather" "multline" "align" "alignat" "xalignat" "dmath" "math")))
 '(haskell-indentation-cycle-warn nil)
 '(haskell-indentation-starter-offset 2)
 '(haskell-literate-default (quote tex))
 '(haskell-process-auto-import-loaded-modules nil)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines nil)
 '(haskell-process-type (quote cabal-repl))
 '(hi2-layout-offset 4)
 '(hi2-left-offset 4)
 '(ido-auto-merge-work-directories-length -1)
 '(ido-ignore-extensions nil)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(make-backup-files nil)
 '(menu-bar-mode nil)
 '(org-capture-templates
   (quote
    (("n" "Notes" entry
      (file+datetree "~/org/notes.org")
    (("o" "Open Source Software" entry
      (file+datetree "~/org/oss.org")
      "* %?
  %U
  %i
  %a")
     ("p" "Physics" entry
      (file+datetree "~/org/physics.org")
      "* %?
  %U
  %i
  %a")
     ("t" "Todo" entry
      (file "~/org/todo.org")
      "* TODO %?
  %i
  %a"))))
 '(org-default-notes-file "~/org/notes.org")
 '(org-hide-leading-stars t)
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-docview org-gnus org-info org-jsinfo org-habit org-irc org-mew org-mhe org-rmail org-vm org-wl org-w3m)))
 '(tab-always-indent t)
 '(tab-stop-list (number-sequence 2 120 2))
 '(tab-width 2)
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(whitespace-style (quote (face trailing tabs)))
 '(x-select-enable-clipboard nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Source Code Pro"))))
 '(org-level-1 ((t (:inherit nil :foreground "#FD971F" :height 1.0))))
 '(org-level-2 ((t (:inherit nil :foreground "#A6E22E" :height 1.0))))
 '(org-level-3 ((t (:inherit nil :foreground "#66D9EF" :height 1.0))))
 '(org-level-4 ((t (:inherit nil :foreground "#E6DB74" :height 1.0))))
 '(org-level-5 ((t (:inherit nil :foreground "#A1EFE4"))))
 '(org-level-6 ((t (:inherit nil :foreground "#A6E22E"))))
 '(org-level-7 ((t (:inherit nil :foreground "#F92672"))))
 '(org-level-8 ((t (:inherit nil :foreground "#66D9EF")))))

;; Turn on built-in modes
(global-whitespace-mode t)
(show-paren-mode t)
(electric-indent-mode t)

;; Load or install el-get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")

;; Basic packages to make Emacs usable
(el-get 'sync
        '(el-get
          evil
          evil-indent-textobject
          evil-leader
          evil-surround
          helm
          monokai-theme
          org-mode
          undo-tree
          ))

(require 'evil-leader)
(global-evil-leader-mode)

;; Be evil
(require 'evil)
(evil-mode t)

(require 'evil-surround)
(global-evil-surround-mode 1)

(require 'evil-indent-textobject)

(evil-define-command ttuegel/evil-shift-line (count &optional left)
  "Shift the current line right COUNT times (left if LEFT is non-nil).
The line is shifted to the nearest tab stop. Unlike `evil-shift-right-line', the
value of `evil-shift-width' is ignored for better emacs interoperability. Works
even when the line is blank."
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

(defun evil-map (key def &rest bindings)
  (evil-leader--def-keys evil-normal-state-map key def bindings)
  (evil-leader--def-keys evil-visual-state-map key def bindings)
  (evil-leader--def-keys evil-motion-state-map key def bindings)
  (evil-leader--def-keys evil-operator-state-map key def bindings))

;;; Up/down/left/right
(evil-map "t" 'evil-previous-visual-line
          "h" 'evil-next-visual-line
          "d" 'evil-backward-char
          "n" 'evil-forward-char)

;;; Beginning/end of line (home/end)
;; Use back-to-indentation instead of evil-beginning-of-line so that
;; cursor ends up at the first non-whitespace character of a line. 0
;; can be used to go to real beginning of line
(evil-map "_" 'back-to-indentation
          "-" 'evil-end-of-line)

;; Scrolling
(evil-map "\M-t" 'evil-scroll-page-up
          "\M-h" 'evil-scroll-page-down
          "T" 'evil-scroll-up
          "H" 'evil-scroll-down)

;; Execute command: map : to ;
(evil-map "s" 'evil-ex)

;;; Cut/copy/paste
(evil-map "k" 'evil-delete)

;;; Undo Tree
(evil-map "u" 'undo-tree-undo
          "U" 'undo-tree-redo)

;;; Ace Jump
(evil-map "M-f" 'evil-ace-jump-word-mode)
(evil-map "M-F" 'evil-ace-jump-line-mode)

;;; Search
(evil-map "/" 'isearch-forward
          "?" 'isearch-backward
          "l" 'isearch-repeat-forward
          "L" 'isearch-repeat-backward)

(global-set-key (kbd "C-c /") 'isearch-forward)
(global-set-key (kbd "C-c ?") 'isearch-backward)
(global-set-key (kbd "C-c l") 'isearch-repeat-forward)
(global-set-key (kbd "C-c L") 'isearch-repeat-backward)

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
(define-key evil-window-map "w" 'evil-window-new)
(define-key evil-window-map "k" 'evil-window-delete)

(evil-leader/set-key
  "s" ctl-x-map
  "M-s" 'execute-extended-command)

(evil-leader/set-key "TAB"
  (lambda ()
    (interactive)
    (switch-to-buffer (other-buffer (current-buffer) t))))

(evil-leader/set-key
  "bk" 'kill-buffer
  "bK" 'kill-other-buffers
  "bn" 'switch-to-next-buffer
  "bp" 'switch-to-prev-buffer
  "bR" (lambda () (interactive) (revert-buffer nil t))
  "br" 'rename-current-buffer-file
  "bs" 'switch-to-buffer)

(evil-leader/set-key
  "ff" 'find-file
  "fi" 'find-user-init-file
  "fS" 'evil-write-all
  "fs" 'evil-write)

(evil-leader/set-key "jk" 'evil-join)

(evil-leader/set-key
  "en" 'next-error
  "ep" 'previous-error)

(require 'helm-config)
(helm-mode 1)

;; Other packages to load lazily
(el-get 'sync
        '(auctex
          company-mode
          company-ghc
          diminish
          fill-column-indicator
          flycheck
          git-timemachine
          hi2
          haskell-mode
          ledger-mode
          magit
          markdown-mode
          nix-mode
          ;org-mode
          rainbow-delimiters
          yasnippet))

(evil-leader/set-key
  "gs" 'magit-status
  "gt" 'git-timemachine)

(evil-leader/set-key
  "oc" 'org-capture)

(add-hook 'lisp-mode-hook 'turn-on-fci-mode)

(add-to-list 'load-path "~/.emacs.d/elisp")

(defadvice newline (after indent-clean-after-newline activate)
  "Stop ill-behaved major-modes from leaving indentation on blank lines.
After a newline, remove whitespace from the previous line if that line is
only whitespace."
  (progn
    (forward-line -1)
    (beginning-of-line)
    (while
        (re-search-forward "^[[:space:]]+$" (line-end-position) t)
      (replace-match ""))
    (forward-line 1)
    (back-to-indentation)))

(require 'diminish)
(diminish 'global-whitespace-mode)
(diminish 'helm-mode)

(require 'undo-tree)
(global-undo-tree-mode 1)
(diminish 'undo-tree-mode)

(require 'company)
(diminish 'company-mode)
(define-key company-active-map (kbd "M-h") 'company-select-next)
(define-key company-active-map (kbd "M-t") 'company-select-previous)
(global-company-mode)

(provide 'init)
;;; init.el ends here
