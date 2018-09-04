;;; Keymaps

;; C-x

(bind-key* "C-e" ctl-x-map)
(unbind-key "C-x")

(bind-key* "M-e" #'execute-extended-command)

(bind-key* "C-u" mode-specific-map)
(bind-key* "M-u" #'universal-argument)

(bind-key "C-v" #'quoted-insert)

;; Windows

(define-prefix-command 'window-map)
(bind-key* "C-w" window-map)
(bind-key "|" #'split-window-right window-map)
(bind-key "-" #'split-window-below window-map)
(bind-key "k" #'delete-window window-map)
(bind-key "o" #'delete-other-windows window-map)
(bind-key "w" #'other-window window-map)

;; Motion

(bind-key* "C-d" #'backward-char)
(bind-key* "C-h" #'next-line)
(bind-key* "C-t" #'previous-line)
(bind-key* "C-n" #'forward-char)

(bind-key* "C-S-D" #'beginning-of-visual-line)
(bind-key* "C-S-H" #'scroll-up-command)
(bind-key* "C-S-T" #'scroll-down-command)
(bind-key* "C-S-N" #'end-of-visual-line)

(bind-key "C-k" #'kill-region)
(bind-key "C-S-k" #'kill-line)
(bind-key "M-k" #'copy-region-as-kill)

;; Buffers

(define-prefix-command 'buffer-map)
(bind-key "b" buffer-map ctl-x-map)
(bind-key "b" #'switch-to-buffer buffer-map)
(bind-key "k" #'kill-buffer buffer-map)
(bind-key "C-k" #'kill-this-buffer buffer-map)
(bind-key "d" #'switch-to-prev-buffer buffer-map)
(bind-key "n" #'switch-to-next-buffer buffer-map)
(bind-key "R" (lambda () (interactive) (revert-buffer nil t)) buffer-map)

;; Undo

(require 'undo-tree)

(bind-key* "C-z" #'undo-tree-undo)
(bind-key* "C-S-z" #'undo-tree-redo)
(bind-key "M-z" #'undo-tree-visualize)

(global-undo-tree-mode 1)
(diminish 'undo-tree-mode)

;; Control Mode

(require 'control-mode)

(bind-key "C-b" #'control-mode control-mode-keymap)
(bind-key* "C-b" #'control-mode)

(provide 'init-keymaps)
