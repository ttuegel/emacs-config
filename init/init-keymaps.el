;;; Keymaps

(setq evil-toggle-key "C-,")

(require 'evil)


;; C-x

(bind-key "C-r" ctl-x-map)
(unbind-key "C-r" evil-normal-state-map)
(unbind-key "C-x")

(bind-key ":" #'execute-extended-command ctl-x-map)


(bind-key "C-v" #'quoted-insert)


;; Evil normal state

(bind-key "C-b" #'evil-normal-state evil-insert-state-map)
(bind-key "C-b" #'evil-normal-state evil-replace-state-map)
(bind-key "C-b" #'evil-normal-state evil-visual-state-map)


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


;; Motion

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
   ("D" . beginning-of-visual-line)
   ("N" . evil-end-of-line)
   ("M-h" . evil-scroll-page-down)
   ("M-t" . evil-scroll-page-up)))

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


(let ((map evil-normal-state-map))
  (unbind-key "d" map) ; evil-delete
  (unbind-key "D" map) ; evil-delete-line

  (bind-keys
   :map map
   ("k" . evil-delete)
   ("K" . evil-delete-line)))


;; Buffers

(define-prefix-command 'buffer-map)
(bind-key "b" buffer-map ctl-x-map)
(bind-keys
 :map buffer-map
 ("b" . switch-to-buffer)
 ("C-b" . buffer-menu)
 ("k" . kill-buffer)
 ("C-k" . kill-this-buffer)
 ("n" . switch-to-next-buffer)
 ("p" . switch-to-prev-buffer)
 ("R" . (lambda () (interactive) (revert-buffer nil t)))
 ("r" . rename-current-buffer-file))


;; Undo

(require 'undo-tree)

(let ((map undo-tree-map))
  (unbind-key "C-_" map)
  (unbind-key "M-_" map))

(bind-keys
    :map evil-normal-state-map
    ("u" . undo-tree-undo)
    ("U" . undo-tree-redo))

(global-undo-tree-mode 1)
(diminish 'undo-tree-mode)


(provide 'init-keymaps)
