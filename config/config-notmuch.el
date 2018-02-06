(require 'notmuch)


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


(provide 'config-notmuch)
