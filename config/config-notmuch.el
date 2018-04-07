(require 'notmuch)

(setq notmuch-tag-macro-alist
      (list
       '("k" "+deleted" "-inbox")
       '("m" "+mute" "-inbox")
       '("M" "+mute-thread" "-inbox")
       '("n" "+notice")
       '("s" "+spam")
       '("a" "+ad" "-inbox")))

(defun notmuch-show-apply-tag-macro (key)
  (interactive "k")
  (let
      ((macro (assoc key notmuch-tag-macro-alist)))
    (if macro
        (apply 'notmuch-show-tag-message (cdr macro))
      (message "Not a tag macro: `%s'" key))))

(defun notmuch-search-apply-tag-macro (key)
  (interactive "k")
  (let
      ((macro (assoc key notmuch-tag-macro-alist)))
    (if macro
        (apply 'notmuch-show-tag-message (cdr macro))
      (message "Not a tag macro: `%s'" key))))


(provide 'config-notmuch)
