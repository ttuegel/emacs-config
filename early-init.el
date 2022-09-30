;;; early-init.el -- early initialization -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; Garbage collection
;; Increase the GC threshold (bytes) for faster startup.
;; The default is 800 kilobytes.
(setq gc-cons-threshold (* 64 1024 1024))

;;; Loading Emacs Lisp
;; Load the source if it is newer than the compiled file
(setq load-prefer-newer t)

;;; Native compilation
(when (featurep 'native-compile)
  ;; Silence disruptive compiler warnings
  (setq native-comp-async-report-warnings-errors nil)

  ;; Compile asynchronously
  (setq native-comp-deferred-compilation t)

  ;; Set the right directory for the native compilation cache
  (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))
  )

;;; Interface
;; Remove some uneeded interface elements.
;; We can turn anything back on later, if we wish.
(setq inhibit-startup-message t)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(horizontal-scroll-bars) default-frame-alist)
(push '(mouse-color . "white") default-frame-alist)

;; Use UTF-8 everywhere. It's 2022, how is this not default?
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Use "y" and "n" to confirm/negate prompt instead of "yes" and "no"
;; Using `advice' here to make it easy to reverse in custom
;; configurations with `(advice-remove 'yes-or-no-p #'y-or-n-p)'
;;
;; N.B. Emacs 28 has a variable for using short answers, which should
;; be preferred if using that version or higher.
(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  (advice-add 'yes-or-no-p :override #'y-or-n-p))

;; Load the color theme early to avoid flashing a white screen at startup.
(load-theme 'modus-operandi)

;; Use system default monospace font in 12 pt height.
(defun ttuegel/set-font (frame)
  "Configure font when FRAME is created."
  (select-frame frame)
  (when (display-graphic-p)
    (set-frame-font "Monospace-12")))

;; Set font in all extant frames.
(mapc #'ttuegel/set-font (frame-list))

;; Set font in all future frames.
(add-hook 'after-make-frame-functions #'ttuegel/set-font)

(provide 'early-init)
;;; early-init.el ends here
