;; Don't initialize `package.el' at startup so that Rational Emacs will skip
;; updating the package archives. Updating the archives takes several seconds,
;; but it isn't necessary because we get all our dependencies from Nix.
(setq package-enable-at-startup nil)
