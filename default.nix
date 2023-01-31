let
  sources = import ./nix/sources.nix;
  emacs-overlay = import sources."emacs-overlay";
  nixpkgs__ = import sources."nixpkgs" {};
in

{ nixpkgs ? nixpkgs__ }:

let nixpkgs_ = nixpkgs; in
let nixpkgs = import nixpkgs_.path { overlays = [ emacs-overlay ]; }; in

let inherit (nixpkgs) pkgs; in

let
  lock = {
    emacs = pkgs.lib.importJSON ./emacs.lock.json;
  };
in

let
  inherit (pkgs) emacsNativeComp emacsPackagesFor;
  emacsPackages = emacsPackagesFor emacsNativeComp;
  emacs = emacsPackages.emacsWithPackages (epkgs: with epkgs; [
    all-the-icons
    auctex
    auto-compile
    avy
    bind-key
    boon
    cargo
    cdlatex
    company-math
    consult
    deadgrep
    dhall-mode
    diminish
    direnv
    doom-modeline
    editorconfig
    embark
    expand-region
    eyebrowse
    fish-mode
    flycheck
    flycheck-rust
    git-timemachine
    groovy-mode
    haskell-mode
    helpful
    highlight-indentation
    idris-mode
    imenu-anywhere
    kotlin-mode
    lsp-haskell
    lsp-java
    lsp-mode
    lsp-ui
    magit
    marginalia
    markdown-mode
    modus-themes
    nix-buffer
    nix-mode
    orderless
    org
    projectile
    puni
    rainbow-delimiters
    ripgrep
    rust-mode
    scala-mode
    smartparens
    swift-mode
    typescript-mode
    use-package
    vertico
    visual-fill-column
    which-key
    winum
    yaml-mode
    yasnippet
  ]);

in

emacs
