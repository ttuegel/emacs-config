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
    use-package

    # Interface
    bind-key
    highlight-indentation
    projectile # project management
    ripgrep deadgrep  # search
    visual-fill-column
    which-key  # display keybindings after incomplete command
    winum eyebrowse # window management
    all-the-icons doom-modeline

    consult
    embark
    helpful
    marginalia
    orderless
    vertico

    # Themes
    diminish
    modus-themes

    # Delimiters
    rainbow-delimiters smartparens

    # Evil
    avy
    evil
    evil-surround
    evil-indent-textobject
    evil-cleverparens

    # Git
    magit
    git-timemachine

    # LaTeX
    auctex
    cdlatex
    company-math

    auto-compile
    direnv
    flycheck

    markdown-mode
    yaml-mode

    # Haskell
    haskell-mode
    dhall-mode

    lsp-mode
    lsp-ui
    lsp-haskell
    lsp-java
    yasnippet

    # Org
    org

    # Rust
    rust-mode cargo flycheck-rust

    # Nix
    nix-mode nix-buffer

    # Idris
    idris-mode

    editorconfig
    fish-mode

    groovy-mode
    scala-mode
    kotlin-mode
    swift-mode
    typescript-mode

  ]);

in

emacs
