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
  inherit (pkgs) emacsGcc emacsPackagesFor;
  emacsPackages = emacsPackagesFor emacsGcc;
  emacs = emacsPackages.emacsWithPackages (epkgs: with epkgs; [
    use-package

    # Interface
    bind-key
    company
    highlight-indentation
    projectile # project management
    ripgrep  # search
    visual-fill-column
    which-key  # display keybindings after incomplete command
    winum eyebrowse # window management

    selectrum prescient consult embark
    selectrum-prescient embark-consult
    company-prescient
    consult-flycheck consult-lsp
    marginalia

    # Themes
    diminish
    modus-themes
    spaceline # modeline beautification
    solarized-theme

    # Delimiters
    rainbow-delimiters smartparens

    # Evil
    avy
    evil
    evil-surround
    evil-indent-textobject
    evil-cleverparens
    undo-tree

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
    lsp-haskell

    lsp-mode lsp-ui lsp-ivy lsp-java lsp-treemacs
    yasnippet

    # Org
    org

    # Rust
    rust-mode cargo flycheck-rust

    # Mail
    notmuch messages-are-flowing

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

    clean-aindent-mode
    ws-butler
  ]);

in

emacs
