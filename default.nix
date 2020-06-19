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
  inherit (pkgs) emacsPackagesFor emacsGit;
  emacsPackages = emacsPackagesFor emacsGit;
  emacs = emacsPackages.emacsWithPackages (epkgs: with epkgs; [
    use-package

    # Interface
    bind-key
    company
    ivy counsel swiper
    projectile counsel-projectile  # project management
    ripgrep  # search
    visual-fill-column
    which-key  # display keybindings after incomplete command
    winum eyebrowse # window management

    # Themes
    diminish
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
    git-auto-commit-mode
    git-timemachine
    magit

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
    eglot
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
    w3m

    # Ledger
    ledger-mode flycheck-ledger evil-ledger

    groovy-mode
    scala-mode
  ]);

in

emacs
