{
  description = "Emacs configuration";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs?ref=master";
  inputs.emacs-overlay.url = "github:nix-community/emacs-overlay";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, emacs-overlay, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system: {
      packages.default = self.packages.${system}.emacs;
      packages.emacs =
        with import nixpkgs {
          inherit system;
          overlays = [ emacs-overlay.overlay ];
        };
        let inherit (emacsPackagesFor emacsUnstable) emacsWithPackages; in
        emacsWithPackages (epkgs: with epkgs; [
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
    });
}
