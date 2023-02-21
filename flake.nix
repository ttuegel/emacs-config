{
  description = "Emacs configuration";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs?ref=master";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system: {
      packages.default = self.packages.${system}.emacs;
      packages.emacs = with import nixpkgs { inherit system; };
	      emacs;
	      # emacs.override { nativeComp = false; };
    });
}
