#! /usr/bin/env nix-shell
#! nix-shell -i fish -p fish git nix-prefetch-scripts

set rev $argv[1]
set url https://github.com/emacs-mirror/emacs/archive/$rev.tar.gz
set sha256 (nix-prefetch-url $url --unpack --unpack $argv[2..-1])
echo "{\"url\": \"$url\", \"sha256\": \"$sha256\"}" >emacs.lock.json

