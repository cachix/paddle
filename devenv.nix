{ pkgs, lib, config, inputs, ... }:

{
  languages.haskell.enable = true;
  languages.haskell.package = pkgs.haskell.compiler.ghc9103;

  packages = [
    pkgs.openssl
    pkgs.zlib
  ];
}
