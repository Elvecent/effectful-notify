{ devMode ? false }:
let
  versions = import ./versions.nix;

  pkgs = import (builtins.fetchTarball versions.nixpkgs) {};

  cronSrc = pkgs.fetchgit {
    url = "https://github.com/tfausak/cron.git";
    sha256 = "sha256-J15UmPAn6JWllYRgYVuVDlrriNOMeHMKIBGL/9Iq0HI=";
    rev = "82f065966af12f799c71f3df458592604a617b75";
  };

  inherit (pkgs.lib.attrsets) mapAttrs;
  
  haskellPackages = pkgs.haskell.packages.${versions.haskellCompiler}.override {
    overrides = self: super:
      mapAttrs (_: v:
        if    builtins.typeOf v == "set"
           && builtins.hasAttr "isHaskellLibrary" v
           && v.isHaskellLibrary
        then
          with pkgs.haskell.lib;
          dontCheck (if devMode
                     then disableLibraryProfiling (disableOptimization v)
                     else v)
        else v) super;
  };
in
haskellPackages.override (old: {
  overrides = pkgs.lib.composeExtensions old.overrides (self: super: with pkgs.haskell.lib;
    let
      new =
        mapAttrs (_: value:
          dontCheck (self.callHackageDirect value {})
        ) versions.haskellOverrides;
    in with pkgs.haskell.lib;
      new // {
        notify-effectful = self.callCabal2nix "notify-effectful" ../. {};
      });
})
