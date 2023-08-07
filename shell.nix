{ devMode ? false }:
let
  whenDev = stuff: if devMode then stuff else [];
  haskellPackages = import ./nix/haskellPackages.nix { inherit devMode; };
in
haskellPackages.shellFor {
  packages = p:
    [ p.effectful-notify
    ];
  buildInputs = whenDev [
    haskellPackages.stylish-haskell
  ];
  withHoogle = devMode;
}
