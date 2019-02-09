let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages =
        pkgs.haskellPackages.override {
          overrides = haskellPackagesNew: haskellPackagesOld: rec {
            deriv-hs =
                haskellPackagesNew.callPackage ./deriv-hs.nix {};
          };
        };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };
in with pkgs;
haskellPackages.deriv-hs
