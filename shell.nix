with (import <nixpkgs> {}).pkgs;
(haskellPackages.callPackage ./mail-queue.nix {}).env
