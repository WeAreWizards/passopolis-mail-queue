with (import <nixpkgs> {}).pkgs;
(haskellngPackages.callPackage ./mail-queue.nix {}).env
