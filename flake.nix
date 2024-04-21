{
  description = "sd-utility";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        ghcVersion = "928";
        packageName = "sd-utility";

        config = {};

        overlays = [
          (final: prev:
            let
              haskellPkgs = final.haskell.packages."ghc${ghcVersion}";
            in {
              myHaskellPackages = haskellPkgs.override {
                overrides = hfinal: hprev: {
                  order-statistic-tree =
                    hfinal.callCabal2nix "order-statistic-tree"
                      (final.fetchFromGitHub {
                        owner = "shlok";
                        repo = "ostree";
                        rev = "1ef23f4b58883194f09579691663313c52743569";
                        sha256 = "sha256-4lhY2M2K7huahaOmKRPXP0yN567aL3HuoyAhH6Fv2+o=";
                      }) {};

                  ${packageName} = (hfinal.callCabal2nix "${packageName}" ./. {});
                };
              };

              ${packageName} = final.myHaskellPackages.${packageName};

              myDevShell = final.myHaskellPackages.shellFor {
                packages = p: [ p.${packageName} ];
                nativeBuildInputs = [
                  haskellPkgs.cabal-install
                  haskellPkgs.haskell-language-server
                ];
              };
            })
        ];

        pkgs = import nixpkgs { inherit config overlays system; };
      in {
        packages = {
          default = pkgs.${packageName};
          ${packageName} = pkgs.${packageName};
        };
        devShells.default = pkgs.myDevShell;
      });
}
