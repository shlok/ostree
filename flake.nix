{
  description = "order-statistic-tree";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        ghcVersion = "928";
        packageName = "order-statistic-tree";

        config = {};

        overlays = [
          (final: prev:
            let
              haskellPkgs = final.haskell.packages."ghc${ghcVersion}";
            in {
              myHaskellPackages = haskellPkgs.override {
                overrides = hfinal: hprev: { 
                  ${packageName} =
                    # See (*).
                    final.haskell.lib.doBenchmark
                      (hfinal.callCabal2nix "${packageName}" ./. {});
                };
              };

              ${packageName} = final.myHaskellPackages.${packageName};

              myDevShell = final.myHaskellPackages.shellFor {
                packages = p: [ p.${packageName} ];
                nativeBuildInputs = [
                  haskellPkgs.cabal-install
                  haskellPkgs.haskell-language-server
                ];

                # Makes "cabal bench" use dependencies from the Nix package set. Also seems to
                # require (*) above.
                genericBuilderArgsModifier = args: args // { doBenchmark = true; };
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
