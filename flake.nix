{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { flake-utils, nixpkgs, self }:
    flake-utils.lib.eachDefaultSystem (system: {
      devShell = with nixpkgs.legacyPackages."${system}";

        mkShell {
          buildInputs = with nixpkgs.legacyPackages.${system}.haskellPackages; [
            haskell-language-server
            ghcid
            cabal-install
          ];
        };
    });
}
