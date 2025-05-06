{
  # Flake inputs
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  # Flake outputs
  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        source = pkgs.lib.sourceByRegex ./. [
          "^backend\.cabal$"
          "^src.*$"
          "^test.*$"
        ];

        pkgs = import nixpkgs { inherit system; };
        
        haskellPackages = pkgs.haskellPackages.override {
          overrides = self: super: {
            backend = super.callCabal2nix "backend" source { };
          };
        };
        backend = haskellPackages.backend;
      in
      {
        
        # Development environment output
        devShells.default = (backend.envFunc { withHoogle = true; }).overrideAttrs (
          final: prev: with haskellPackages; {
            nativeBuildInputs = prev.nativeBuildInputs ++ [
              cabal-install
              ghcid
              haskell-language-server
              pkgs.nil
              pkgs.nixfmt-rfc-style
            ];
            shellHook = ''
              set -o allexport
              source .env && echo "Sourced .env" || echo "No .env to source"
              source .env.local && echo "Sourced .env.local" || echo "No .env.local to source"
              set +o allexport
            '';
          }
        );
      }
    );
}
