{
  description = "2048 roguelike game";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs =
    inputs:
    let
      overlay = final: prev: {
        haskell = prev.haskell // {
          packageOverrides =
            hfinal: hprev:
            prev.haskell.packageOverrides hfinal hprev // { tiles = hfinal.callCabal2nix "tiles" ./. { }; };
        };
        tiles = final.haskell.lib.compose.justStaticExecutables final.haskellPackages.tiles;
      };
      perSystem =
        system:
        let
          pkgs = import inputs.nixpkgs {
            inherit system;
            overlays = [ overlay ];
          };
          hspkgs = pkgs.haskellPackages;
        in
        {
          devShell = hspkgs.shellFor {
            withHoogle = true;
            packages = p: [ p.tiles ];
            buildInputs = [
              hspkgs.zlib
              hspkgs.cabal-install
              hspkgs.haskell-language-server
              hspkgs.hlint
              pkgs.bashInteractive
              hspkgs.ghcid
              hspkgs.hspec-discover
              hspkgs.fourmolu
              pkgs.hpack
              pkgs.jq
              pkgs.go-task
              pkgs.nixfmt-rfc-style
            ];
          };
          defaultPackage = pkgs.tiles;
        };
    in
    { inherit overlay; } // inputs.flake-utils.lib.eachDefaultSystem perSystem;
}
