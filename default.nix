{ ghcjsVer ? "ghcjs",
  ghcVer ? "ghc884",
  profiling ? false,
  hls ? false,
}:
let
  sources = import ./nix/sources.nix;

  # We have this bifurcated setup because of a nixpkgs bug causing ghcjs to not
  # work on unstable:
  # https://github.com/NixOS/nixpkgs/issues/95931
  # However! We also want to provide a haskell-language-server, which landed in
  # nixpkgs after 20.03. So, the server is on unstable and the client side is
  # on 20.03. Duplication: likely pretty much just ghc.
  nixpkgs-stable = import sources.nixpkgs-stable {
      config = {
        # yes, packages are broken, but we fix them ;-)
        allowBroken = true;
      };
      overlays = [
        (import ./nix/gitignore.nix { })
        (import ./nix/compose-haskell-overlays.nix {
          ghcVer = ghcjsVer;
          overlays = [
            (import ./client.nix { })
          ];
        })
      ];
    };

  client = nixpkgs-stable.haskell.packages."${ghcjsVer}".Carnap-GHCJS;
  truth-tree = import sources.truth-tree { inherit nixpkgs; };

  nixpkgs = import sources.nixpkgs {
      config = {
        # yes, packages are broken, but we fix them ;-)
        allowBroken = true;
      };
      overlays = [
        (import ./nix/gitignore.nix { })
        (import ./nix/compose-haskell-overlays.nix {
          inherit ghcVer;
          overlays = [
            (import ./server.nix { inherit profiling truth-tree client ; inherit (sources) persistent; })
          ];
        })
      ];
    };

  inherit (nixpkgs) lib;

  devtools = { isGhcjs }: with nixpkgs.haskell.packages."${ghcVer}"; ([
    Cabal
    cabal-install
    ghcid
    hasktags
    yesod-bin
    # hls is disabled for ghcjs shells because it probably will not work on
    # pure-ghcjs components.
  ] ++ (lib.optional (hls && !isGhcjs) haskell-language-server)
  ) ++ (with nixpkgs; [
    cabal2nix
    niv
  ]);

  in rec {
    inherit nixpkgs nixpkgs-stable client truth-tree;
    server = nixpkgs.haskell.packages."${ghcVer}".Carnap-Server;

    # a ghc-based shell for development of Carnap and Carnap-Server
    # Carnap-GHCJS currently broken on ghc, see `server.nix` for details
    ghcShell = nixpkgs.haskell.packages."${ghcVer}".shellFor {
      packages = p: [ p.Carnap p.Carnap-Client p.Carnap-Server ];
      withHoogle = true;
      buildInputs = devtools { isGhcjs = false; };
    };

    ghcjsShell = nixpkgs-stable.haskell.packages."${ghcjsVer}".shellFor {
      packages = p: [ p.Carnap p.Carnap-Client p.Carnap-GHCJS ];
      withHoogle = true;
      buildInputs = devtools { isGhcjs = true; };
    };
  }
