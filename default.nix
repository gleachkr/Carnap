args@{ useClientFromCi ? false,
  profiling ? false,
  hls ? false,
}:
let
  ghcjsVer = "ghcjs";
  ghcVer = "ghc8104";
  sources = import ./nix/sources.nix;

  # This is separate since ghcjs often gets broken randomly on nixpkgs
  # unstable, so we always want to use a release version.
  # However, the versions of haskell packages in stable are often too old to
  # use for us. Thus, we want to have a separate one.
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

  client-ci = nixpkgs.fetchzip {
    url = "https://github.com/ubc-carnap-team/Carnap/releases/download/v0.1/client-js.tar.gz";
    sha256 = "026n880c88i44mva40qj0nkalmh1an8shwz8mdpf2yn9kx3z8200";
  };
  client = if useClientFromCi then
    client-ci
  else
    nixpkgs-stable.haskell.packages."${ghcjsVer}".Carnap-GHCJS;

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
            (import ./server.nix {
              inherit profiling client;
            })
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
    inherit nixpkgs nixpkgs-stable client;
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
