{ }:
{ nixpkgs }:
let
  inherit (nixpkgs.lib) gitignoreSource;
  inherit (nixpkgs.haskell.lib) dontCheck justStaticExecutables;
in newpkgs: oldpkgs: {
  # most tests are broken on ghcjs even if packages themselves work
  # we also disable library profiling for compile perf
  mkDerivation = args: oldpkgs.mkDerivation (args // {
    doCheck = false;
    enableLibraryProfiling = false;
  });

  # ghcjs-base is broken and there is no fix on their side. downgrade primitive
  # to pre 0.7.0.0
  primitive = oldpkgs.callHackage "primitive" "0.6.4.0" {};

  # nixpkgs has overridden this one extra spicily to test on !i686
  # which ironically includes us, in spite of us blocking tests.
  # Override it harder.
  diagrams-lib = dontCheck oldpkgs.diagrams-lib;

  # ghcjs-dom-0.2.4.0 (released 2016)
  ghcjs-dom = oldpkgs.callPackage ./nix/ghcjs-dom-ghcjs.nix { };

  Carnap = dontCheck (oldpkgs.callCabal2nix "Carnap" (gitignoreSource ./Carnap) { });

  Carnap-Client = oldpkgs.callCabal2nix "Carnap-Client" (gitignoreSource ./Carnap-Client) { };

  Carnap-GHCJS = justStaticExecutables (oldpkgs.callCabal2nix "Carnap-GHCJS" (gitignoreSource ./Carnap-GHCJS) { });
}
