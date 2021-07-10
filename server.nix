{ client, persistent, withHoogle ? true, profiling ? false }:
{ nixpkgs }:
let
  inherit (nixpkgs.lib) gitignoreSource;
  inherit (nixpkgs.haskell.lib)
    disableSharedExecutables
    disableLibraryProfiling
    doJailbreak
    dontCheck
    justStaticExecutables
    overrideSrc
    overrideCabal;
in
newpkgs: oldpkgs: {
  # TODO: This has actually been fixed as of 2021-01-09 but it probably hasn't
  # got into our nixpkgs. Need to update that.
  # downgrade to 1.8.x because yesod-auth-oauth2 has not fixed support for
  # newer versions in any *released* version yet
  hoauth2 = oldpkgs.callHackage "hoauth2" "1.8.9" { };

  # update to fix a possible security bug: https://github.com/thoughtbot/yesod-auth-oauth2/issues/132
  yesod-auth-oauth2 = oldpkgs.callHackageDirect {
    pkg = "yesod-auth-oauth2";
    ver = "0.6.1.3";
    sha256 = "1bikn9kfw6mrsais4z1nk07aa7i7hyrcs411kbbfgc7n74k6sd5b";
  } { };

  # failing tests on macOS for some reason:
  # https://github.com/ubc-carnap-team/Carnap/runs/1074093219?check_suite_focus=true
  tz = dontCheck oldpkgs.tz;

  # Use the version from https://github.com/yesodweb/persistent/pull/1106
  # using overrideSrc to maintain dependency relations and nix's fixes/overrides for these
  persistent = overrideSrc oldpkgs.persistent { src = (persistent + "/persistent"); };
  persistent-sqlite = overrideSrc oldpkgs.persistent-sqlite { src = (persistent + "/persistent-sqlite"); };
  # this one we have to recreate from scratch anyway because they added a dependency
  persistent-postgresql = dontCheck (oldpkgs.callCabal2nix "persistent-postgresql" (persistent + "/persistent-postgresql") { });
  persistent-template = overrideSrc oldpkgs.persistent-template { src = (persistent + "/persistent-template"); };
  persistent-qq = overrideSrc oldpkgs.persistent-qq { src = (persistent + "/persistent-qq"); };
  persistent-test = overrideSrc oldpkgs.persistent-test { src = (persistent + "/persistent-test"); };
  # too tight an upper version bound on persistent
  yesod-persistent = doJailbreak oldpkgs.yesod-persistent;
  yesod-auth = doJailbreak oldpkgs.yesod-auth;

  # they wrote a spec that calls out to Google. It does not work in a nix
  # builder.
  oidc-client = dontCheck oldpkgs.oidc-client;

  # lti13 and yesod-auth-lti13 are not in nixpkgs yet
  # lti13 = oldpkgs.callCabal2nix "lti13" ../lti13/lti13 { };
  # yesod-auth-lti13 = oldpkgs.callCabal2nix "yesod-auth-lti13" ../lti13/yesod-auth-lti13 { };
  lti13 = oldpkgs.callHackageDirect {
    pkg = "lti13";
    ver = "0.2.0.0";
    sha256 = "0j6pjrmpps36ppg750wnmksvpgv2i14pfzpwg3zxhzpniigpxd4x";
  } { };
  yesod-auth-lti13 = oldpkgs.callHackageDirect {
    pkg = "yesod-auth-lti13";
    ver = "0.2.0.0";
    sha256 = "130ylr7prhw9j5lx5wni70nb2v72jcc9l7l6zk30camg1q96r2mg";
  } { };

  # dontCheck: https://github.com/gleachkr/Carnap/issues/123
  Carnap        = disableLibraryProfiling
                    (dontCheck (oldpkgs.callCabal2nix "Carnap" (gitignoreSource ./Carnap) { }));
  Carnap-Client = disableLibraryProfiling
                    (oldpkgs.callCabal2nix "Carnap-Client" (gitignoreSource ./Carnap-Client) { });

  Carnap-Server = justStaticExecutables ((overrideCabal
    (oldpkgs.callCabal2nix "Carnap-Server" (gitignoreSource ./Carnap-Server) { })
    (old: let book = ./Carnap-Book; in {
      preConfigure = ''
        mkdir -p $out/share
        echo ":: Copying Carnap-Server data"
        cp -r ${book} $out/share/book

        echo ":: Copying js in $(pwd)"
        find static/ghcjs/allactions/ -type l -delete
        cp ${client.out}/bin/AllActions.jsexe/all.js static/ghcjs/allactions/
        cp ${client.out}/bin/AllActions.jsexe/out.js static/ghcjs/allactions/
        cp ${client.out}/bin/AllActions.jsexe/lib.js static/ghcjs/allactions/
        cp ${client.out}/bin/AllActions.jsexe/runmain.js static/ghcjs/allactions/
        echo ":: Adding a universal settings file"
        cp -n config/settings-example.yml config/settings.yml
        cp -r {config,static} $out/share
        cat config/settings.yml
        '';

      enableExecutableProfiling = profiling;
      enableLibraryProfiling = profiling;
      buildDepends = [ book client ];

      isExecutable = true;
      # Carnap-Server has no tests/they are broken
      doCheck = false;
      # remove once updated past ghc865
      # https://github.com/haskell/haddock/issues/979 (additionally disabled by
      # justStaticExecutables)
      doHaddock = false;
    })).overrideAttrs (drv: {
      # inspired by
      # https://github.com/NixOS/nixpkgs/blob/91340ae/pkgs/development/tools/pandoc/default.nix
      # reduce closure size by deleting references to the pandoc binary. pandoc
      # depends transitively on all installed haskell packages.  Bad for docker
      # image size (4GB+).
      disallowedReferences = with newpkgs; [
        warp
        yesod-core
        pandoc
        pandoc-types
        HTTP
        tzdata
      ];
      postInstall = with newpkgs; ''
        echo 'deleting reference to stuff with bins'
        remove-references-to \
          -t ${warp} \
          $out/bin/Carnap-Server
        remove-references-to \
          -t ${yesod-core} \
          $out/bin/Carnap-Server
        remove-references-to \
          -t ${pandoc} \
          $out/bin/Carnap-Server
        remove-references-to \
          -t ${pandoc-types} \
          $out/bin/Carnap-Server
        remove-references-to \
          -t ${HTTP} \
          $out/bin/Carnap-Server
        remove-references-to \
          -t ${tzdata} \
          $out/bin/Carnap-Server
      '';
    }));
}
