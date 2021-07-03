{ client, withHoogle ? true, profiling ? false }:
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
  # failing tests on macOS for some reason:
  # https://github.com/ubc-carnap-team/Carnap/runs/1074093219?check_suite_focus=true
  tz = dontCheck oldpkgs.tz;

  # this is required over 0.12.6.3 that's in the repos as of 2021-02-26, in
  # order to allow newer pandoc.
  # it is jailbroken to allow persistent 2.11
  yesod-markdown = doJailbreak (oldpkgs.callHackageDirect {
    pkg = "yesod-markdown";
    ver = "0.12.6.4";
    sha256 = "1bz9mwh71z4djbagk9m9643s939k5iqdda0wsdk20h4zzq8wdy2j";
  } {});

  # they wrote a spec that calls out to Google. It does not work in a nix
  # builder.
  oidc-client = dontCheck oldpkgs.oidc-client;

  # lti13 and yesod-auth-lti13 are not in nixpkgs yet
  # lti13 = oldpkgs.callCabal2nix "lti13" ../lti13/lti13 { };
  # yesod-auth-lti13 = oldpkgs.callCabal2nix "yesod-auth-lti13" ../lti13/yesod-auth-lti13 { };
  lti13 = oldpkgs.callHackageDirect {
    pkg = "lti13";
    ver = "0.2.0.1";
    sha256 = "17wkxajyr536n2rqr8w739pqip717m6vqn4a61whxqr5ia7lkiy6";
  } { };
  yesod-auth-lti13 = oldpkgs.callHackageDirect {
    pkg = "yesod-auth-lti13";
    ver = "0.2.0.1";
    sha256 = "0sq316ys04gqvcymrlj1f158qfrf3mpn60rcw1h8rm92x9g4ca5p";
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
        cp config/settings-example.yml config/settings.yml
        cp -r {config,static} $out/share
        cat config/settings.yml
        '';

      enableExecutableProfiling = profiling;
      enableLibraryProfiling = profiling;
      buildDepends = [ book client ];

      isExecutable = true;
      # Carnap-Server has no tests/they are broken
      doCheck = false;
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
