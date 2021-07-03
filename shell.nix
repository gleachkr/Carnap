args@{ghcjs ? false, hls ? !ghcjs, useClientFromCi ? false, ...}:
let
  importArgs = removeAttrs (args // { inherit hls useClientFromCi; }) ["ghcjs"];
  def = import ./default.nix importArgs;
  inherit (def) ghcShell ghcjsShell;
in
if ghcjs then ghcjsShell else ghcShell
