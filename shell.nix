{ pure ? false }:
let
  packages = import ./.;
  inherit (packages) pkgs goada-plutus;
  inherit (goada-plutus) haskell;

in
  haskell.project.shellFor {
    withHoogle = false;

    nativeBuildInputs = with goada-plutus; [
      hlint
      cabal-install
      haskell-language-server
      stylish-haskell
      pkgs.niv
      cardano-repo-tool
      pkgs.ghcid
      # HACK: This shouldn't need to be here.
      pkgs.lzma.dev
    ] ++ (pkgs.lib.optionals pure [
      pkgs.git
      pkgs.cacert
      pkgs.curl
      pkgs.jq
    ]);
  }
