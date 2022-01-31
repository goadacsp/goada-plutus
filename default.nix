########################################################################
# default.nix -- The top-level nix build file for goada-plutus.
#
# This file defines various attributes that are used for building and
# developing goada-plutus.
#
########################################################################

let
  # Here a some of the various attributes for the variable 'packages':
  #
  # { pkgs
  #   goada-plutus: {
  #     haskell: {
  #       project # The Haskell project created by haskell-nix.project
  #       packages # All the packages defined by our project, including dependencies
  #       projectPackages # Just the packages in the project
  #     }
  #     hlint
  #     cabal-install
  #     stylish-haskell
  #     haskell-language-server
  #   }
  # }
  packages = import ./nix;

  inherit (packages) pkgs goada-plutus;
  project = goada-plutus.haskell.project;
in
{
  inherit pkgs goada-plutus;

  inherit project;
}
