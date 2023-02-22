let
  #hostNix = import <nixpkgs> {};
  compiler = "ghc8107";

  config = {
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${compiler}" = pkgs.haskell.packages."${compiler}".override {
            overrides = self: super: {
            };
          };
        };
      };
    };
    allowBroken = true;
  };
  #nixpkgsPin = hostNix.pkgs.lib.importJSON ./nixos-pinned.json;

  pkgs = import <nixpkgs> { inherit config; };

  easy-hls = pkgs.callPackage (pkgs.fetchFromGitHub {
    owner = "jkachmar";
    repo = "easy-hls-nix";
    rev = "ecb85ab6ba0aab0531fff32786dfc51feea19370";                                    
    sha256 = "14v0jx8ik40vpkcq1af1b3377rhkh95f4v2cl83bbzpna9aq6hn2";
  }) {
    ghcVersions = [ "8.10.7" ];
  };

  drv = pkgs.haskell.packages."${compiler}".callCabal2nix "my-palantype" ./. { };
  env =
    # don't know why, but the haskell-language doesn't seem to
    # be a build tool, but a native build input
    #
    # with pkgs.haskell.lib;
    # addBuildTools drv (
    #   with pkgs.haskellPackages;
    #   [ haskell-language-server ]
    # );
    with pkgs.haskellPackages;
    drv.env.overrideAttrs ( oldAttrs: rec {
      nativeBuildInputs =
        oldAttrs.nativeBuildInputs ++ [
          cabal-install
          easy-hls
        ];
    });
in
  if pkgs.lib.inNixShell then env else drv
