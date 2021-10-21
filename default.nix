let
  #hostNix = import <nixpkgs> {};
  pkgs = import <nixpkgs> {};
  compiler = "ghc8104";

  #easy-hls-src = hostNix.pkgs.fetchFromGitHub {
  #  owner = "jkachmar";
  #  repo = "easy-hls-nix";
  #  rev = "db85cac9d0405b4769b75cba0b004aed3beaf2de";
  #  sha256 = "10nff6mqflrd6dz1fp2l9vmfwbgk0r7zm81qh2xnjj19a47pd7v3";
  #};
  #easy-hls = hostNix.callPackage easy-hls-src { };
  
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
    # allowBroken = true;
  };
  #nixpkgsPin = hostNix.pkgs.lib.importJSON ./nixos-pinned.json;

  #src = hostNix.pkgs.fetchFromGitHub {
  #  owner = "NixOS";
  #  repo  = "nixpkgs-channels";
  #  inherit (nixpkgsPin) rev sha256;
  #};

  #pkgs = import src { inherit config; };
  drv = pkgs.haskell.packages."${compiler}".callCabal2nix "my-palantype" ./. { };
in
  {
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
            haskell-language-server
          ];
      });
    lib = drv;
  }
