{
  description = "shell with haskell for fpl";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };

  outputs =
    inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "x86_64-linux"
        "aarch64-linux"
      ];
      imports = [
        inputs.haskell-flake.flakeModule
      ];
      perSystem =
        {
          self',
          system,
          lib,
          config,
          pkgs,
          ...
        }:
        {
          haskellProjects.default = {
            basePackages = pkgs.haskellPackages;

            # Packages to add on top of `basePackages`, e.g. from Hackage
            packages = {
              # aeson.source = "1.5.0.0"; # Hackage version
            };

            # my-haskell-package development shell configuration
            devShell = {
              hlsCheck.enable = false;
            };

            # What should haskell-flake add to flake outputs?
            autoWire = [
              "packages"
              "apps"
              "checks"
            ]; # Wire all but the devShell
          };

          devShells.default = pkgs.mkShell {
            name = "ipl shell";
            inputsFrom = [
              config.haskellProjects.default.outputs.devShell
            ];
            nativeBuildInputs = with pkgs; [
              # other development tools.
              haskell-language-server
              haskellPackages.fmt
            ];
          };

          packages.default = self'.packages.secd;
        };
    };
}
