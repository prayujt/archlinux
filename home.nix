{ config, pkgs, ... }:

{
  imports = [
    ./packages.nix
    ./shell.nix
    ./files.nix
    ./services.nix
  ];

  home = {
    username = "prayuj";
    homeDirectory = "/home/prayuj";
    # Home Manager compatibility defaults; this is not the package version.
    stateVersion = "25.05";
  };

  targets.genericLinux.enable = true;
  programs.home-manager.enable = true;

  nix = {
    enable = true;
    package = pkgs.nix;
    settings = {
      experimental-features = [ "nix-command" "flakes" ];
      max-jobs = "auto";
    };
  };

  home.file.".config/home-manager".source =
    config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/nix";
}
