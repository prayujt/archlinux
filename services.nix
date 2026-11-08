{ pkgs, ... }:

let
  unitFile = name:
    let unit = pkgs.writeTextDir name (builtins.readFile (./files/systemd + "/${name}"));
    in "${unit}/${name}";
in

{
  # These units are currently disabled. Installing their definitions as files
  # preserves that state and their existing manual enable/disable behavior.
  home.file = {
    ".config/systemd/user/switch-audio.service".source =
      unitFile "switch-audio.service";
    ".config/systemd/user/switch-audio.timer".source =
      unitFile "switch-audio.timer";
  };

  systemd.user.startServices = false;
}
