{ config, lib, ... }:

let
  sourceRoot = lib.cleanSourceWith {
    src = ./files;
    filter = path: type: baseNameOf path != "__pycache__";
  };
  relativePath = file: builtins.unsafeDiscardStringContext
    (lib.removePrefix "${toString sourceRoot}/" (toString file));
  files = builtins.filter
    (file: lib.hasPrefix "." (relativePath file))
    (lib.filesystem.listFilesRecursive sourceRoot);
  # Desktop tools edit these files in place. Keep their canonical sources
  # writable, while Home Manager owns the links at their original locations.
  writable = path: builtins.any (prefix: lib.hasPrefix prefix path) [
    ".config/alacritty/"
    ".config/hypr/"
    ".config/quickshell/"
    ".local/share/desktop-styles/"
  ];
in
{
  home.file = builtins.listToAttrs (map (file:
    let path = relativePath file;
    in lib.nameValuePair path {
      source = if writable path then
        config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/nix/files/${path}"
      else file;
    }
  ) files) // {
    ".icons/default/cursors".source =
      config.lib.file.mkOutOfStoreSymlink "/usr/share/icons/Breeze_Default/cursors";
  };
}
