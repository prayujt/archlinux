{
  description = "User Environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs }: {
    packages.x86_64-linux =
      let
        pkgs = nixpkgs.legacyPackages.x86_64-linux;

        pythonEnv = pkgs.buildEnv {
          name = "python-env";
          paths = with pkgs.python312Packages; [
            pkgs.python312
            pip
            requests
            numpy
            pandas
            matplotlib
          ];
        };

        nodeEnv = pkgs.buildEnv {
          name = "node-env";
          paths = with pkgs; [
            nodejs_24
            yarn
          ];
        };

        goEnv = pkgs.buildEnv {
          name = "go-env";
          paths = [ pkgs.go ];
        };

        rustEnv = pkgs.buildEnv {
          name = "rust-env";
          paths = with pkgs; [
            rustup
          ];
        };

      in {
        python = pythonEnv;
        node = nodeEnv;
        go = goEnv;
        rust = rustEnv;

        default = pkgs.buildEnv {
          name = "user-env";
          paths = [ pkgs.nix pythonEnv nodeEnv goEnv rustEnv ];
        };
      };
  };
}
