{
  description = "User Environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs }: {
    packages.x86_64-linux =
      let
        pkgs = nixpkgs.legacyPackages.x86_64-linux;

        pythonEnv = pkgs.python312.withPackages (ps: with ps; [
          pip
          requests
          numpy
          pandas
          matplotlib
          epc
          watchdog
          setuptools
        ]);

        nodeEnv = pkgs.buildEnv {
          name = "node-env";
          paths = with pkgs; [
            nodejs_24
            yarn
          ] ++ (with pkgs.nodePackages; [
            svelte-language-server
            typescript
            typescript-language-server
          ]);
        };

        goEnv = pkgs.buildEnv {
          name = "go-env";
          paths = [ pkgs.go pkgs.gopls ];
        };

        rustEnv = pkgs.buildEnv {
          name = "rust-env";
          paths = with pkgs; [
            rustup
          ];
        };

        solidityEnv = pkgs.buildEnv {
          name = "solidity-env";
          paths = with pkgs; [
            solc
          ];
        };

        elixirEnv = pkgs.buildEnv {
          name = "elixir-env";
          paths = with pkgs; [
            elixir
            erlang
          ];
        };

      in {
        python = pythonEnv;
        node = nodeEnv;
        go = goEnv;
        rust = rustEnv;
        solidity = solidityEnv;
        elixir = elixirEnv;

        default = pkgs.buildEnv {
          name = "user-env";
          paths = [ pkgs.nix pythonEnv nodeEnv goEnv rustEnv solidityEnv elixirEnv ];
        };
      };
  };
}
