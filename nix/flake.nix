{
  description = "User Environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs }: {
    packages.x86_64-linux =
      let
        pkgs = import nixpkgs {
          system = "x86_64-linux";
          config = {
            allowUnfree = true;
          };
        };

        pythonEnv = pkgs.python312.withPackages (ps: with ps; [
          pip
          requests
          numpy
          pandas
          matplotlib
          epc
          watchdog
          setuptools
          pulsectl
          psycopg2
          openai
          python-dotenv
          ytmusicapi
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
          paths = [ pkgs.go pkgs.gopls pkgs.godef pkgs.gotools ];
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

        terraformEnv = pkgs.buildEnv {
          name = "terraform-env";
          paths = with pkgs; [
            terraform
            terraform-ls
          ];
        };

        devToolsEnv = pkgs.buildEnv {
          name = "devtools-env";
          paths = with pkgs; [
            postgresql
            mariadb
            htop
            tmux
          ];
        };

      in {
        python = pythonEnv;
        node = nodeEnv;
        go = goEnv;
        rust = rustEnv;
        solidity = solidityEnv;
        elixir = elixirEnv;
        terraform = terraformEnv;

        default = pkgs.buildEnv {
          name = "user-env";
          paths = [ pkgs.nix pythonEnv nodeEnv goEnv rustEnv solidityEnv elixirEnv terraformEnv devToolsEnv ];
        };
      };
  };
}
