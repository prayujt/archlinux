{ pkgs, ... }:

{
  home.packages = with pkgs; [
    nix

    (python312.withPackages (ps: with ps; [
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
      soco
    ]))

    nodejs_24
    yarn
    nodePackages.svelte-language-server
    nodePackages.typescript
    nodePackages.typescript-language-server

    go
    gopls
    godef
    gotools
    rustup
    solc
    elixir
    erlang
    terraform
    terraform-ls
    postgresql
    mariadb
    htop
    tmux
  ];
}
