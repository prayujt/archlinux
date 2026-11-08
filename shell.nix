{ config, ... }:

let
  home = config.home.homeDirectory;
  denoInit = ''
    if [ -r "$HOME/.deno/env" ]; then
      . "$HOME/.deno/env"
    fi
  '';
in
{
  home.sessionVariables = {
    GOPATH = "${home}/.go";
    GOBIN = "${home}/.go/bin";
    HYPRSHOT_DIR = "${home}/Pictures/screenshots";
    EDITOR = "nvim";
    VISUAL = "nvim";
    JAVA_HOME = "/usr/lib/jvm/default";
    TERMINAL = "alacritty";
    DOCKER_BUILDKIT = "1";
    AUDIO_SCRAPER_HOST = "https://audio-scraper.prayujt.com";
  };

  programs.zsh = {
    enable = true;
    # Completion is initialized with Zsh's built-ins in interactive.zsh.
    enableCompletion = false;
    autocd = true;
    defaultKeymap = "emacs";
    autosuggestion = {
      enable = true;
      strategy = [ "history" "completion" ];
      highlight = "fg=239";
    };
    shellAliases = {
      c = "codex --yolo";
      claude = "claude --dangerously-skip-permissions";
      vim = "nvim";
      ls = "lsd";
      graph = "git log --all --decorate --oneline --graph";
      copy = "wl-copy";
      ws = "websocat";
      nd = "nix develop -c zsh -l";
      scrape = "audio-scrape";
      planewise = "ssh ubuntu@api.planewise.app -i ~/.tokens/planewise-ec2.pem";
      edugator = "ssh ubuntu@metrics.edugator.app -i ~/.tokens/metrics-keypair.pem";
      bwlogin = "bw login --apikey";
    };
    history = {
      size = 50000;
      save = 10000;
    };
    initContent = builtins.readFile ./files/zsh/interactive.zsh;
  };

  programs.bash = {
    enable = true;
    shellAliases = {
      ls = "ls --color=auto";
      grep = "grep --color=auto";
    };
    profileExtra = denoInit;
    initExtra = denoInit + ''
      PS1='[\u@\h \W]\$ '
    '';
  };
}
