export ZSH="/home/prayuj/.config/oh-my-zsh"
ZSH_THEME="jonathan"
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=239'

zstyle ':completion:*' '' matcher-list 'm:{a-z}={A-Z}'
zstyle ':omz:plugins:nvm' lazy yes

plugins=(git dotenv nvm)

source $ZSH/oh-my-zsh.sh

alias vim=nvim
alias mysql=mariadb
alias path=realpath
alias ls=lsd
alias graph='git log --all --decorate --oneline --graph'
alias copy='wl-copy'
alias volume='~/.scripts/pulseaudio | wob -c ~/.config/wob/wob.ini &; disown'
alias wallpaper='killall hyprpaper; hyprpaper &; disown'
alias record_screen='wf-recorder --muxer=v4l2 --codec=rawvideo --file=/dev/video4 -x yuv420p'
alias ws='wscat --connect'
alias waybar_restart='killall -SIGUSR2 waybar'
alias c='z'
alias activate='source ~/.miniconda3/bin/activate ml4t'

planewise() {
    PGPASSWORD=$(pass planewise/rds) psql -h planewise-psql.cjeqfhagckjg.us-east-1.rds.amazonaws.com -U planewise sslmode=require
}

open() {
  xdg-open "$1" &
  disown
}
sizeof() {
  du -h --max-depth=1 "$1"
}
cd() {
  builtin cd "$@" 2>/dev/null && return
  emulate -L zsh
  setopt local_options extended_glob
  local matches
  matches=( (#i)${(P)#}(N/) )
  case $#matches in
    0) 
      if ((#cdpath)) &&
         [[ ${(P)#} != (|.|..)/* ]] &&
         matches=( $^cdpath/(#i)${(P)#}(N/) ) &&
         ((#matches==1))
      then
        builtin cd $@[1,-2] $matches[1]
        return
      fi
      # Still nothing. Let cd display the error message.
      echo "cd: no such file or directory: $@";;
      # builtin cd "$@";;
    1)
      builtin cd $@[1,-2] $matches[1];;
    *)
      print -lr -- "Ambiguous case-insensitive directory match:" $matches >&2
      return 3;;
  esac
}

export GOPATH=/home/prayuj/.go
export GOBIN=$GOPATH/bin
export HYPRSHOT_DIR=/home/prayuj/Pictures/screenshots
export EDITOR=nvim
export VISUAL=nvim
export TERM=xterm-256color
export JAVA_HOME=/usr/lib/jvm/default
export TERMINAL="alacritty"
export LD_LIBRARY_PATH=/opt/cuda/lib
export DOCKER_BUILDKIT=1
source ~/.keys

export PATH=$PATH:/home/prayuj/.local/bin
export PATH=$PATH:/opt/cuda
export PATH=$PATH:/opt/cuda/bin
export PATH=$PATH:/home/prayuj/.emacs.d/bin
export PATH=$PATH:/home/prayuj/.scripts
export PATH=$PATH:/home/prayuj/.scripts/bin
export PATH=$PATH:/home/prayuj/.miner
export PATH=$PATH:/home/prayuj/.thinkorswim
export PATH=$PATH:$GOPATH/bin
export PATH=$PATH:/home/prayuj/.cargo/bin
export PATH=$PATH:/home/prayuj/.config/emacs/bin
export PATH=$PATH:/usr/local/sbin
export PATH=$PATH:/home/prayuj/.config/nvm/versions/node/v22.2.0/bin
export PATH=$PATH:/usr/bin/site_perl:/usr/bin/vendor_perl:/usr/bin/core_perl

export PKG_CONFIG_PATH=/usr/local/lib/pkgconfig

#bindkey '^\t' autosuggest-accept
bindkey '\t' forward-word
bindkey '^[[Z' backward-kill-word
bindkey '^ ' expand-or-complete
#bindkey '^?' autosuggest-clear

export ZSH_AUTOSUGGEST_STRATEGY=(
  history
  completion
)

# Load zsh plugins
source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.plugin.zsh
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# pnpm
export PNPM_HOME="/home/prayuj/.local/share/pnpm"
case ":$PATH:" in
  *":$PNPM_HOME:"*) ;;
  *) export PATH="$PNPM_HOME:$PATH" ;;
esac

eval "$(zoxide init zsh)"
# pnpm end

export NVM_DIR="$HOME/.config/nvm"
#[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
#[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
