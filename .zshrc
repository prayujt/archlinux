export ZSH="/home/prayuj/.config/oh-my-zsh"
ZSH_THEME="spaceship"
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=239'

source $ZSH/oh-my-zsh.sh

alias vim=nvim
alias mysql=mariadb
alias ls=lsd
alias graph='git log --all --decorate --oneline --graph'
alias copy='wl-copy'
alias volume='~/.scripts/pulseaudio | wob -c ~/.config/wob/wob.ini &; disown'
alias ws='websocat'
alias codex='codex --yolo'
alias c='codex'
alias nd='nix develop -c zsh -l'
alias claude='claude --dangerously-skip-permissions'
alias scrape='audio-scrape'

alias editenv='nvim ~/nix/flake.nix'
alias updateenv='nix profile upgrade --profile ~/.nix-profiles/user-env nix'
alias cleanup='nix store gc && nix-collect-garbage -d'

alias planewise='ssh ubuntu@api.planewise.app -i ~/.tokens/planewise-ec2.pem'
planewisedb() {
    PGPASSWORD=$(pass planewise/rds) psql -h planewise-psql.cjeqfhagckjg.us-east-1.rds.amazonaws.com -U planewise sslmode=require
}

alias edugator='ssh ubuntu@metrics.edugator.app -i ~/.tokens/metrics-keypair.pem'
edugatordb() {
    psql -h edugator-db.cznhwcrim7oz.us-east-1.rds.amazonaws.com -U edugator sslmode=require
}

open() {
  xdg-open "$1" &
  disown
}

export GOPATH=/home/prayuj/.go
export GOBIN=$GOPATH/bin
export HYPRSHOT_DIR=/home/prayuj/Pictures/screenshots
export EDITOR=nvim
export VISUAL=nvim
export JAVA_HOME=/usr/lib/jvm/default
export TERMINAL="alacritty"
export DOCKER_BUILDKIT=1
export AUDIO_SCRAPER_HOST=https://audio-scraper.prayujt.com

typeset -U path PATH
path+=(
  "$HOME/.local/bin"
  /opt/cuda/bin
  "$HOME/.scripts"
  "$GOBIN"
  "$HOME/.cargo/bin"
  /usr/local/sbin
  /usr/bin/site_perl
  /usr/bin/vendor_perl
  /usr/bin/core_perl
)

bindkey '\t' forward-word
bindkey '^[[Z' backward-kill-word
bindkey '^ ' expand-or-complete

export ZSH_AUTOSUGGEST_STRATEGY=(
  history
  completion
)

# Load secrets
source ~/.secrets

# Load zsh plugins
source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.plugin.zsh
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

if [ -e /home/prayuj/.nix-profile/etc/profile.d/nix.sh ]; then . /home/prayuj/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer

if [[ -n "$IN_NIX_SHELL" && -n "$VIRTUAL_ENV" && -r "$VIRTUAL_ENV/bin/activate" ]]; then
  source "$VIRTUAL_ENV/bin/activate"
fi

# Map existing credentials to Bitwarden CLI variable names.
export BW_CLIENTID="${BW_CLIENT_ID}"
export BW_CLIENTSECRET="${BW_CLIENT_SECRET}"

# Bitwarden CLI helpers
alias bwlogin='bw login --apikey'

_bw_session() (
  setopt noxtrace
  umask 077
  local bw_dir="$HOME/.config/bitwarden-cli"
  local bw_session="${BW_SESSION-}" bw_lock_fd bw_tmp
  mkdir -p "$bw_dir" || return
  chmod 700 "$bw_dir" || return
  exec {bw_lock_fd}>"$bw_dir/session.lock" || return
  command flock "$bw_lock_fd" || return

  [[ -s "$bw_dir/session" ]] && bw_session="$(<"$bw_dir/session")"
  if [[ -z "$bw_session" ]] || ! BW_SESSION="$bw_session" command bw unlock --check --nointeraction >/dev/null 2>&1; then
    command bw login --check --nointeraction >/dev/null 2>&1 || return
    if [[ -f "$bw_dir/master-password" ]]; then
      bw_session="$(command bw unlock --passwordfile "$bw_dir/master-password" --raw --nointeraction)" || return
    elif [[ "$1" == auto ]]; then
      return 1
    else
      bw_session="$(command bw unlock --raw)" || return
    fi
  fi

  bw_tmp="$(command mktemp "$bw_dir/session.XXXXXX")" || return
  print -r -- "$bw_session" > "$bw_tmp"
  command mv -f "$bw_tmp" "$bw_dir/session" || return
  print -r -- "$bw_session"
)

bwunlock() {
  setopt localoptions noxtrace
  local bw_session
  bw_session="$(_bw_session)" || return
  export BW_SESSION="$bw_session"
}

_bw_unlock_ready() {
  setopt localoptions noxtrace
  local bw_fd="$1" bw_session
  if IFS= read -r -u "$bw_fd" bw_session && [[ -n "$bw_session" ]]; then
    export BW_SESSION="$bw_session"
  fi
  zle -F "$bw_fd"
  exec {bw_fd}<&-
  unset _bw_unlock_fd
}

_bw_unlock_start() {
  setopt localoptions noxtrace
  [[ -o interactive && -t 0 ]] || return
  [[ -n "${_bw_unlock_fd-}" ]] && return
  exec {_bw_unlock_fd}< <(_bw_session auto </dev/null 2>/dev/null)
  zle -F "$_bw_unlock_fd" _bw_unlock_ready
}

_bw_unlock_start
