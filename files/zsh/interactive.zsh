autoload -Uz compinit vcs_info add-zsh-hook
completion_cache="${XDG_CACHE_HOME:-$HOME/.cache}/zsh"
mkdir -p "$completion_cache"
compinit -d "$completion_cache/zcompdump-$ZSH_VERSION"
unset completion_cache

zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:git:*' formats ' %b'
add-zsh-hook precmd vcs_info
setopt PROMPT_SUBST INTERACTIVE_COMMENTS
PROMPT='%F{8}%*%f %F{cyan}%~%f%F{magenta}${vcs_info_msg_0_}%f${IN_NIX_SHELL:+ (nix)}
%(?.%F{green}.%F{red})>%f '

planewisedb() {
    PGPASSWORD=$(pass planewise/rds) psql -h planewise-psql.cjeqfhagckjg.us-east-1.rds.amazonaws.com -U planewise sslmode=require
}

edugatordb() {
    psql -h edugator-db.cznhwcrim7oz.us-east-1.rds.amazonaws.com -U edugator sslmode=require
}

open() {
  xdg-open "$1" &
  disown
}

path+=(
  "$HOME/.local/bin"
  "$HOME/.scripts"
  "$GOBIN"
  "$HOME/.cargo/bin"
)

bindkey '^I' expand-or-complete
bindkey '^[[Z' backward-kill-word
bindkey '^ ' expand-or-complete

# Keep credentials outside the Nix store.
source ~/.secrets

if [[ -n "$IN_NIX_SHELL" && -n "$VIRTUAL_ENV" && -r "$VIRTUAL_ENV/bin/activate" ]]; then
  source "$VIRTUAL_ENV/bin/activate"
fi

# Map existing credentials to Bitwarden CLI variable names.
export BW_CLIENTID="${BW_CLIENT_ID}"
export BW_CLIENTSECRET="${BW_CLIENT_SECRET}"

# Bitwarden CLI helpers
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
