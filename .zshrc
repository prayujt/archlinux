export ZSH="/home/prayuj/.config/oh-my-zsh"
ZSH_THEME="intheloop"

zstyle ':completion:*' '' matcher-list 'm:{a-z}={A-Z}'

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion.
# Case-sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to automatically update without prompting.
# DISABLE_UPDATE_PROMPT="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line if pasting URLs and other text is messed up.
# DISABLE_MAGIC_FUNCTIONS="true"

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# Caution: this setting can cause issues with multiline prompts (zsh 5.7.1 and newer seem to work)
# See https://github.com/ohmyzsh/ohmyzsh/issues/5765
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# You can set one of the optional three formats:
# "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# or set a custom format using the strftime function format specifications,
# see 'man strftime' for details.
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load?
# Standard plugins can be found in $ZSH/plugins/
# Custom plugins may be added to $ZSH_CUSTOM/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git dotenv)

source $ZSH/oh-my-zsh.sh

alias vim='nvim'
alias ls='lsd'
alias graph='git log --all --decorate --oneline --graph'
alias copy='xclip -selection clipboard'
alias wallpaper='nitrogen ~/Pictures/wallpapers'
alias usb='cd /run/media/prayuj'
alias wifiscan='nmcli device wifi list'
alias locate='sudo updatedb && locate'
alias flood='sudo hping3 -c 10000 -d 128 -S -w 64 -p 8000 --flood --rand-source 192.168.0.1'
alias server='ssh prayuj@prayujt.com -p 1024'
alias files='ssh files@prayujt.com -p 1024'
alias wifioff=nmcli radio wifi off
alias wifion=nmcli radio wifi on
alias wifistatus=nmcli device

open() {
	xdg-open "$1" &
	disown
}

check() {
	nmap -sT -p- "$1"
}

download() {
	scp -P 1024 prayuj@prayujt.com:~/"$1" "$2" 
}

downloadf() {
	scp -r -P 1024 prayuj@prayujt.com:~/"$1" "$2" 
}

downloadfiles() {
	scp -P 1024 files@prayujt.com:~/"$1" "$2" 
}

downloadfilesf() {
	scp -r -P 1024 prayuj@prayujt.com:~/"$1" "$2" 
}

upload() {
	scp -P 1024 "$1" prayuj@prayujt.com:~/"$2"
}

uploadf() {
	scp -P 1024 -r "$1" prayuj@prayujt.com:~/"$2"
}

uploadm() {
    scp -P 1024 "$1" files@prayujt.com:~/Music/"$2"
}

drop() {
	scp -P 1024 "$1" prayuj@prayujt.com:~/Dropbox/"$2"
}

dropf() {
	scp -P 1024 -r "$1" prayuj@prayujt.com:~/Dropbox/"$2"
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

wificonnect() {
    nmcli device wifi connect "$1" password "$2"
}

send() {
	cp "$1" ~/sync/Kaniel/
}

sendf() {
	cp -r "$1" ~/sync/Kaniel/
}

export PATH=$PATH:/home/prayuj/.local/bin
export PATH=$PATH:/opt/cuda
export PATH=$PATH:/opt/cuda/bin
export PATH=$PATH:/usr/lib/jvm/java-11-openjdk
export PATH=$PATH:/home/prayuj/.emacs.d/bin
export PATH=$PATH:/home/prayuj/.scripts
export PATH=$PATH:/home/prayuj/.scripts/bin
export PATH=$PATH:/home/prayuj/.miner
export EDITOR=nvim
export VISUAL=nvim
export TERM=xterm-256color
export JAVA_HOME=/usr/lib/jvm/java-11-openjdk
export TERMINAL="alacritty"
export LD_LIBRARY_PATH=/usr/local/lib
source ~/.keys

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
