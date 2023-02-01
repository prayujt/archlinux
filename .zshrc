export ZSH="/home/prayuj/.config/oh-my-zsh"
ZSH_THEME="intheloop"

zstyle ':completion:*' '' matcher-list 'm:{a-z}={A-Z}'

plugins=(git dotenv)

source $ZSH/oh-my-zsh.sh

alias vim=nvim
alias path=realpath
alias ls=lsd
alias graph='git log --all --decorate --oneline --graph'
alias copy='xclip -selection clipboard'
alias wallpaper='nitrogen ~/Pictures/wallpapers'
alias usb='cd /run/media/prayuj'
alias wifiscan=nmcli device wifi list
alias locate='sudo updatedb && locate'
alias flood='sudo hping3 -c 10000 -d 128 -S -w 64 -p 8000 --flood --rand-source 192.168.0.1'
alias server='ssh prayuj@prayujt.com -p 1024'
alias rootserver='ssh root@prayujt.com -p 1024'
alias files='ssh files@prayujt.com -p 1024'
alias wifioff=nmcli radio wifi off
alias wifion=nmcli radio wifi on
alias wifistatus=nmcli device
alias news='cat ~/.config/polybar/scripts/news/current_news.txt | sed -r "s/[\[]+/\n\[/g"'

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
uploadfiles() {
  scp -P 1024 "$1" files@prayujt.com:~/"$2"
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
replace() {
  # $1: spotify song URL
  # $2: youtube song URL
  ssh files@prayujt.com -p 1024 "python3 /home/files/.scripts/music/replace.py $1 $2 &; disown"
}
download_playlist() {
  # $1: spotify playlist URL
  # $2: ampache playlist name
  ssh files@prayujt.com -p 1024 "python3 /home/files/.scripts/music/playlist.py $1 $2"
}
update() {
  ssh files@prayujt.com -p 1024 "python3 /home/files/.scripts/music/cronjob.py&"&
}

export GOPATH=/home/prayuj/.go
export GOBIN=$GOPATH/bin
export XDG_CONFIG_HOME=/home/prayuj/.config
export EDITOR=nvim
export VISUAL=nvim
export TERM=xterm-256color
export JAVA_HOME=/usr/lib/jvm/java-11-openjdk
export TERMINAL="alacritty"
export LD_LIBRARY_PATH=/usr/local/lib
export NVM_DIR="$HOME/.nvm"
source ~/.keys

export PATH=$PATH:/home/prayuj/.local/bin
export PATH=$PATH:/opt/cuda
export PATH=$PATH:/opt/cuda/bin
export PATH=$PATH:/usr/lib/jvm/java-11-openjdk
export PATH=$PATH:/home/prayuj/.emacs.d/bin
export PATH=$PATH:/home/prayuj/.scripts
export PATH=$PATH:/home/prayuj/.scripts/bin
export PATH=$PATH:/home/prayuj/.miner
export PATH=$PATH:/home/prayuj/.thinkorswim
export PATH=$PATH:$GOPATH/bin

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

# neofetch
