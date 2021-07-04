export EDITOR=nvim
export VISUAL=nvim
export LC_CTYPE=en_US.UTF-8
export LC_ALL=en_US.UTF-8

# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH="/Users/almagest/.oh-my-zsh"
plugins=(git zsh-autosuggestions zsh-syntax-highlighting)

source $ZSH/oh-my-zsh.sh

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
export GOROOT=/usr/local/go
export GOPATH=~/.go

export PATH="$PATH:/usr/local/texlive/2019/bin/x86_64-linux"
export PATH="$PATH:/usr/local/texlive/2020/bin/x86_64-darwin"
export PATH="/Users/linuxbrew/.linuxbrew/bin:$PATH"
export PATH="/Users/almagest/.local/bin:$PATH"
export PATH="/Users/almagest/.cargo/bin:$PATH"
export PATH="/Users/almagest/.ghcup/bin:$PATH"
export PATH="/Users/almagest/.cabal/bin:$PATH"
export PATH="/Users/almagest/.nimble/bin:$PATH"
export PATH="/Users/almagest/.flutter/bin:$PATH"
export PATH="/Users/almagest/.npm-global/bin:$PATH"
export PATH="/usr/local/opt/qt/bin:$PATH"
export PATH="/usr/local/opt/llvm/bin:$PATH"
export PATH=$PATH:$GOROOT/bin
export PATH=$PATH:$GOPATH/bin
export VISUAL=nvim
export EDITOR="$VISUAL"
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$HOME/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$HOME/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/lib/

export LDFLAGS="-L/usr/local/opt/qt/lib:$LDFLAGS"
export LDFLAGS="-L/usr/local/opt/llvm/lib:$LDFLAGS"
export CPPFLAGS="-I/usr/local/opt/qt/include:$CPPFLAGS"
export CPPFLAGS="-I/usr/local/opt/llvm/include:$CPPFLAGS"
export PKG_CONFIG_PATH="/usr/local/opt/qt/lib/pkgconfig"

# Define aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"
alias c='clear'
alias v='nvim'
alias e='emacs -nw'
alias t='tmux'
alias hexdump='hexyl'
alias doom='~/.emacs.d/bin/doom'
alias ls='exa'
# alias vols='amixer -D pulse sset Master'
alias getmusic='youtube-dl -x --audio-format m4a'
alias hakwon='cd ~/Library/Mobile\ Documents/com\~apple\~CloudDocs/TeX_Documents/Hakwon'
# alias gsync='cd ~/GDrive && grive'
# alias dynamix='google-chrome-stable --disable-web-security --allow-file-access-from-files --allow-file-access --allow-cross-origin-auth-prompt ~/GDrive/Dynamix/index.html'
# alias plpdfs="wget -A pdf -m -p -E -k -K -np http://prl.korea.ac.kr/\~pronto/home/courses/cose212/2019/ && cp -r ~/prl.korea.ac.kr/~pronto/home/courses/cose212/2019 ~/GDrive/Haskell/PL/ && rm -r ~/prl.korea.ac.kr"
# alias checkclass="xprop | grep WM_CLASS"
# alias conph="kdeconnect-cli -n 'Samsung SM-A810S'"
# alias kakaotalk="wine ~/.wine/drive_c/Program\ Files/Kakao/KakaoTalk/KakaoTalk.exe"
# alias mahjong="wine ~/.wine/drive_c/Program\ Files/SEGA/MJ_Mahjong/MJ_Launcher.exe"
# alias installstack="curl -sSL https://get.haskellstack.org/ | sh"
alias autohaskell="stack exec ghcid"
alias gitall="git add --all && git commit -m"
# alias emptytrash="rm -rf ~/.local/share/Trash && rm -rf ~/.local/share/vifm/Trash"

# Load Configures
# alias qutecfg="vim ~/.config/qutebrowser/config.py"
# alias xmonadcfg="vim ~/.xmonad/xmonad.hs"
# alias polybarcfg="vim ~/.config/polybar/config"
alias zshcfg="vim ~/.zshrc"
alias vimcfg='vim ~/.vimrc'
# alias urxvtreset="xrdb ~/.Xresources"

eval $(thefuck --alias)
eval "$(starship init zsh)"

bindkey -v
[ -f "/home/almagest//.ghcup/env" ] && source "/home/almagest//.ghcup/env" # ghcup-env

