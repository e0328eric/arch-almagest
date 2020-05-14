set -gx PATH $PATH ~/.local/bin
set -gx PATH $PATH ~/.cabal/bin
set -gx PATH $PATH ~/.ghcup/bin
set -gx PATH $PATH ~/.ghcup/env

function e
    emacs -nw
end

function gsync
    cd ~/GDrive
    grive
end

function xmonadcfg
    vim ~/.xmonad/xmonad.hs
end

function vimcfg
    vim ~/.vimrc
end

function xmonadcfg
    vim ~/.xmonad/xmonad.hs
end

function dwmcfg
    vim ~/.dwm/config.h
end

function i3cfg
    vim ~/.config/i3/config
end

function fishcfg
    vim ~/.config/fish/config.fish
end

function getmusic
    youtube-dl -x --audio-format m4a $argv
end

function conph
    kdeconnect-cli -n 'Samsung SM-A810S' --share $argv
end

thefuck --alias | source
