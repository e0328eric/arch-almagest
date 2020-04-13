set -gx PATH $PATH ~/.local/bin
set -gx PATH $PATH ~/.cabal/bin
set -gx PATH $PATH ~/.ghcup/bin

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

function fishcfg
    vim ~/.config/fish/config.fish
end

function getmusic
    youtube-dl -x --audio-format m4a
end
