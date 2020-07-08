set fish_color_normal normal
set fish_color_command 99cc99
set fish_color_quote ffcc66
set fish_color_end cc99cc
set fish_color_redirection d3d0c8
set fish_color_error f2777a
set fish_color_param d3d0c8
set fish_color_selection white --bold --background=brblack
set fish_color_search_match bryellow --background=brblack
set fish_color_history_current --bold
set fish_color_operator 6699cc
set fish_color_escape 66cccc
set fish_color_cwd green
set fish_color_cwd_root red
set fish_color_valid_path --underline
set fish_color_autosuggestion 747369
set fish_color_user brgreen
set fish_color_host normal
set fish_color_cancel -r
set fish_pager_color_completion normal
set fish_pager_color_description B3A06D yellow
set fish_pager_color_prefix white --bold --underline
set fish_pager_color_progress brwhite --background=cyan
set fish_color_comment ffcc66
set fish_color_match 1EB176
set pure_color_info cyan
set pure_color_success 268BD2
set pure_color_primary 6699CC
set -gx PATH $PATH ~/.local/bin
set -gx PATH $PATH ~/.cabal/bin
set -gx PATH $PATH ~/.ghcup/bin
set -gx PATH $PATH ~/.ghcup/env

function vim
    nvim $argv
end

function ls
    lsd $argv
end

function xmonadcfg
    nvim ~/.xmonad/xmonad.hs
end

function vimcfg
    nvim ~/.config/nvim/init.vim
end

function xmonadcfg
    nvim ~/.xmonad/xmonad.hs
end

function dwmcfg
    nvim ~/.dwm/config.h
end

function dwmrest
    cd ~/.dwm
    sudo make clean install
    cd ~/
end

function stcfg
    nvim ~/GitHub/backup/utils/st/config.h
end

function alacfg
    nvim ~/GitHub/backup/utils/alacritty/alacritty.yml
end

function strest
    cd ~/GitHub/backup/utils/st
    sudo make clean install
    cd ~/
end

function fishcfg
    nvim ~/.config/fish/config.fish
end

function getmusic
    youtube-dl -x --audio-format m4a $argv
end

function gitall
    git add . && git commit -m $argv && git push origin master
end

function conph
    kdeconnect-cli -n 'Samsung SM-A810S' --share $argv
end

function installstack
    curl -sSL https://get.haskellstack.org/ | sh
end

function checkclass
    xprop | grep WM_CLASS
end

thefuck --alias | source
