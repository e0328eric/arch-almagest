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
set PATH $PATH ~/.local/bin
set PATH $PATH ~/.cabal/bin
set PATH $PATH ~/.ghcup/bin
set PATH $PATH ~/.ghcup/env
set PATH $PATH ~/.cargo/bin
set --universal -x GOPATH ~/.go
set PATH $PATH $GOROOT/bin:$GOPATH/bin
set GOLSPPATH $GOPATH/src/github.com/ajaymt/golsp

function emptytrash
    rm -rf /home/almagest/.local/share/Trash
    rm -rf /home/almagest/.local/share/vifm/Trash
end

function v
    nvim $argv
end

function doom
  ~/.emacs.d/bin/doom $argv
end

function ls
    lsd $argv
end

function vimcfg
    nvim ~/.config/nvim/init.vim
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

starship init fish | source

thefuck --alias | source
