" -----------------------------------------------------------------------------
" Color settings
" -----------------------------------------------------------------------------

colorscheme gruvbox
set t_Co=256

" For Gruvbox to look correct in terminal Vim you'll want to source a palette
" script that comes with the Gruvbox plugin.
"
" Add this to your ~/.profile file:
"   source "$HOME/.vim/plugged/gruvbox/gruvbox_256palette.sh"

" Gruvbox comes with both a dark and light theme.
set background=dark

" This needs to come last, otherwise the colors aren't correct.
syntax on

if (has('termguicolors'))
  set termguicolors
endif
