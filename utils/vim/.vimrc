" -----------------------------------------------------------------------------
" This config is targeted for Vim 8.0+ and expects you to have Plugin installed.
" -----------------------------------------------------------------------------

" -----------------------------------------------------------------------------
" Plugins
" -----------------------------------------------------------------------------

set nocompatible              " be iMproved, required
filetype off                  " required

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'VundleVim/Vundle.vim'

" Vim Theme
Plugin 'fneu/breezy'
Plugin 'gruvbox-community/gruvbox'

"Airline Theme
Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'

" Zoom in and out of a specific split pane (similar to tmux).
Plugin 'dhruvasagar/vim-zoom'

" Pass focus events from tmux to Vim (useful for autoread and linting tools).
Plugin 'tmux-plugins/vim-tmux-focus-events'

" Navigate and manipulate files in a tree view.
Plugin 'scrooloose/nerdtree'

" Run a diff on 2 directories.
Plugin 'will133/vim-dirdiff'

" Add spelling errors to the quickfix list (vim-ingo-library is a dependency).
"Plugin 'inkarkat/vim-ingo-library' | Plugin 'inkarkat/vim-SpellCheck'

" Modify * to also work with visual selections.
Plugin 'nelstrom/vim-visual-star-search'

" Automatically clear search highlights after you move your cursor.
Plugin 'haya14busa/is.vim'

" Better display unwanted whitespace.
Plugin 'ntpeters/vim-better-whitespace'

" Toggle comments in various ways.
Plugin 'tpope/vim-commentary'

" Surround text with quotes, parenthesis, brackets, and more.
Plugin 'tpope/vim-surround'

" Automatically set 'shiftwidth' + 'expandtab' (indention) based on file type.
Plugin 'tpope/vim-sleuth'

" A number of useful motions for the quickfix list, pasting and more.
Plugin 'tpope/vim-unimpaired'

" Drastically improve insert mode performance in files with folds.
Plugin 'Konfekt/FastFold'

" Show git file changes in the gutter.
Plugin 'mhinz/vim-signify'

" A git wrapper.
Plugin 'tpope/vim-fugitive'

" Dim paragraphs above and below the active paragraph.
Plugin 'junegunn/limelight.vim'

" Distraction free writing by removing UI elements and centering everything.
Plugin 'junegunn/goyo.vim'

" A bunch of useful language related snippets (ultisnips is the engine).
Plugin 'SirVer/ultisnips'

" Rust in vim
Plugin 'rust-lang/rust.vim'

Plugin 'godlygeek/tabular'
Plugin 'plasticboy/vim-markdown'

Plugin 'eagletmt/neco-ghc'
Plugin 'reedes/vim-thematic'

Plugin 'ervandew/supertab'

" Coloring Hex code
Plugin 'lilydjwg/colorizer'

" YouCompleteMe
Plugin 'ycm-core/YouCompleteMe'

" AutoPair
Plugin 'jiangmiao/auto-pairs'

" Languages and file types.
Plugin 'chrisbra/csv.vim'
Plugin 'ekalinin/dockerfile.vim'
Plugin 'elixir-editors/vim-elixir'
Plugin 'elzr/vim-json'
Plugin 'Glench/Vim-Jinja2-Syntax'
Plugin 'lifepillar/pgsql.vim'
Plugin 'othree/html5.vim'
Plugin 'PotatoesMaster/i3-vim-syntax'
Plugin 'tpope/vim-git'
Plugin 'tpope/vim-liquid'
Plugin 'tpope/vim-rails'
Plugin 'wgwoods/vim-systemd-syntax'
Plugin 'neovimhaskell/haskell-vim'

call vundle#end()
filetype plugin indent on

" -----------------------------------------------------------------------------
" Status line
" -----------------------------------------------------------------------------

" Heavily inspired by: https://github.com/junegunn/dotfiles/blob/master/vimrc
function! s:statusline_expr()
  let mod = "%{&modified ? '[+] ' : !&modifiable ? '[x] ' : ''}"
  let ro  = "%{&readonly ? '[RO] ' : ''}"
  let ft  = "%{len(&filetype) ? '['.&filetype.'] ' : ''}"
  let fug = "%{exists('g:loaded_fugitive') ? fugitive#statusline() : ''}"
  let sep = ' %= '
  let pos = ' %-12(%l : %c%V%) '
  let pct = ' %P'

  return '[%n] %f %<'.mod.ro.ft.fug.sep.pos.'%*'.pct
endfunction

let &statusline = s:statusline_expr()

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

" -----------------------------------------------------------------------------
" Basic Settings
"   Research any of these by running :help <setting>
" -----------------------------------------------------------------------------

let mapleader=" "
let maplocalleader=" "

set autoindent
set autoread
set backspace=indent,eol,start
set backupdir=/tmp//,.
set clipboard=unnamedplus
set complete-=i
set cursorline
set completeopt=menuone
set cryptmethod=blowfish2
set directory=/tmp//,.
set encoding=utf-8
set expandtab smarttab
set formatoptions=tcqrn1
set hidden
set hlsearch
set incsearch
set laststatus=2
set lazyredraw
set matchpairs+=<:> " Use % to jump between pairs
set nocompatible
set noerrorbells visualbell t_vb=
set noshiftround
set nospell
set nostartofline
set nu rnu
set regexpengine=1
set renderoptions=type:directx
set ruler
set scrolloff=3
set shiftwidth=4
set showcmd
set showmatch
set showmode
set smartcase
set softtabstop=4
set splitbelow
set tabstop=4
set textwidth=0
set term=terminator
set ttimeout
set ttyfast
set ttymouse=sgr
set undodir=/tmp//,.
set virtualedit=block
set whichwrap=b,s,<,>
set wildmenu
set wildmode=full
set wrap
:hi Normal guibg=NONE ctermbg=NONE
:hi NonText ctermbg=none
:highlight SpecialKey ctermbg=none
:hi CursorLine ctermbg = NONE guibg = NONE
:highlight LineNr term=bold cterm=NONE ctermfg=DarkGrey ctermbg=NONE gui=NONE guifg=Black guibg=#589A5D

let &t_kD = "\x1b[3~"

runtime! macros/matchit.vim
" Colour files called htex like tex
au BufRead,BufNewFile *.htex set syntax=tex

" Change vim cursor
let &t_SI = "\<Esc>[6 q"
let &t_SR = "\<Esc>[4 q"
let &t_EI = "\<Esc>[2 q"

" -----------------------------------------------------------------------------
" Vim Buffers
" -----------------------------------------------------------------------------
" Turn on the buffer
let g:airline#extensions#tabline#enabled = 1
" Only for filenames
let g:airline#extensions#tabline#fnamemod = ':t'

" -----------------------------------------------------------------------------
" Basic mappings
" -----------------------------------------------------------------------------

" Open Terminal
map <Leader>tm :let $VIM_DIR=expand('%:p:h')<CR>:terminal<CR>cd $VIM_DIR<CR>

" Open new buffer
nmap <leader>bn :enew<cr>

" Next Buffer
nmap <leader>bl :bnext<CR>

" Previous Buffer
nmap <leader>bh :bprevious<CR>

" Close Current Buffer
nmap <leader>bd :bp <BAR> bd #<CR>

" ls
nmap <leader>ls :ls<CR>

" Window configs
nmap <leader>wv <C-w>v
nmap <leader>ws <C-w>S
nmap <leader>wq <C-w>q
nmap <leader>wh <C-w>h
nmap <leader>wj <C-w>j
nmap <leader>wk <C-w>k
nmap <leader>wl <C-w>l

" Haskell Auto Indent
nmap <leader>hs :%!stylish-haskell<CR>

" Seamlessly treat visual lines as actual lines when moving around.
inoremap <Down> <C-o>gj
inoremap <Up> <C-o>gk
nmap <C-Left> <C-o>h
nmap <C-Right> <C-o>l
inoremap <C-Left> <C-o>h
inoremap <C-Right> <C-o>l

" Move test top while in insert mode
inoremap <C-S-t> <C-o>zt

" Fix Delete key
"inoremap [P <Del>

" Navigate around splits with a single key combo.
nnoremap <C-l> <C-w><C-l>
nnoremap <C-h> <C-w><C-h>
nnoremap <C-k> <C-w><C-k>
nnoremap <C-j> <C-w><C-j>

" Cycle through splits.
nnoremap <S-Tab> <C-w>w

" Press * to search for the term under the cursor or a visual selection and
" then press a key below to replace all instances of it in the current file.
nnoremap <Leader>r :%s///g<Left><Left>
nnoremap <Leader>rc :%s///gc<Left><Left><Left>

" The same as above but instead of acting on the whole file it will be
" restricted to the previously visually selected range. You can do that by
" pressing *, visually selecting the range you want it to apply to and then
" press a key below to replace all instances of it in the current selection.
xnoremap <Leader>r :s///g<Left><Left>
xnoremap <Leader>rc :s///gc<Left><Left><Left>

" Type a replacement term and press . to repeat the replacement again. Useful
" for replacing a few instances of the term (comparable to multiple cursors).
nnoremap <silent> s* :let @/='\<'.expand('<cword>').'\>'<CR>cgn
xnoremap <silent> s* "sy:let @/=@s<CR>cgn

" Clear search highlights.
map <Leader><Space> :let @/=''<CR>

" Format paragraph (selected or not) to 80 character lines.
nnoremap <Leader>g gqap
xnoremap <Leader>g gqa

" Prevent x from overriding what's in the clipboard.
noremap x "_x
noremap X "_x

" Prevent selecting and pasting from overwriting what you originally copied.
xnoremap p pgvy

" Keep cursor at the bottom of the visual selection after you yank it.
vmap y ygv<Esc>

" Eliminate issues where you accidentally hold shift for too long with :w.
command! W write

" Toggle spell check.
map <F5> :setlocal spell!<CR>

" Toggle quickfix window.
function! QuickFix_toggle()
    for i in range(1, winnr('$'))
        let bnum = winbufnr(i)
        if getbufvar(bnum, '&buftype') == 'quickfix'
            cclose
            return
        endif
    endfor

    copen
endfunction
nnoremap <silent> <Leader>c :call QuickFix_toggle()<CR>

" Convert the selected text's title case using the external tcc script.
"   Requires: https://github.com/nickjj/title-case-converter
vnoremap <Leader>tc c<C-r>=system('tcc', getreg('"'))[:-2]<CR>

nnoremap <Leader>ml :r !typora % &<CR>
map <F2> :NERDTreeToggle<CR>

" -----------------------------------------------------------------------------
" Basic autocommands
" -----------------------------------------------------------------------------

" Reduce delay when switching between modes.
augroup NoInsertKeycodes
  autocmd!
  autocmd InsertEnter * set ttimeoutlen=0
  autocmd InsertLeave * set ttimeoutlen=500
augroup END

" Auto-resize splits when Vim gets resized.
autocmd VimResized * wincmd

" Update a buffer's contents on focus if it changed outside of Vim.
au FocusGained,BufEnter * :checktime

" Unset paste on InsertLeave.
autocmd InsertLeave * silent! set nopaste

" Make sure all types of requirements.txt files get syntax highlighting.
autocmd BufNewFile,BufRead requirements*.txt set syntax=python

" ----------------------------------------------------------------------------
" Basic commands
" ----------------------------------------------------------------------------

" Add all TODO items to the quickfix list relative to where you opened Vim.
function! s:todo() abort
  let entries = []
  for cmd in ['git grep -niIw -e TODO -e FIXME 2> /dev/null',
            \ 'grep -rniIw -e TODO -e FIXME . 2> /dev/null']
    let lines = split(system(cmd), '\n')
    if v:shell_error != 0 | continue | endif
    for line in lines
      let [fname, lno, text] = matchlist(line, '^\([^:]*\):\([^:]*\):\(.*\)')[1:3]
      call add(entries, { 'filename': fname, 'lnum': lno, 'text': text })
    endfor
    break
  endfor

  if !empty(entries)
    call setqflist(entries)
    copen
  endif
endfunction

command! Todo call s:todo()

" Profile Vim by running this command once to start it and again to stop it.
function! s:profile(bang)
  if a:bang
    profile pause
    noautocmd qall
  else
    profile start /tmp/profile.log
    profile func *
    profile file *
  endif
endfunction

command! -bang Profile call s:profile(<bang>0)

" -----------------------------------------------------------------------------
" Pluginin settings, mappings and autocommands
" -----------------------------------------------------------------------------

let g:airline_powerline_fonts = 1

" .............................................................................
" scrooloose/nerdtree
" .............................................................................

let g:NERDTreeShowHidden=1
let g:NERDTreeAutoDeleteBuffer=1

" Open nerd tree at the current file or close nerd tree if pressed again.
nnoremap <silent> <expr> <Leader>n g:NERDTree.IsOpen() ? "\:NERDTreeClose<CR>" : bufexists(expand('%')) ? "\:NERDTreeFind<CR>" : "\:NERDTree<CR>"

" .............................................................................
" ntpeters/vim-better-whitespace
" .............................................................................

let g:strip_whitespace_confirm=0
let g:strip_whitespace_on_save=1

" .............................................................................
" Konfekt/FastFold
" .............................................................................

let g:fastfold_savehook=0
let g:fastfold_fold_command_suffixes=[]

" .............................................................................
" junegunn/limelight.vim
" .............................................................................

let g:limelight_conceal_ctermfg=244

" .............................................................................
" lervag/vimtex
" .............................................................................

"let g:tex_flavor = 'latex'
"let g:vimtex_view_general_viewer = 'qpdfview'
"let g:vimtex_view_general_options = '--unique file:@pdf\#src:@line@tex'
"let g:vimtex_view_general_options_latexmk = '--unique'

" .............................................................................
" plasticboy/vim-markdown:w
" .............................................................................

let g:vim_markdown_folding_disabled = 1
let g:tex_conceal = ""
let g:vim_markdown_math = 1

"==============================================================================
" SirVer/ultisnips
"==============================================================================

let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<tab>"
let g:UltiSnipsJumpBackwardTrigger="<s-tab>"
let g:UltiSnipsUsePythonVersion = 3

let g:UltiSnipsSnippetDirectories=['/home/almagest/.vim/localSnips']

let g:python_highlight_all = 1

let g:haskell_enable_quantification = 1   " to enable highlighting of `forall`
let g:haskell_enable_recursivedo = 1      " to enable highlighting of `mdo` and `rec`
let g:haskell_enable_arrowsyntax = 1      " to enable highlighting of `proc`
let g:haskell_enable_pattern_synonyms = 1 " to enable highlighting of `pattern`
let g:haskell_enable_typeroles = 1        " to enable highlighting of type roles
let g:haskell_enable_static_pointers = 1  " to enable highlighting of `static`
let g:haskell_backpack = 1                " to enable highlighting of backpack keywords
let g:haskell_indent_if = 3
let g:haskell_indent_case = 2
let g:haskell_indent_where = 6
let g:haskell_indent_before_where = 2
let g:haskell_indent_after_bare_where = 2
let g:haskell_indent_do = 3
let g:haskell_indent_guard = 2

"==============================================================================
" ycm-core/YouCompleteMe
"==============================================================================

noremap <leader>gt :YcmCompleter GetType<CR>
let g:ycm_key_list_select_completion = ['<C-n>', '<Down>']
let g:ycm_key_list_previous_completion = ['<C-p>', '<Up>']
let g:SuperTabDefaultCompletionType = '<C-n>'
let g:ycm_error_symbol = '??'
let g:ycm_warning_symbol = '>>'
let g:ycm_filetype_blacklist = {
  \ 'plaintex': 1,
  \ 'tex': 1
  \}

"==============================================================================
" jiangmiao/auto-pairs
"==============================================================================

let g:AutoPairsShortcutFastWrap = '<C-]>'

"==============================================================================
" rust-lang/rust.vim
"==============================================================================

let g:rustfmt_autosave = 1
