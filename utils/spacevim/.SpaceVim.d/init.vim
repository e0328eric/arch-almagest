let g:spacevim_force_global_config = 0
let g:spacevim_snippet_engine = 'ultisnips'
" disable neosnippet to avoid conflict with UltiSnips
let g:spacevim_disabled_plugins=[
\ ['Shougo/neosnippet.vim'],
\ ]
" custom plugin
let g:spacevim_custom_plugins=[
\ ['SirVer/ultisnips/', {'on_ft' : 'autocomplete'}],
\ ]
let g:neosnippet#snippets_directory = '~/.SpaceVim.d/snippets'
