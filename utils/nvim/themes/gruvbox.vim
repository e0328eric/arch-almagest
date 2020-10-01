if (has("termguicolors"))
  set termguicolors
  hi LineNr ctermbg=NONE guibg=NONE
  hi Crates ctermfg=green ctermbg=NONE cterm=NONE
  " or link it to another highlight group
  hi link Crates WarningMsg
endif

let g:gruvbox_contrast_dark = 'soft'
syntax enable
colorscheme gruvbox
hi Normal ctermbg=NONE guibg=NONE
hi NonText ctermbg=NONE guibg=NONE
