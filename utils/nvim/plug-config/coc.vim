let g:coc_snippet_next = '<C-j>'
let g:coc_snippet_prev = '<C-k>'

nnoremap <F5> :call LanguageClient_contextMenu()<CR>
autocmd FileType plaintex let b:coc_pairs_disabled = ['<', '{', '(', '[']
autocmd FileType plaintex let b:coc_pairs = [["$", "$"]]
autocmd FileType tex let b:coc_pairs_disabled = ['<', '{', '(', '[']
autocmd FileType tex let b:coc_pairs = [["$", "$"]]
